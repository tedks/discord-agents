(** Text formatters for MCP tool results. *)

let field name fields =
  List.assoc_opt name fields

let string_field object_name name fields =
  match field name fields with
  | Some (`String value) -> Ok value
  | _ ->
    Error (
      Printf.sprintf "%s.%s must be a string" object_name name
    )

let int_type_error object_name name =
  Error (
    Printf.sprintf "%s.%s must be an integer" object_name name
  )

let int_range_error object_name name =
  Error (
    Printf.sprintf "%s.%s must be an in-range integer" object_name name
  )

let int_field object_name name fields =
  match field name fields with
  | Some (`Int value) -> Ok value
  | Some (`Intlit value) ->
    (match int_of_string_opt value with
     | Some value -> Ok value
     | None -> int_range_error object_name name)
  | _ -> int_type_error object_name name

let int_field_default default object_name name fields =
  match field name fields with
  | None -> Ok default
  | Some _ -> int_field object_name name fields

let string_field_default default object_name name fields =
  match field name fields with
  | None -> Ok default
  | Some (`String value) -> Ok value
  | _ ->
    Error (
      Printf.sprintf "%s.%s must be a string" object_name name
    )

let bool_field_default default name fields =
  match field name fields with
  | Some (`Bool value) -> value
  | _ -> default

let list_field ?(null_is_empty=false) name fields =
  match field name fields with
  | None -> Ok []
  | Some `Null when null_is_empty -> Ok []
  | Some (`List values) -> Ok values
  | Some _ ->
    Error (
      Printf.sprintf "Control API %s field must be an array" name
    )

(* The blank line between body and footer belongs to the joiner, not to
   the footer string: a caller that passed a footer without its own
   leading "\n\n" would otherwise silently glue it onto the last
   bullet. [footer] is the footer text alone. *)
let finish_lines ?footer ~empty_message lines =
  match lines with
  | [] -> Ok empty_message
  | lines ->
    let body = lines |> List.rev |> String.concat "\n" in
    (match footer with
     | None -> Ok body
     | Some footer -> Ok (body ^ "\n\n" ^ footer))

let format_lines ?(null_list_is_empty=false) ?footer
    ~field_name ~empty_message ~line_of_item fields =
  match list_field ~null_is_empty:null_list_is_empty field_name fields with
  | Error _ as error -> error
  | Ok items ->
    let rec loop index lines = function
      | [] -> finish_lines ?footer ~empty_message lines
      | item :: rest ->
        match line_of_item index item with
        | Error _ as error -> error
        | Ok line -> loop (index + 1) (line :: lines) rest
    in
    loop 0 [] items

(* Every tool result comes through here: an [error] string short-circuits
   to Error (Python returns [result["error"]] the same way), anything
   else is handed to the tool's own renderer. *)
let format_object ~format_fields = function
  | `Assoc fields ->
    (match field "error" fields with
     | Some (`String message) -> Error message
     | Some _ -> Error "Control API error field must be a string"
     | None -> format_fields fields)
  | _ -> Error "Control API response must be an object"

let format_response ?(null_list_is_empty=false) ?footer
    ~field_name ~empty_message ~line_of_item response =
  format_object response
    ~format_fields:(fun fields ->
      format_lines ~null_list_is_empty
        ?footer ~field_name ~empty_message ~line_of_item fields)

let project_line index = function
  | `Assoc fields ->
    (match string_field "project" "name" fields,
           string_field "project" "path" fields with
     | Ok name, Ok path ->
       let bare_suffix =
         (* Control_api emits a bool here; malformed non-bools fail closed
            instead of inheriting Python's broad truthiness. *)
         if bool_field_default false "is_bare" fields then " [bare]" else ""
       in
       Ok (Printf.sprintf "%d. **%s** — `%s`%s"
             (index + 1) name path bare_suffix)
     | Error message, _ | _, Error message -> Error message)
  | _ -> Error "project entry must be an object"

let format_list_projects response =
  format_response
    ~field_name:"projects"
    ~empty_message:"No projects found."
    ~line_of_item:project_line
    response

(* [Control_api.handle_list_sessions] applies [Resource.single_line] to
   [project_name] before this formatter interpolates it into a Discord
   markdown bullet. *)
let session_line = function
  | `Assoc fields ->
    (match string_field "session" "project_name" fields,
           string_field "session" "agent_kind" fields,
           int_field "session" "message_count" fields,
           string_field "session" "thread_id" fields with
     | Ok project_name, Ok agent_kind, Ok message_count, Ok thread_id ->
       Ok (Printf.sprintf "- **%s** / %s — %d messages (thread: <#%s>)"
             project_name agent_kind message_count thread_id)
     | Error message, _, _, _
     | _, Error message, _, _
     | _, _, Error message, _
     | _, _, _, Error message -> Error message)
  | _ -> Error "session entry must be an object"

let format_list_sessions response =
  format_response
    (* Python treats explicit null sessions as empty because list_sessions
       guards with [if not sessions]; list_projects intentionally does not. *)
    ~null_list_is_empty:true
    ~field_name:"sessions"
    ~empty_message:"No active sessions."
    ~line_of_item:(fun _index -> session_line)
    response

(* Negative ages don't occur (Control_api derives them from mtime), and
   the [< 60] guard keeps them out of the division if they ever do —
   which matters, because OCaml's [/] truncates toward zero while
   Python's [//] floors: [-70] renders "-70m ago" through the guard on
   both sides, but "-1h ago" here against "-2h ago" in Python if the
   guard is ever loosened. *)
let age_minutes_text age_minutes =
  if age_minutes < 60 then
    Printf.sprintf "%dm ago" age_minutes
  else
    Printf.sprintf "%dh ago" (age_minutes / 60)

(* Mirrors Python's [s.get("working_dir", "") or "(unknown project)"]:
   absent, null and empty all fall back. A non-string fails closed
   instead of inheriting Python's [str()] rendering — the same call the
   [project_line] [is_bare] handling makes above. *)
let recent_working_dir fields =
  match field "working_dir" fields with
  | None | Some `Null -> Ok "(unknown project)"
  | Some (`String "") -> Ok "(unknown project)"
  | Some (`String value) -> Ok value
  | Some _ -> Error "recent_session.working_dir must be a string"

(* One line shape for all three agents: Claude's listing omits the
   working_dir segment, Codex's and Gemini's carry it. Their footers and
   empty messages are the only other difference.

   Null policy inside a session object is deliberately split, because
   the fields are not alike: [working_dir] null is reachable and
   meaningful (Python's [or] makes it "(unknown project)", so we match),
   while a null [session_id_short], [summary] or [age_minutes] is
   malformed — Python interpolates the literal "None" for the first two
   and raises TypeError on the third ([None < 60]), where we return a
   field-specific error for all three. Each case is pinned by a test.

   Every interpolated string field is forced single-line first. A
   literal newline in any of them lands the rest of the entry at column
   0, where Discord parses it as a sibling bullet — a forged entry the
   calling agent cannot tell from a real one and may feed back to
   [resume_session]. [summary] already arrives normalized from the
   discoverers and [session_id_short] is usually an 8-char hex prefix,
   but neither is guaranteed: [Resource.short_id] is a [String.sub] that
   validates nothing, and all three fields have the same provenance —
   Codex reads [session_id] and [cwd] out of the same rollout record,
   Claude takes its id from a filename. So scrub all three rather than
   rest the invariant on the shape of upstream data.
   [Bot.format_session_listing] (lib/bot.ml:1512) scrubs the working dir
   and summary of the Discord-side listing for the same reason, but not
   its session id — issue #103 tracks closing that half. Python does not
   scrub at all, so this is a deliberate divergence, visible only for
   pathological input. *)
let recent_session_line ~with_working_dir = function
  | `Assoc fields ->
    let working_dir =
      (* Claude's listing never renders working_dir, so it must not
         validate it either: failing closed on an unrendered field would
         reject a listing Python renders fine. *)
      if with_working_dir then recent_working_dir fields else Ok ""
    in
    (match string_field "recent_session" "session_id_short" fields,
           int_field_default 0 "recent_session" "age_minutes" fields,
           working_dir,
           string_field_default "(no summary)"
             "recent_session" "summary" fields with
     | Ok session_id_short, Ok age_minutes, Ok working_dir, Ok summary ->
       let session_id_short = Resource.single_line session_id_short in
       let age = age_minutes_text age_minutes in
       let summary = Resource.single_line summary in
       Ok (
         if with_working_dir then
           Printf.sprintf "- `%s` %s — %s — %s"
             session_id_short age (Resource.single_line working_dir) summary
         else
           Printf.sprintf "- `%s` %s — %s" session_id_short age summary
       )
     | Error message, _, _, _
     | _, Error message, _, _
     | _, _, Error message, _
     | _, _, _, Error message -> Error message)
  | _ -> Error "recent_session entry must be an object"

let format_recent_sessions ~empty_message ~footer ~with_working_dir response =
  format_response
    ~null_list_is_empty:true
    ~footer
    ~field_name:"sessions"
    ~empty_message
    ~line_of_item:(fun _index item ->
      recent_session_line ~with_working_dir item)
    response

let format_list_claude_sessions response =
  format_recent_sessions
    ~empty_message:"No recent Claude sessions found."
    ~footer:"Use resume_session with a session ID prefix to attach."
    ~with_working_dir:false
    response

let format_list_codex_sessions response =
  format_recent_sessions
    ~empty_message:"No recent Codex sessions found."
    ~footer:"Use resume_session with kind=codex to attach."
    ~with_working_dir:true
    response

let format_list_gemini_sessions response =
  format_recent_sessions
    ~empty_message:"No recent Gemini sessions found."
    ~footer:"Use resume_session with kind=gemini to attach."
    ~with_working_dir:true
    response

(* [Config.string_of_agent_kind] is the only real source of [agent_kind],
   so ASCII case mapping is enough — but Python's [str.capitalize()] is
   Unicode-aware, so a non-ASCII kind would diverge. The [lowercase]
   pass is not redundant: it reproduces [capitalize()]'s down-casing of
   the tail, which turns "CODEX" into "Codex", not "CODEX". *)
let python_capitalize_ascii value =
  value
  |> String.lowercase_ascii
  |> String.capitalize_ascii

(* The lifecycle tools render into Discord markdown like everything else
   here, so they inherit [recent_session_line]'s rule: single-line every
   control-API string before interpolating it. The provenance is the
   same untrusted set — Control_api builds start_session's project_name
   and working_dir from project config and worktree paths, resume_session
   hands back the full on-disk session id, and stop_session's message
   embeds a project name (lib/control_api.ml). A newline in any of them
   puts the rest of the reply at column 0 in the calling agent's render,
   where it reads as a separate statement about a session that doesn't
   exist. Python scrubs none of this; deliberate divergence, pinned by
   tests.

   [sanitize_utf8] for the same reason applied field-wide rather than to
   the session id alone: working dirs are real filesystem paths and can
   carry non-UTF-8 bytes, Yojson re-emits those verbatim, and the
   decoder on the other side of the JSON-RPC boundary raises rather than
   rendering. Identity on valid input, so parity is unaffected. *)
let render_string value =
  Resource.sanitize_utf8 (Resource.single_line value)

let format_start_session response =
  let format_fields fields =
    match string_field_default "" "start_session" "thread_id" fields,
          string_field_default "" "start_session" "working_dir" fields,
          string_field_default "" "start_session" "project_name" fields with
    | Ok thread_id, Ok working_dir, Ok project_name ->
      Ok (Printf.sprintf
            "Started session for **%s** in <#%s>.\nWorking in: `%s`"
            (render_string project_name) (render_string thread_id)
            (render_string working_dir))
    | Error message, _, _
    | _, Error message, _
    | _, _, Error message -> Error message
  in
  format_object ~format_fields response

let format_resume_session response =
  let format_fields fields =
    match string_field_default "" "resume_session" "thread_id" fields,
          string_field_default "" "resume_session" "session_id" fields,
          string_field_default "" "resume_session" "agent_kind" fields with
    | Ok thread_id, Ok session_id, Ok agent_kind ->
      let sid_short =
        (* Python slices the decoded str, so its [:8] is 8 codepoints.
           A byte-counting [String.sub] would cut a multibyte id in half
           and emit a half-encoded character into the JSON-RPC response,
           which a strict client fails to decode. Ids arrive from a
           rollout record (Codex) or a filename (Claude), so
           [render_string]'s sanitize pass matters here even more than
           elsewhere. *)
        Resource.utf8_prefix ~max_chars:8 (render_string session_id)
      in
      let kind_label =
        if String.equal agent_kind "" then ""
        else Printf.sprintf "%s " (python_capitalize_ascii
                                     (render_string agent_kind))
      in
      Ok (Printf.sprintf "Resumed %ssession `%s` in <#%s>."
            kind_label sid_short (render_string thread_id))
    | Error message, _, _
    | _, Error message, _
    | _, _, Error message -> Error message
  in
  format_object ~format_fields response

let format_send_message response =
  let format_fields fields =
    match string_field_default "" "send_message" "thread_id" fields,
          int_field_default 0 "send_message" "remaining_hops" fields,
          string_field_default "sent" "send_message" "state" fields with
    | Ok thread_id, Ok remaining_hops, Ok "posted_not_routed" ->
      Ok (Printf.sprintf
            "Posted message to <#%s>, but the target session disappeared before routing. remaining_hops=%d."
            (render_string thread_id) remaining_hops)
    | Ok thread_id, Ok remaining_hops, Ok _ ->
      Ok (Printf.sprintf "Sent message to <#%s>. remaining_hops=%d."
            (render_string thread_id) remaining_hops)
    | Error message, _, _
    | _, Error message, _
    | _, _, Error message -> Error message
  in
  format_object ~format_fields response

let format_stop_session response =
  let format_fields fields =
    match
      string_field_default "Stop requested." "stop_session" "message" fields
    with
    | Ok message -> Ok (render_string message)
    | Error _ as error -> error
  in
  format_object ~format_fields response
