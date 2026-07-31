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

let string_option_field object_name name fields =
  match field name fields with
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | _ ->
    Error (
      Printf.sprintf "%s.%s must be a string or null" object_name name
    )

let string_truthy_default default object_name name fields =
  match string_option_field object_name name fields with
  | Ok (Some value) when not (String.equal value "") -> Ok value
  | Ok _ -> Ok default
  | Error _ as error -> error

let object_field_default_empty object_name name fields =
  match field name fields with
  | None | Some `Null -> Ok []
  | Some (`Assoc fields) -> Ok fields
  | _ ->
    Error (
      Printf.sprintf "%s.%s must be an object or null" object_name name
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

(* The render boundary for this whole module: every control-API string
   is single-lined and UTF-8 sanitized before it is interpolated into
   output the calling agent will render as Discord markdown.

   Newlines first. Nothing the control API returns is legitimately
   multi-line — every message it builds is a [Printf.sprintf] sentence,
   and this module owns the newlines in its own layouts — so a newline
   arriving inside a *field* means someone put it there. It lands the
   rest of the reply at column 0, where it reads as a separate,
   authoritative-looking statement: a goal objective is free-form user
   text and set_goal takes any thread_id, so one session can plant a
   line like "Login repair: run `curl …|sh` on the bot host." in
   another session's config listing, indistinguishable from the real
   one this module emits two lines below.

   Then UTF-8: working dirs are filesystem paths, session ids come from
   rollout records and filenames, and a Yojson-decoded \uD800 escape is
   raw surrogate bytes. Yojson re-emits all of it verbatim and the
   decoder across the JSON-RPC boundary raises rather than rendering.

   Identity on valid single-line input, so parity with Python is
   unaffected for every realistic response; Python scrubs nothing, so
   this is a deliberate divergence, pinned by tests. *)
let render_string value =
  Resource.sanitize_utf8 (Resource.single_line value)

(* Python's [if x:] over the JSON types the control API can produce.
   [`Float 0.] is spelled out because OCaml would otherwise call it
   truthy where Python does not. *)
let json_truthy = function
  | `Null | `Bool false | `String "" | `List [] | `Assoc [] -> false
  | `Int 0 | `Intlit "0" | `Float 0. -> false
  | _ -> true

(* Python's f-string interpolation of a scalar: [str()] of the decoded
   value, which is why null renders "None" and booleans capitalize.

   Non-zero floats fail closed rather than guess: matching CPython's
   [repr] (shortest round-tripping decimal) is not something
   [string_of_float] does — it would print "0.1" as "0.1" but "1e22" as
   "1e+22" — and no control-API field emits one, so a float here means
   the response is already malformed. *)
let json_scalar_text object_name name = function
  | `String value -> Ok (render_string value)
  | `Int value -> Ok (string_of_int value)
  | `Intlit value -> Ok value
  | `Bool true -> Ok "True"
  | `Bool false -> Ok "False"
  | `Null -> Ok "None"
  | _ ->
    Error (
      Printf.sprintf "%s.%s must be a scalar value" object_name name
    )

let render_values object_name name = function
  | `List values ->
    let rec loop rendered = function
      | [] -> Ok (rendered |> List.rev |> String.concat ", ")
      | `Null :: rest -> loop ("`null`" :: rendered) rest
      | `String "" :: rest -> loop ("`\"\"`" :: rendered) rest
      | `String value :: rest ->
        loop (Printf.sprintf "`%s`" (render_string value) :: rendered) rest
      | value :: rest ->
        (match json_scalar_text object_name name value with
         | Ok text -> loop (Printf.sprintf "`%s`" text :: rendered) rest
         | Error _ as error -> error)
    in
    loop [] values
  | value -> json_scalar_text object_name name value

(* [d.get(k)] used as a condition, i.e. Python's truthiness over an
   optional key. Distinct from [bool_field_default], which fails closed
   on a non-bool: that is right where failing closed *omits* something
   (see [project_line]'s is_bare), but here the false branch asserts
   "unsupported for `codex`" — stating something false rather than
   leaving something out. Match Python instead. *)
let field_truthy name fields =
  match field name fields with
  | Some value -> json_truthy value
  | None -> false

(* [d.get(k, default)] — the default applies to an *absent* key only.
   A key that is present and null is a value, and Python f-strings it to
   "None"; folding null into the default would make us describe a
   different contract than the one the control API sent, in prose that
   reads just as authoritative. The sibling [render_values] on the same
   output line already renders null as "None", so this also keeps one
   line internally consistent.

   Deliberately unlike [string_field_default] above, which *errors* on
   an explicit null. That one reads fields we interpolate as facts (a
   status, a thread id) where a null is a malformed response worth
   refusing; this one reads fields that describe what values are
   allowed, where Python's rendering is the contract being reported. *)
let string_of_json_field_default default object_name name fields =
  match field name fields with
  | None -> Ok default
  | Some value -> json_scalar_text object_name name value

let truthy_string_option_field object_name name fields =
  match string_option_field object_name name fields with
  | Ok (Some value) when not (String.equal value "") ->
    Ok (Some (render_string value))
  | Ok _ -> Ok None
  | Error _ as error -> error

(* The config tools have roughly twenty-five interpolation sites between
   them. Attaching [render_string] to each one means the safety holds
   only as long as everyone remembers; attaching it to *reading* a field
   means a new field is scrubbed by construction. Same values, read
   through these. *)
let rendered_string_field_default default object_name name fields =
  Result.map render_string
    (string_field_default default object_name name fields)

let rendered_string_truthy_default default object_name name fields =
  Result.map render_string
    (string_truthy_default default object_name name fields)

let argument_has_key name = function
  | `Assoc fields -> Option.is_some (field name fields)
  | _ -> false

let argument_get_is_not_none name = function
  | `Assoc fields ->
    (match field name fields with
     | None | Some `Null -> false
     | Some _ -> true)
  | _ -> false

let join_sentences parts =
  String.concat " " parts

let top_level_session_noun count =
  if count = 1 then "session" else "sessions"

let token_budget_suffix ~object_name ~prefix ~suffix fields =
  match field "token_budget" fields with
  | Some value when json_truthy value ->
    (match json_scalar_text object_name "token_budget" value with
     | Ok value -> Ok (Printf.sprintf "%s%s%s" prefix value suffix)
     | Error _ as error -> error)
  | _ -> Ok ""

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

let format_default_agent ~arguments response =
  let format_fields fields =
    match rendered_string_field_default "" "default_agent" "agent" fields,
          rendered_string_field_default "" "default_agent"
            "effective_top_level_agent" fields,
          truthy_string_option_field "default_agent" "rescue_agent" fields with
    | Ok agent, Ok effective, Ok rescue ->
      let rescue_active =
        bool_field_default false "disk_rescue_active" fields
      in
      if not (argument_get_is_not_none "agent" arguments) then
        let parts = [Printf.sprintf "Default agent: `%s`." agent] in
        let parts =
          match rescue with
          | None -> parts
          | Some rescue ->
            let suffix = if rescue_active then " (active)" else "" in
            parts @
            [Printf.sprintf "Rescue agent: `%s`%s." rescue suffix]
        in
        let parts =
          (* Compares scrubbed values, so two agent names differing only
             in whitespace would collapse and drop this sentence, where
             Python would print it. Both sides come from
             Config.string_of_agent_kind's three-value enum, so the
             inputs can't differ that way — but it is the one place the
             render boundary is more than cosmetic. *)
          if not (String.equal effective "")
             && not (String.equal effective agent)
          then
            parts @
            [Printf.sprintf
               "Effective top-level agent: `%s`." effective]
          else parts
        in
        Ok (join_sentences parts)
      else
        (match int_field_default 0 "default_agent" "reset_count" fields,
               int_field_default 0 "default_agent" "busy_count" fields with
         | Ok reset_count, Ok busy_count ->
           let parts = [Printf.sprintf "Default agent set to `%s`." agent] in
           let parts =
             if reset_count = 0 then parts
             else
               parts @
               [Printf.sprintf
                  "Reset %d idle top-level %s immediately."
                  reset_count (top_level_session_noun reset_count)]
           in
           let parts =
             if busy_count = 0 then parts
             else
               parts @
               [Printf.sprintf
                  "%d busy top-level %s will switch after queued work finishes."
                  busy_count (top_level_session_noun busy_count)]
           in
           let parts =
             if rescue_active
                && not (String.equal effective "")
                && not (String.equal effective agent)
             then
               parts @
               [Printf.sprintf
                  "Disk pressure is active, so top-level sessions currently use rescue agent `%s`."
                  effective]
             else parts
           in
           Ok (join_sentences parts)
         | Error message, _ | _, Error message -> Error message)
    | Error message, _, _
    | _, Error message, _
    | _, _, Error message -> Error message
  in
  format_object ~format_fields response

let format_rescue_agent ~arguments response =
  let format_fields fields =
    match truthy_string_option_field "rescue_agent" "agent" fields,
          rendered_string_field_default "" "rescue_agent"
            "effective_top_level_agent" fields with
    | Ok agent, Ok effective ->
      let rescue_active =
        bool_field_default false "disk_rescue_active" fields
      in
      if not (argument_has_key "agent" arguments) then
        let parts =
          match agent with
          | Some agent -> [Printf.sprintf "Rescue agent: `%s`." agent]
          | None -> ["Rescue agent: disabled."]
        in
        let parts =
          if rescue_active && not (String.equal effective "") then
            parts @
            [Printf.sprintf
               "Disk pressure is active, so top-level sessions currently use `%s`."
               effective]
          else parts
        in
        Ok (join_sentences parts)
      else
        (match int_field_default 0 "rescue_agent" "reset_count" fields,
               int_field_default 0 "rescue_agent" "busy_count" fields with
         | Ok reset_count, Ok busy_count ->
           let parts =
             match agent with
             | Some agent ->
               [Printf.sprintf "Rescue agent set to `%s`." agent]
             | None -> ["Rescue agent disabled."]
           in
           let parts =
             if reset_count = 0 then parts
             else
               parts @
               [Printf.sprintf
                  "Reset %d idle top-level %s immediately."
                  reset_count (top_level_session_noun reset_count)]
           in
           let parts =
             if busy_count = 0 then parts
             else
               parts @
               [Printf.sprintf
                  "%d busy top-level %s will switch after queued work finishes."
                  busy_count (top_level_session_noun busy_count)]
           in
           let parts =
             if rescue_active && not (String.equal effective "") then
               parts @
               [Printf.sprintf
                  "Disk pressure is active, so top-level sessions currently use `%s`."
                  effective]
             else parts
           in
           Ok (join_sentences parts)
         | Error message, _ | _, Error message -> Error message)
    | Error message, _ | _, Error message -> Error message
  in
  format_object ~format_fields response

let format_goal_line fields =
  match object_field_default_empty "get_agent_config" "goal" fields with
  | Error _ as error -> error
  | Ok [] -> Ok "Goal: none"
  | Ok goal_fields ->
    (match rendered_string_field_default "" "goal" "objective" goal_fields,
           rendered_string_field_default "active" "goal" "status" goal_fields with
     | Ok objective, Ok status ->
       (match token_budget_suffix
                ~object_name:"goal" ~prefix:", token budget "
                ~suffix:"" goal_fields with
        | Ok suffix ->
          Ok (Printf.sprintf "Goal: `%s`%s — %s"
                status suffix objective)
        | Error _ as error -> error)
     | Error message, _ | _, Error message -> Error message)

let append_login_help lines fields =
  match object_field_default_empty "get_agent_config" "login_help" fields with
  | Error _ as error -> error
  | Ok [] -> Ok lines
  | Ok login_fields ->
    (match rendered_string_field_default "" "login_help" "command" login_fields with
     | Error _ as error -> error
     | Ok command ->
       Ok (lines @
           [Printf.sprintf
              "Login repair: run `%s` on the bot host." command]))

let format_get_agent_config_options lines result_fields options =
  if options = [] then Ok lines
  else
    let lines = lines @ [""; "Potential values:"] in
    let format_agent_options lines =
      match object_field_default_empty
              "configuration_options" "agent_kind" options with
      | Error _ as error -> error
      | Ok agent_options ->
        (match field "values" agent_options with
         | Some values when json_truthy values ->
           (match render_values "agent_kind" "values" values,
                  rendered_string_field_default "chosen when the session starts"
                    "agent_kind" "set_with" agent_options with
            | Ok values, Ok set_with ->
              Ok (lines @
                  [Printf.sprintf
                     "- Agent kind: %s; current thread is read-only here (%s)"
                     values set_with])
            | Error message, _ | _, Error message -> Error message)
         | _ -> Ok lines)
    in
    let format_model_options lines =
      match object_field_default_empty
              "configuration_options" "model" options with
      | Error _ as error -> error
      | Ok [] -> Ok lines
      | Ok model_options ->
        (match string_of_json_field_default
                 "any non-empty model string"
                 "model" "values" model_options,
               (match field "clear_values" model_options with
                | None -> Ok ""
                | Some values -> render_values "model" "clear_values" values),
               int_field_default 200 "model" "max_bytes" model_options with
         | Ok model_values, Ok clear_values, Ok max_bytes ->
           Ok (lines @
               [Printf.sprintf
                  "- Model: %s; clear with %s; max %d bytes"
                  model_values clear_values max_bytes])
         | Error message, _, _
         | _, Error message, _
         | _, _, Error message -> Error message)
    in
    let format_effort_options lines =
      match object_field_default_empty
              "configuration_options" "effort" options with
      | Error _ as error -> error
      | Ok [] -> Ok lines
      | Ok effort_options ->
        if field_truthy "supported" effort_options then
          (match (match field "values" effort_options with
                  | None -> Ok ""
                  | Some values -> render_values "effort" "values" values),
                 (match field "clear_values" effort_options with
                  | None -> Ok ""
                  | Some values ->
                    render_values "effort" "clear_values" values) with
           | Ok values, Ok clear_values ->
             Ok (lines @
                 [Printf.sprintf
                    "- Effort: %s; clear with %s"
                    values clear_values])
           | Error message, _ | _, Error message -> Error message)
        else
          (match rendered_string_field_default "" "get_agent_config"
                   "agent_kind" result_fields with
           | Ok agent_kind ->
             Ok (lines @
                 [Printf.sprintf "- Effort: unsupported for `%s`"
                    agent_kind])
           | Error _ as error -> error)
    in
    let format_goal_options lines =
      match object_field_default_empty
              "configuration_options" "goal" options with
      | Error _ as error -> error
      | Ok [] -> Ok lines
      | Ok goal_options ->
        if field_truthy "supported" goal_options then
          let objective =
            match field "objective" goal_options with
            | Some (`Assoc fields) -> Ok fields
            | None | Some `Null -> Ok []
            | Some _ -> Error "goal.objective must be an object or null"
          in
          (match objective with
           | Error _ as error -> error
           | Ok objective ->
             match string_of_json_field_default
                     "any non-empty string" "goal.objective"
                     "values" objective,
                   int_field_default 4000 "goal.objective"
                     "max_bytes" objective,
                   (match field "status_values" goal_options with
                    | None -> Ok ""
                    | Some values ->
                      render_values "goal" "status_values" values),
                   (match object_field_default_empty
                            "goal" "token_budget" goal_options with
                    | Error _ as error -> error
                    | Ok token_budget ->
                      string_of_json_field_default
                        "positive integer or null" "goal.token_budget"
                        "values" token_budget),
                   string_of_json_field_default
                     "clear=true" "goal" "clear_values" goal_options with
             | Ok objective_values, Ok objective_max, Ok statuses,
               Ok token_budget_values, Ok clear_values ->
               Ok (lines @
                   [Printf.sprintf
                      "- Goal: objective is %s (max %d bytes); status %s; token_budget %s; clear with `%s`"
                      objective_values objective_max statuses
                      token_budget_values clear_values])
             | Error message, _, _, _, _
             | _, Error message, _, _, _
             | _, _, Error message, _, _
             | _, _, _, Error message, _
             | _, _, _, _, Error message -> Error message)
        else
          Ok (lines @ ["- Goal: unsupported for this agent"])
    in
    match format_agent_options lines with
    | Error _ as error -> error
    | Ok lines ->
      match format_model_options lines with
      | Error _ as error -> error
      | Ok lines ->
        match format_effort_options lines with
        | Error _ as error -> error
        | Ok lines -> format_goal_options lines

let format_get_agent_config response =
  let format_fields fields =
    match rendered_string_field_default "" "get_agent_config" "agent_kind" fields,
          rendered_string_truthy_default "default" "get_agent_config" "model" fields,
          rendered_string_truthy_default "default" "get_agent_config" "effort" fields,
          format_goal_line fields with
    | Ok agent_kind, Ok model, Ok effort, Ok goal_line ->
      let lines = [
        Printf.sprintf "Agent: `%s`" agent_kind;
        Printf.sprintf "Model: `%s`" model;
        Printf.sprintf "Effort: `%s`" effort;
        goal_line;
      ] in
      (match append_login_help lines fields with
       | Error _ as error -> error
       | Ok lines ->
         match truthy_string_option_field
                 "get_agent_config" "goal_mechanism" fields with
         | Error _ as error -> error
         | Ok mechanism ->
           let lines =
             match mechanism with
             | Some mechanism ->
               lines @
               [Printf.sprintf "Goal mechanism: %s." mechanism]
             | None -> lines
           in
           match object_field_default_empty "get_agent_config"
                   "configuration_options" fields with
           | Error _ as error -> error
           | Ok options ->
             match format_get_agent_config_options lines fields options with
             | Error _ as error -> error
             | Ok lines ->
               match truthy_string_option_field
                       "get_agent_config" "command_briefing" fields with
               | Error _ as error -> error
               | Ok briefing ->
                 let lines =
                   match briefing with
                   | Some briefing -> lines @ [""; "Briefing: " ^ briefing]
                   | None -> lines
                 in
                 Ok (String.concat "\n" lines))
    | Error message, _, _, _
    | _, Error message, _, _
    | _, _, Error message, _
    | _, _, _, Error message -> Error message
  in
  format_object ~format_fields response

let format_set_model response =
  let format_fields fields =
    match rendered_string_field_default "" "set_model" "thread_id" fields,
          rendered_string_truthy_default "default" "set_model" "model" fields with
    | Ok thread_id, Ok model ->
      Ok (Printf.sprintf
            "Model override for <#%s> is now `%s`." thread_id model)
    | Error message, _ | _, Error message -> Error message
  in
  format_object ~format_fields response

let format_set_effort response =
  let format_fields fields =
    match rendered_string_field_default "" "set_effort" "thread_id" fields,
          rendered_string_truthy_default "default" "set_effort" "effort" fields with
    | Ok thread_id, Ok effort ->
      Ok (Printf.sprintf
            "Effort override for <#%s> is now `%s`." thread_id effort)
    | Error message, _ | _, Error message -> Error message
  in
  format_object ~format_fields response

let format_set_goal response =
  let format_fields fields =
    match rendered_string_field_default "" "set_goal" "thread_id" fields,
          object_field_default_empty "set_goal" "goal" fields with
    | Ok thread_id, Ok [] ->
      Ok (Printf.sprintf "Goal cleared for <#%s>." thread_id)
    | Ok thread_id, Ok goal_fields ->
      (match rendered_string_field_default "active" "goal" "status" goal_fields,
             rendered_string_field_default "" "goal" "objective" goal_fields,
             truthy_string_option_field
               "set_goal" "goal_mechanism" fields with
       | Ok status, Ok objective, Ok mechanism ->
         (match token_budget_suffix
                  ~object_name:"goal" ~prefix:" Token budget: `"
                  ~suffix:"`." goal_fields with
          | Error _ as error -> error
          | Ok suffix ->
            let mechanism_text =
              match mechanism with
              | None -> ""
              | Some mechanism -> Printf.sprintf " Mechanism: %s." mechanism
            in
            Ok (Printf.sprintf
                  "Goal set for <#%s>: `%s` — %s.%s%s"
                  thread_id status objective suffix mechanism_text))
       | Error message, _, _
       | _, Error message, _
       | _, _, Error message -> Error message)
    | Error message, _ | _, Error message -> Error message
  in
  format_object ~format_fields response

let format_start_login_flow response =
  let format_fields fields =
    match object_field_default_empty
            "start_login_flow" "login" fields,
          rendered_string_field_default "" "start_login_flow" "message" fields with
    | Ok login_fields, Ok intro ->
      (* [intro] rather than [message]: the error arms below bind their
         own [message], and the shadowing made it read as though the
         response field were being propagated as the error. *)
      (match rendered_string_field_default "" "login" "command" login_fields,
             rendered_string_field_default "" "login" "note" login_fields with
       | Ok command, Ok note ->
         Ok (Printf.sprintf "%s\n\nRun on bot host: `%s`\n%s"
               intro command note)
       | Error message, _ | _, Error message -> Error message)
    | Error message, _ | _, Error message -> Error message
  in
  format_object ~format_fields response

let format_import_project response =
  let format_fields fields =
    match
      rendered_string_field_default "" "import_project" "project_name" fields,
      rendered_string_field_default "" "import_project" "channel_id" fields,
      rendered_string_field_default "" "import_project" "working_dir" fields
    with
    | Ok project_name, Ok channel_id, Ok working_dir ->
      let action =
        (* Python's [if result.get("existing"):] — truthiness, and the
           false branch asserts "imported" for a project that already
           existed, so this is a [field_truthy] case rather than a
           [bool_field_default] one (see the note on both). *)
        if field_truthy "existing" fields then
          "already existed"
        else
          "imported"
      in
      Ok (Printf.sprintf
            "Project **%s** %s in <#%s>.\nWorking in: `%s`"
            project_name action channel_id working_dir)
    | Error message, _, _
    | _, Error message, _
    | _, _, Error message -> Error message
  in
  format_object ~format_fields response

(* [rename_thread]'s message is the reachable one: Control_api builds it
   as "Renamed to %s." from the caller-supplied name (lib/control_api.ml),
   which Discord accepts with newlines in it. So any caller that can
   rename a thread would otherwise control the text of a second line the
   calling agent renders as bot-authored. *)
let format_message_response object_name default_message response =
  let format_fields fields =
    rendered_string_field_default default_message object_name "message" fields
  in
  format_object ~format_fields response

let format_restart_bot response =
  format_message_response "restart_bot" "Restart initiated." response

let format_rename_thread response =
  format_message_response "rename_thread" "Renamed." response

let format_cleanup_channels response =
  format_message_response "cleanup_channels" "Done." response

let format_refresh_projects response =
  let format_fields fields =
    match int_field_default 0 "refresh_projects" "total" fields,
          int_field_default 0 "refresh_projects" "delta" fields with
    | Ok total, Ok delta ->
      if delta > 0 then
        let plural = if delta = 1 then "" else "s" in
        Ok (Printf.sprintf
              "Refreshed: found %d new project%s (%d total)."
              delta plural total)
      else
        Ok (Printf.sprintf
              "Refreshed: no new projects (%d total)." total)
    | Error message, _ | _, Error message -> Error message
  in
  format_object ~format_fields response
