(** Pure command parsing — no I/O, no mutable state.

    Commands require a ! prefix to avoid hijacking natural language
    like "help me debug this" or "start here". *)

type t =
  | List_projects
  | List_sessions
  | List_claude_sessions
  | List_codex_sessions
  | List_gemini_sessions
  | Start_agent of { project : string; kind : Config.agent_kind option }
  (** [kind = None] means use the current effective top-level agent. *)
  | Resume_session of { session_id : string; kind : Config.agent_kind option }
  | Stop_session of { thread_id : string }
  | Interrupt_session
  | Cleanup_channels
  | Default_agent of Config.agent_kind option
  | Rescue_agent of Config.agent_kind option option
  | Session_agent of Config.agent_kind option
  | Restart
  | Refresh
  | Rename_thread of { thread_id : string option; name : string }
  | Status
  | Desktop
  | Mobile
  | Wrapping of int option
  | Lines of int option
  | Scroll of int option  (** None = continue current block, Some n = target block n *)
  | Help
  | Unknown of string

(** Does this message look like a bot command? Requires ! prefix. *)
let is_command content =
  let trimmed = String.trim content in
  String.length trimmed > 0 && trimmed.[0] = '!'

let parse content =
  let parts = String.split_on_char ' ' (String.trim content) in
  let parts = match parts with
    | w :: rest when String.length w > 0 && w.[0] = '!' ->
      String.lowercase_ascii (String.sub w 1 (String.length w - 1)) :: rest
    | other -> other
  in
  match parts with
  | ["projects"] | ["list"] -> List_projects
  | ["sessions"] -> List_sessions
  | ["claude-sessions"] -> List_claude_sessions
  | ["codex-sessions"] -> List_codex_sessions
  | ["gemini-sessions"] -> List_gemini_sessions
  | "start" :: project :: kind_str :: _ ->
    (match Config.agent_kind_of_string (String.lowercase_ascii kind_str) with
     | Ok kind -> Start_agent { project; kind = Some kind }
     | Error _ -> Unknown content)
  | ["start"; project] ->
    Start_agent { project; kind = None }
  | ["start"] ->
    List_projects
  | ["resume"; session_id] -> Resume_session { session_id; kind = None }
  | ["resume"; kind_str; session_id] ->
    (match Config.agent_kind_of_string (String.lowercase_ascii kind_str) with
     | Ok k -> Resume_session { session_id; kind = Some k }
     | Error _ -> Unknown content)
  | ["default-agent"] | ["default_agent"] -> Default_agent None
  | ["default-agent"; kind_str] | ["default_agent"; kind_str] ->
    (match Config.agent_kind_of_string (String.lowercase_ascii kind_str) with
     | Ok k -> Default_agent (Some k)
     | Error _ -> Unknown content)
  | ["rescue-agent"] | ["rescue_agent"] -> Rescue_agent None
  | ["rescue-agent"; kind_str] | ["rescue_agent"; kind_str] ->
    let kind_str = String.lowercase_ascii kind_str in
    if kind_str = "off" || kind_str = "none" then
      Rescue_agent (Some None)
    else
      (match Config.agent_kind_of_string kind_str with
       | Ok k -> Rescue_agent (Some (Some k))
       | Error _ -> Unknown content)
  | ["session-agent"] | ["session_agent"] -> Session_agent None
  | ["session-agent"; kind_str] | ["session_agent"; kind_str] ->
    (match Config.agent_kind_of_string (String.lowercase_ascii kind_str) with
     | Ok k -> Session_agent (Some k)
     | Error _ -> Unknown content)
  | ["stop"; thread_id] -> Stop_session { thread_id }
  | ["esc"] | ["int"] | ["interrupt"] -> Interrupt_session
  | ["cleanup-channels"] | ["cleanup"] -> Cleanup_channels
  | "rename" :: rest when rest <> [] ->
    (* !rename <name> — rename current thread
       !rename <thread_id> <name> — rename a specific thread *)
    let first = List.hd rest in
    let rest_tail = List.tl rest in
    (* If first token is all digits (a snowflake ID), treat it as thread_id *)
    let is_snowflake s = String.length s > 10 && String.length s < 25
      && String.for_all (fun c -> c >= '0' && c <= '9') s in
    if is_snowflake first && rest_tail <> [] then
      Rename_thread { thread_id = Some first;
                      name = String.concat " " rest_tail }
    else
      Rename_thread { thread_id = None;
                      name = String.concat " " rest }
  | ["restart"] -> Restart
  | ["refresh"] -> Refresh
  | ["status"] | ["version"] | ["info"] -> Status
  | ["desktop"] -> Desktop
  | ["mobile"] -> Mobile
  | ["wrapping"] -> Wrapping None
  | ["wrapping"; n] ->
    (match int_of_string_opt n with
     | Some w when w > 0 -> Wrapping (Some w)
     | _ -> Unknown content)
  | ["lines"] -> Lines None
  | ["lines"; n] ->
    (* Accept any positive int here; the handler enforces the upper
       bound (currently 1000) so the user gets a friendly message
       rather than silent rejection. *)
    (match int_of_string_opt n with
     | Some l when l > 0 -> Lines (Some l)
     | _ -> Unknown content)
  | ["scroll"] -> Scroll None
  | ["scroll"; n] ->
    (match int_of_string_opt n with
     | Some s when s <> 0 -> Scroll (Some s)
     | _ -> Unknown content)
  | ["help"] -> Help
  | _ -> Unknown content

(** Fuzzy-match a query against project names.
    Tries: exact, case-insensitive, numeric index, prefix, substring.
    Returns None on ambiguous or no match. *)
let find_project_fuzzy projects query =
  let q = String.lowercase_ascii query in
  match List.find_opt (fun (p : Project.t) -> p.name = query) projects with
  | Some _ as found -> found
  | None ->
  match List.find_opt (fun (p : Project.t) ->
    String.lowercase_ascii p.name = q) projects with
  | Some _ as found -> found
  | None ->
  match int_of_string_opt query with
  | Some n when n >= 1 && n <= List.length projects ->
    Some (List.nth projects (n - 1))
  | _ ->
  let prefix_matches = List.filter (fun (p : Project.t) ->
    let name = String.lowercase_ascii p.name in
    String.length name >= String.length q
    && String.sub name 0 (String.length q) = q
  ) projects in
  match prefix_matches with
  | [p] -> Some p
  | _ ->
  let substr_matches = List.filter (fun (p : Project.t) ->
    let name = String.lowercase_ascii p.name in
    let rec has_substr i =
      if i + String.length q > String.length name then false
      else if String.sub name i (String.length q) = q then true
      else has_substr (i + 1)
    in
    has_substr 0
  ) projects in
  match substr_matches with
  | [p] -> Some p
  | _ -> None
