(** Pure command parsing — no I/O, no mutable state.

    Commands require a ! prefix to avoid hijacking natural language
    like "help me debug this" or "start here". *)

type t =
  | List_projects
  | List_sessions
  | List_claude_sessions
  | Start_agent of { project : string; kind : Config.agent_kind }
  | Resume_session of { session_id : string }
  | Stop_session of { thread_id : string }
  | Cleanup_channels
  | Restart
  | Rename_thread of { thread_id : string option; name : string }
  | Status
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
  | "start" :: project :: kind_str :: _ ->
    let kind = match Config.agent_kind_of_string (String.lowercase_ascii kind_str) with
      | Ok k -> k | Error _ -> Config.Claude in
    Start_agent { project; kind }
  | ["start"; project] ->
    Start_agent { project; kind = Config.Claude }
  | ["start"] ->
    List_projects
  | ["resume"; session_id] -> Resume_session { session_id }
  | ["stop"; thread_id] -> Stop_session { thread_id }
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
  | ["status"] | ["version"] | ["info"] -> Status
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
