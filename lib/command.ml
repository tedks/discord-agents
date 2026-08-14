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
  | Import_project of { url : string; name : string option }
  | Resume_session of { session_id : string; kind : Config.agent_kind option }
  | Fork_session of { move : bool option }
  (** [move = None] means the handler applies the channel-specific default. *)
  | Reset_session
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

type id =
  | List_projects_id
  | List_sessions_id
  | List_claude_sessions_id
  | List_codex_sessions_id
  | List_gemini_sessions_id
  | Start_agent_id
  | Import_project_id
  | Resume_session_id
  | Fork_session_id
  | Reset_session_id
  | Stop_session_id
  | Interrupt_session_id
  | Cleanup_channels_id
  | Default_agent_id
  | Rescue_agent_id
  | Session_agent_id
  | Restart_id
  | Refresh_id
  | Rename_thread_id
  | Status_id
  | Desktop_id
  | Mobile_id
  | Wrapping_id
  | Lines_id
  | Scroll_id
  | Help_id
  | Unknown_id

type drain_policy =
  | Allow_during_drain
  | Block_during_drain

type spec = {
  id : id;
  primary : string;
  aliases : string list;
  help : string list;
  no_arg_command : t option;
  drain_policy : drain_policy;
}

let id_of = function
  | List_projects -> List_projects_id
  | List_sessions -> List_sessions_id
  | List_claude_sessions -> List_claude_sessions_id
  | List_codex_sessions -> List_codex_sessions_id
  | List_gemini_sessions -> List_gemini_sessions_id
  | Start_agent _ -> Start_agent_id
  | Import_project _ -> Import_project_id
  | Resume_session _ -> Resume_session_id
  | Fork_session _ -> Fork_session_id
  | Reset_session -> Reset_session_id
  | Stop_session _ -> Stop_session_id
  | Interrupt_session -> Interrupt_session_id
  | Cleanup_channels -> Cleanup_channels_id
  | Default_agent _ -> Default_agent_id
  | Rescue_agent _ -> Rescue_agent_id
  | Session_agent _ -> Session_agent_id
  | Restart -> Restart_id
  | Refresh -> Refresh_id
  | Rename_thread _ -> Rename_thread_id
  | Status -> Status_id
  | Desktop -> Desktop_id
  | Mobile -> Mobile_id
  | Wrapping _ -> Wrapping_id
  | Lines _ -> Lines_id
  | Scroll _ -> Scroll_id
  | Help -> Help_id
  | Unknown _ -> Unknown_id

let string_of_id = function
  | List_projects_id -> "list_projects"
  | List_sessions_id -> "list_sessions"
  | List_claude_sessions_id -> "list_claude_sessions"
  | List_codex_sessions_id -> "list_codex_sessions"
  | List_gemini_sessions_id -> "list_gemini_sessions"
  | Start_agent_id -> "start_agent"
  | Import_project_id -> "import_project"
  | Resume_session_id -> "resume_session"
  | Fork_session_id -> "fork_session"
  | Reset_session_id -> "reset_session"
  | Stop_session_id -> "stop_session"
  | Interrupt_session_id -> "interrupt_session"
  | Cleanup_channels_id -> "cleanup_channels"
  | Default_agent_id -> "default_agent"
  | Rescue_agent_id -> "rescue_agent"
  | Session_agent_id -> "session_agent"
  | Restart_id -> "restart"
  | Refresh_id -> "refresh"
  | Rename_thread_id -> "rename_thread"
  | Status_id -> "status"
  | Desktop_id -> "desktop"
  | Mobile_id -> "mobile"
  | Wrapping_id -> "wrapping"
  | Lines_id -> "lines"
  | Scroll_id -> "scroll"
  | Help_id -> "help"
  | Unknown_id -> "unknown"

let command_spec ?(aliases=[]) ?no_arg_command ~id ~primary ~help
    ~drain_policy () =
  { id; primary; aliases; help; no_arg_command; drain_policy }

let read_only ?aliases ?no_arg_command ~id ~primary ~help () =
  command_spec ?aliases ?no_arg_command ~id ~primary ~help
    ~drain_policy:Allow_during_drain ()

let mutating ?aliases ?no_arg_command ~id ~primary ~help () =
  command_spec ?aliases ?no_arg_command ~id ~primary ~help
    ~drain_policy:Block_during_drain ()

let all_specs = [
  read_only ~id:List_projects_id ~primary:"projects" ~aliases:["list"]
    ~help:["`!projects` — list discovered projects"]
    ~no_arg_command:List_projects ();
  read_only ~id:List_sessions_id ~primary:"sessions"
    ~help:["`!sessions` — list active bot sessions"]
    ~no_arg_command:List_sessions ();
  read_only ~id:List_claude_sessions_id ~primary:"claude-sessions"
    ~help:["`!claude-sessions` — list recent Claude sessions"]
    ~no_arg_command:List_claude_sessions ();
  read_only ~id:List_codex_sessions_id ~primary:"codex-sessions"
    ~help:["`!codex-sessions` — list recent Codex sessions"]
    ~no_arg_command:List_codex_sessions ();
  read_only ~id:List_gemini_sessions_id ~primary:"gemini-sessions"
    ~help:["`!gemini-sessions` — list recent Gemini sessions"]
    ~no_arg_command:List_gemini_sessions ();
  mutating ~id:Start_agent_id ~primary:"start"
    ~help:[
      "`!start <project> [agent]` — start a session (defaults to the current effective top-level agent)";
    ] ();
  mutating ~id:Import_project_id ~primary:"import-project"
    ~aliases:["import_project"]
    ~help:[
      "`!import-project <github-url> [name]` — clone a GitHub repo into the project registry";
    ] ();
  mutating ~id:Default_agent_id ~primary:"default-agent"
    ~aliases:["default_agent"]
    ~help:[
      "`!default-agent [agent]` / `!default_agent [agent]` — show or set the default agent (claude|codex|gemini)";
    ]
    ~no_arg_command:(Default_agent None) ();
  mutating ~id:Rescue_agent_id ~primary:"rescue-agent"
    ~aliases:["rescue_agent"]
    ~help:[
      "`!rescue-agent [agent|off]` / `!rescue_agent [agent|off]` — show or set the rescue agent used under disk pressure";
    ]
    ~no_arg_command:(Rescue_agent None) ();
  mutating ~id:Session_agent_id ~primary:"session-agent"
    ~aliases:["session_agent"]
    ~help:[
      "`!session-agent [agent]` / `!session_agent [agent]` — show or set the current channel session agent";
    ]
    ~no_arg_command:(Session_agent None) ();
  mutating ~id:Resume_session_id ~primary:"resume"
    ~help:[
      "`!resume [agent] <session_id>` — resume a session (no agent = try the current effective top-level agent first)";
    ] ();
  mutating ~id:Fork_session_id ~primary:"fork"
    ~help:["`!fork [--move|--no-move]` — create a new thread for context management"]
    ~no_arg_command:(Fork_session { move = None }) ();
  mutating ~id:Reset_session_id ~primary:"reset"
    ~help:["`!reset` — clear this channel's session context"]
    ~no_arg_command:Reset_session ();
  mutating ~id:Stop_session_id ~primary:"stop"
    ~help:["`!stop <thread_id>` — stop a session"] ();
  mutating ~id:Interrupt_session_id ~primary:"esc"
    ~aliases:["int"; "interrupt"]
    ~help:["`!esc` / `!int` / `!interrupt` — stop this thread's active session"]
    ~no_arg_command:Interrupt_session ();
  mutating ~id:Rename_thread_id ~primary:"rename"
    ~help:["`!rename [thread_id] <name>` — rename a thread"] ();
  read_only ~id:Status_id ~primary:"status" ~aliases:["version"; "info"]
    ~help:[
      "`!status` — bot status and running processes";
      "`!version` — build info and runtime status";
    ]
    ~no_arg_command:Status ();
  mutating ~id:Refresh_id ~primary:"refresh"
    ~help:["`!refresh` — re-scan for new projects"]
    ~no_arg_command:Refresh ();
  mutating ~id:Cleanup_channels_id ~primary:"cleanup"
    ~aliases:["cleanup-channels"]
    ~help:["`!cleanup` — delete stale channels and prune fully-merged agent worktrees"]
    ~no_arg_command:Cleanup_channels ();
  mutating ~id:Restart_id ~primary:"restart"
    ~help:["`!restart` — rebuild and restart (warns but doesn't block active sessions)"]
    ~no_arg_command:Restart ();
  mutating ~id:Desktop_id ~primary:"desktop"
    ~help:["`!desktop` — set wrapping to desktop width"]
    ~no_arg_command:Desktop ();
  mutating ~id:Mobile_id ~primary:"mobile"
    ~help:["`!mobile` — set wrapping to mobile width"]
    ~no_arg_command:Mobile ();
  mutating ~id:Wrapping_id ~primary:"wrapping"
    ~help:["`!wrapping [n]` — show or set line wrap width"]
    ~no_arg_command:(Wrapping None) ();
  mutating ~id:Lines_id ~primary:"lines"
    ~help:["`!lines [n]` — show or set output lines for tool/code display"]
    ~no_arg_command:(Lines None) ();
  mutating ~id:Scroll_id ~primary:"scroll"
    ~help:[
      "`!scroll [n]` — view truncated output (n=block: 1=last, 2=2nd last; repeats advance)";
    ]
    ~no_arg_command:(Scroll None) ();
  read_only ~id:Help_id ~primary:"help"
    ~help:["`!help` — this message"]
    ~no_arg_command:Help ();
]

let names spec = spec.primary :: spec.aliases

let command_without_args name =
  let is_name spec = List.exists (( = ) name) (names spec) in
  match List.find_opt is_name all_specs with
  | Some { no_arg_command = Some cmd; _ } -> Some cmd
  | _ -> None

let spec_of_id id =
  List.find_opt (fun spec -> spec.id = id) all_specs

let spec_id spec = spec.id

let no_arg_command spec = spec.no_arg_command

let is_allowed_during_drain cmd =
  match spec_of_id (id_of cmd) with
  | Some { drain_policy = Allow_during_drain; _ } -> true
  | _ -> false

let help_lines =
  all_specs
  |> List.map (fun spec -> spec.help)
  |> List.flatten

(** Does this message look like a bot command? Requires ! prefix. *)
let is_command content =
  let trimmed = String.trim content in
  String.length trimmed > 0 && trimmed.[0] = '!'

let parse_agent_kind kind_str =
  Config.agent_kind_of_string (String.lowercase_ascii kind_str)

let parse content =
  let parts = String.split_on_char ' ' (String.trim content) in
  let parts = match parts with
    | w :: rest when String.length w > 0 && w.[0] = '!' ->
      String.lowercase_ascii (String.sub w 1 (String.length w - 1)) :: rest
    | other -> other
  in
  match parts with
  | "start" :: project :: kind_str :: _ ->
    (match parse_agent_kind kind_str with
     | Ok kind -> Start_agent { project; kind = Some kind }
     | Error _ -> Unknown content)
  | ["start"; project] ->
    Start_agent { project; kind = None }
  | ["start"] ->
    List_projects
  | ["import-project"; url] | ["import_project"; url] ->
    Import_project { url; name = None }
  | ["import-project"; url; name] | ["import_project"; url; name] ->
    Import_project { url; name = Some name }
  | ["resume"; session_id] -> Resume_session { session_id; kind = None }
  | ["resume"; kind_str; session_id] ->
    (match parse_agent_kind kind_str with
     | Ok k -> Resume_session { session_id; kind = Some k }
     | Error _ -> Unknown content)
  | ["fork"] -> Fork_session { move = None }
  | ["fork"; "--move"] -> Fork_session { move = Some true }
  | ["fork"; "--no-move"] -> Fork_session { move = Some false }
  | ["reset"] -> Reset_session
  | ["default-agent"; kind_str] | ["default_agent"; kind_str] ->
    (match parse_agent_kind kind_str with
     | Ok k -> Default_agent (Some k)
     | Error _ -> Unknown content)
  | ["rescue-agent"; kind_str] | ["rescue_agent"; kind_str] ->
    let kind_str = String.lowercase_ascii kind_str in
    if kind_str = "off" || kind_str = "none" then
      Rescue_agent (Some None)
    else
      (match parse_agent_kind kind_str with
       | Ok k -> Rescue_agent (Some (Some k))
       | Error _ -> Unknown content)
  | ["session-agent"; kind_str] | ["session_agent"; kind_str] ->
    (match parse_agent_kind kind_str with
     | Ok k -> Session_agent (Some k)
     | Error _ -> Unknown content)
  | ["stop"; thread_id] -> Stop_session { thread_id }
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
  | ["wrapping"; n] ->
    (match int_of_string_opt n with
     | Some w when w > 0 -> Wrapping (Some w)
     | _ -> Unknown content)
  | ["lines"; n] ->
    (* Accept any positive int here; the handler enforces the upper
       bound (currently 1000) so the user gets a friendly message
       rather than silent rejection. *)
    (match int_of_string_opt n with
     | Some l when l > 0 -> Lines (Some l)
     | _ -> Unknown content)
  | ["scroll"; n] ->
    (match int_of_string_opt n with
     | Some s when s <> 0 -> Scroll (Some s)
     | _ -> Unknown content)
  | [name] ->
    (match command_without_args name with
     | Some cmd -> cmd
     | None -> Unknown content)
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
