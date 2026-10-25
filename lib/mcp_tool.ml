(** MCP tool descriptors for the discord-agents control surface.

    This module is the typed OCaml source of truth for the MCP tools
    exposed by the discord-agents-mcp stdio server. *)

type id =
  | Start_session
  | List_projects
  | List_hosts
  | Diagnose_host
  | List_project_inventory
  | Refresh_host_projects
  | Import_project
  | List_sessions
  | Send_message
  | Send_attachments
  | Stop_session
  | List_claude_sessions
  | List_codex_sessions
  | List_gemini_sessions
  | List_agy_sessions
  | Default_agent
  | Rescue_agent
  | Get_agent_config
  | Set_model
  | Set_effort
  | Set_goal
  | Start_login_flow
  | Resume_session
  | Restart_bot
  | Rename_thread
  | Cleanup_channels
  | Refresh_projects

type spec = {
  id : id;
  name : string;
  description : string;
  input_schema : Yojson.Safe.t;
  control_method : Control_api.method_id;
}

let string_of_id = function
  | Start_session -> "start_session"
  | List_projects -> "list_projects"
  | List_hosts -> "list_hosts"
  | Diagnose_host -> "diagnose_host"
  | List_project_inventory -> "list_project_inventory"
  | Refresh_host_projects -> "refresh_host_projects"
  | Import_project -> "import_project"
  | List_sessions -> "list_sessions"
  | Send_message -> "send_message"
  | Send_attachments -> "send_attachments"
  | Stop_session -> "stop_session"
  | List_claude_sessions -> "list_claude_sessions"
  | List_codex_sessions -> "list_codex_sessions"
  | List_gemini_sessions -> "list_gemini_sessions"
  | List_agy_sessions -> "list_agy_sessions"
  | Default_agent -> "default_agent"
  | Rescue_agent -> "rescue_agent"
  | Get_agent_config -> "get_agent_config"
  | Set_model -> "set_model"
  | Set_effort -> "set_effort"
  | Set_goal -> "set_goal"
  | Start_login_flow -> "start_login_flow"
  | Resume_session -> "resume_session"
  | Restart_bot -> "restart_bot"
  | Rename_thread -> "rename_thread"
  | Cleanup_channels -> "cleanup_channels"
  | Refresh_projects -> "refresh_projects"

let string_type = `String "string"

let integer_type = `String "integer"

let boolean_type = `String "boolean"

let null_type = `String "null"

let string_list_json values =
  `List (List.map (fun value -> `String value) values)

let field key value fields =
  match value with
  | None -> fields
  | Some value -> (key, value) :: fields

let property ?description ?enum ?items ?max_length ?min_items ?max_items
    ?minimum ?maximum ?default ~type_ () =
  `Assoc (
    [("type", type_)]
    |> field "description" (Option.map (fun s -> `String s) description)
    |> field "enum" (Option.map string_list_json enum)
    |> field "items" items
    |> field "maxLength" (Option.map (fun n -> `Int n) max_length)
    |> field "minItems" (Option.map (fun n -> `Int n) min_items)
    |> field "maxItems" (Option.map (fun n -> `Int n) max_items)
    |> field "minimum" (Option.map (fun n -> `Int n) minimum)
    |> field "maximum" (Option.map (fun n -> `Int n) maximum)
    |> field "default" default
    |> List.rev
  )

let object_schema ?(required=[]) properties =
  `Assoc (
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
    ] @
    if required = [] then [] else [("required", string_list_json required)]
  )

let no_args_schema =
  object_schema []

let agent_enum =
  ["claude"; "codex"; "gemini"; "agy"]

let session_id_property description =
  property ~type_:string_type ~description ()

let hours_property =
  property ~type_:integer_type
    ~description:"How many hours back to search"
    ~default:(`Int 24) ()

let spec ~id ~description ~input_schema ~control_method =
  { id; name = string_of_id id; description; input_schema; control_method }

let all_specs = [
  spec ~id:Start_session
    ~description:"Start a new agent session for a project (claude, codex, gemini, or agy). Creates a Discord thread and git worktree. Returns the thread info."
    ~control_method:Control_api.Start_session_id
    ~input_schema:(object_schema ~required:["project"] [
      ("project", property ~type_:string_type
        ~description:"Project name, number from the list, or a prefix/substring to fuzzy match" ());
      ("agent", property ~type_:string_type
        ~description:"Agent type: claude, codex, gemini (unmaintained; API-key auth only), or agy (Antigravity CLI). If omitted, uses the bot's current effective top-level agent (default agent unless a rescue agent is active under disk pressure)."
        ~enum:agent_enum ());
      ("thread_name", property ~type_:string_type
        ~description:"Short descriptive name for the thread (max 80 chars). If omitted, uses a default name."
        ~max_length:80 ());
      ("model", property ~type_:(`List [string_type; null_type])
        ~description:"Model name to pass to the agent CLI from the first invocation onward. Omit it or pass default/null to use the agent's configured default. Capped at 200 bytes on the server."
        ~max_length:200 ());
      ("initial_prompt", property ~type_:string_type
        ~description:"First message to send to the new agent. Posted visibly in the thread, then the agent starts working on it immediately \u{2014} the user does not need to send a follow-up. Keep it concise: describe the goal and any key context, not step-by-step instructions. Capped at 1900 *bytes* on the server (UTF-8 codepoint-aware truncation, so multi-byte characters can shorten the effective char count); the maxLength below is the worst case (all-ASCII)."
        ~max_length:1900 ());
    ]);
  spec ~id:List_projects
    ~description:"List all discovered projects that can have agent sessions started on them."
    ~control_method:Control_api.List_projects_id
    ~input_schema:no_args_schema;
  spec ~id:List_hosts
    ~description:"List configured executor hosts and the current writer without contacting them. This does not discover projects or establish host availability."
    ~control_method:Control_api.List_hosts_id
    ~input_schema:no_args_schema;
  spec ~id:Diagnose_host
    ~description:"Observe one enabled configured host's CLI version, advertised flags, credential-store presence and MCP callback. Read-only, bounded to 50 seconds, one diagnostic at a time. Does not log in, query models, or authorize sessions."
    ~control_method:Control_api.Diagnose_host_id
    ~input_schema:(object_schema ~required:["host"; "agent"; "cwd"] [
      ("host", property ~type_:string_type ~description:"Configured name from list_hosts" ());
      ("agent", property ~type_:string_type ~enum:agent_enum ());
      ("cwd", property ~type_:string_type ~max_length:4096
        ~description:"Absolute working directory on the selected host (4096-byte server cap). No directory is created." ());
    ]);
  spec ~id:List_project_inventory
    ~description:"List cached project locations grouped by repository across configured hosts. No probes run. Paginate by location offset; a repository can span pages. Inventory locations are not local start indices and do not authorize starts or cleanup."
    ~control_method:Control_api.List_project_inventory_id
    ~input_schema:(object_schema [
      ("offset", property ~type_:integer_type ~minimum:0 ~default:(`Int 0) ());
      ("limit", property ~type_:integer_type ~minimum:1 ~maximum:8 ~default:(`Int 8) ());
    ]);
  spec ~id:Refresh_host_projects
    ~description:"Refresh project observations on one enabled configured host, with a 30-second scan deadline and one refresh at a time. Persists the snapshot on the writer. Failed scans retain previous observations; no sessions or project directories are created."
    ~control_method:Control_api.Refresh_host_projects_id
    ~input_schema:(object_schema ~required:["host"] [
      ("host", property ~type_:string_type ~description:"Configured name from list_hosts" ());
    ]);
  spec ~id:Import_project
    ~description:"Clone a GitHub HTTPS or SSH URL into the bot's project registry, create its Discord project channel, and make the channel session-ready. Reuses local git credentials; no token is accepted."
    ~control_method:Control_api.Import_project_id
    ~input_schema:(object_schema ~required:["url"] [
      ("url", property ~type_:string_type
        ~description:"GitHub repository URL, e.g. https://github.com/owner/repo.git or git@github.com:owner/repo.git" ());
      ("name", property ~type_:string_type
        ~description:"Optional local project directory name under the configured base directory. Defaults to the GitHub repository name."
        ~max_length:100 ());
    ]);
  spec ~id:List_sessions
    ~description:"List active bot sessions (Discord threads with agent sessions attached)."
    ~control_method:Control_api.List_sessions_id
    ~input_schema:no_args_schema;
  spec ~id:Send_message
    ~description:"Send a visible user-style message to another active session thread, then route it through the same handler as a Discord user message. Use list_sessions to find thread IDs. The message is queued if the target session is busy. Do not send command-looking messages. If replying to an inter-agent message, pass its remaining_hops value so loops terminate."
    ~control_method:Control_api.Send_message_id
    ~input_schema:(object_schema ~required:["thread_id"; "message"] [
      ("thread_id", session_id_property
        "Discord thread ID of the destination active session");
      ("message", property ~type_:string_type
        ~description:"Plain message text to send. Must not start with ! and must fit in one Discord message. The bot enforces a 1600-byte cap, so non-ASCII text may allow fewer characters."
        ~max_length:1600 ());
      ("source_thread_id", property ~type_:string_type
        ~description:"Optional claimed Discord thread ID of the sending session, if known. This is displayed as an untrusted claimed source until caller context is wired in." ());
      ("remaining_hops", property ~type_:integer_type
        ~description:"Loop guard. Defaults to 3 for a new chain. If replying to an inter-agent message, pass the remaining_hops value shown in that message."
        ~minimum:1 ~maximum:5 ~default:(`Int 3) ());
    ]);
  spec ~id:Send_attachments
    ~description:"Upload one or more files from the calling session's worktree to its own Discord thread. Paths may be relative to the worktree or absolute within it; paths outside the worktree are rejected."
    ~control_method:Control_api.Send_attachments_id
    ~input_schema:(object_schema ~required:["paths"] [
      ("paths", property ~type_:(`String "array")
        ~description:"Local file paths to attach. Each resolved path must be a regular file inside the calling session's worktree. At most 10 files, 10 MiB each, and 25 MiB for the multipart request."
        ~items:(property ~type_:string_type ())
        ~min_items:1 ~max_items:10 ());
      ("message", property ~type_:string_type
        ~description:"Optional text for the attachment message. Must fit in one Discord message (2000 bytes); it is not split."
        ~max_length:2000 ());
    ]);
  spec ~id:Stop_session
    ~description:"Stop an active bot session by Discord thread ID. Idle sessions stop immediately; busy sessions terminate the active agent run and then clean up the session."
    ~control_method:Control_api.Stop_session_id
    ~input_schema:(object_schema ~required:["thread_id"] [
      ("thread_id", session_id_property "Discord thread ID of the session to stop");
    ]);
  spec ~id:List_claude_sessions
    ~description:"List recent Claude Code sessions running on this machine (last 24h). Useful for finding sessions to resume."
    ~control_method:Control_api.List_claude_sessions_id
    ~input_schema:(object_schema [("hours", hours_property)]);
  spec ~id:List_codex_sessions
    ~description:"List recent Codex CLI sessions on this machine (last 24h). Useful for finding sessions to resume."
    ~control_method:Control_api.List_codex_sessions_id
    ~input_schema:(object_schema [("hours", hours_property)]);
  spec ~id:List_gemini_sessions
    ~description:"List recent Gemini CLI sessions on this machine (last 24h). Useful for finding sessions to resume. Gemini support is unmaintained (API-key auth only)."
    ~control_method:Control_api.List_gemini_sessions_id
    ~input_schema:(object_schema [("hours", hours_property)]);
  spec ~id:List_agy_sessions
    ~description:"List recent agy (Antigravity CLI) conversations on this machine (last 24h). Useful for finding sessions to resume. A conversation shows a working directory only while it is the latest one for that directory; others are listed but can't be resumed by id."
    ~control_method:Control_api.List_agy_sessions_id
    ~input_schema:(object_schema [("hours", hours_property)]);
  spec ~id:Default_agent
    ~description:"Show or set the default agent used for new top-level sessions. Existing idle top-level sessions reset to a fresh session immediately; busy ones reset after their queued work finishes."
    ~control_method:Control_api.Default_agent_id
    ~input_schema:(object_schema [
      ("agent", property ~type_:string_type
        ~description:"Agent type to make the default. Omit to read the current default."
        ~enum:agent_enum ());
    ]);
  spec ~id:Rescue_agent
    ~description:"Show or set the rescue agent automatically used for new top-level sessions under disk pressure. Use agent=off to disable it."
    ~control_method:Control_api.Rescue_agent_id
    ~input_schema:(object_schema [
      ("agent", property ~type_:string_type
        ~description:"Agent type to use as the rescue agent, or off to disable rescue mode. Omit to read the current setting."
        ~enum:(agent_enum @ ["off"]) ());
    ]);
  spec ~id:Get_agent_config
    ~description:"Show the per-session agent configuration for a Discord thread: current values, every supported config value, a single-command briefing, and login repair hint."
    ~control_method:Control_api.Get_agent_config_id
    ~input_schema:(object_schema ~required:["thread_id"] [
      ("thread_id", session_id_property "Discord thread ID of the session");
    ]);
  spec ~id:Set_model
    ~description:"Set or clear the model override for an existing Discord agent session. Pass model explicitly; use model=default or an empty/null value to clear."
    ~control_method:Control_api.Set_model_id
    ~input_schema:(object_schema ~required:["thread_id"; "model"] [
      ("thread_id", session_id_property "Discord thread ID of the session");
      ("model", property ~type_:(`List [string_type; null_type])
        ~description:"Model name to pass to the agent CLI, or default/null to clear" ());
    ]);
  spec ~id:Set_effort
    ~description:"Set or clear the reasoning effort override for an existing Discord agent session. Pass effort explicitly. Use get_agent_config for the selected thread's supported values; set_effort validates against that thread's agent."
    ~control_method:Control_api.Set_effort_id
    ~input_schema:(object_schema ~required:["thread_id"; "effort"] [
      ("thread_id", session_id_property "Discord thread ID of the session");
      ("effort", property ~type_:(`List [string_type; null_type])
        ~description:"Reasoning effort value for the thread's agent, or default/null to clear" ());
    ]);
  spec ~id:Set_goal
    ~description:"Set, update, or clear a persisted Codex session goal. With current codex exec integration this is injected as prompt context; native Codex /goal requires app-server."
    ~control_method:Control_api.Set_goal_id
    ~input_schema:(object_schema ~required:["thread_id"] [
      ("thread_id", session_id_property "Discord thread ID of the session");
      ("objective", property ~type_:(`List [string_type; null_type])
        ~description:"Goal objective, max 4000 bytes. Required for a new goal; omit to update status/token_budget on an existing goal." ());
      ("status", property ~type_:string_type
        ~description:"Goal status"
        ~enum:["active"; "paused"; "blocked"; "usageLimited"; "budgetLimited"; "complete"] ());
      ("token_budget", property ~type_:(`List [integer_type; null_type])
        ~description:"Optional positive token budget" ());
      ("clear", property ~type_:boolean_type
        ~description:"Clear the stored goal" ());
    ]);
  spec ~id:Start_login_flow
    ~description:"Return the login repair command and executor host for a session, or the current writer host when no thread is supplied. The bot does not run OAuth itself."
    ~control_method:Control_api.Start_login_flow_id
    ~input_schema:(object_schema [
      ("thread_id", session_id_property "Discord thread ID whose agent needs login");
      ("agent", property ~type_:string_type
        ~description:"Agent kind when no thread_id is supplied"
        ~enum:agent_enum ());
    ]);
  spec ~id:Resume_session
    ~description:"Resume an existing Claude, Codex, Gemini, or agy session in a new Discord thread. Use list_claude_sessions / list_codex_sessions / list_gemini_sessions / list_agy_sessions to find a session ID. With kind unspecified, the bot tries the current effective top-level agent first, then the others."
    ~control_method:Control_api.Resume_session_id
    ~input_schema:(object_schema ~required:["session_id"] [
      ("session_id", property ~type_:string_type
        ~description:"Session ID or prefix (at least 8 characters)" ());
      ("kind", property ~type_:string_type
        ~description:"Which session store to search. Omit to try the current effective top-level agent first, then the others."
        ~enum:agent_enum ());
    ]);
  spec ~id:Restart_bot
    ~description:"Rebuild the discord-agents bot from source and restart it. Use after code changes."
    ~control_method:Control_api.Restart_id
    ~input_schema:no_args_schema;
  spec ~id:Rename_thread
    ~description:"Rename a Discord thread. Use from the control channel to rename any thread by ID, or specify the thread_id of the thread to rename."
    ~control_method:Control_api.Rename_thread_id
    ~input_schema:(object_schema ~required:["thread_id"; "name"] [
      ("thread_id", property ~type_:string_type
        ~description:"Discord thread ID (snowflake) to rename" ());
      ("name", property ~type_:string_type
        ~description:"New name for the thread (max 100 characters)" ());
    ]);
  spec ~id:Cleanup_channels
    ~description:"Delete stale Discord channels that don't match any current project."
    ~control_method:Control_api.Cleanup_channels_id
    ~input_schema:no_args_schema;
  spec ~id:Refresh_projects
    ~description:"Re-scan for new projects without restarting the bot. Use when a new project has been added to the base directories."
    ~control_method:Control_api.Refresh_projects_id
    ~input_schema:no_args_schema;
]

let spec_id spec = spec.id

let tool_name spec = spec.name

let description spec = spec.description

let input_schema spec = spec.input_schema

let control_method spec = spec.control_method

let control_method_name spec =
  Control_api.string_of_method_id spec.control_method

let control_method_timeout_s spec =
  match Control_api.method_spec_of_id spec.control_method with
  | Some method_spec -> Control_api.method_spec_timeout_s method_spec
  | None ->
    invalid_arg
      (Printf.sprintf "missing control method for MCP tool %s" spec.name)

let json_of_spec spec =
  `Assoc [
    ("name", `String spec.name);
    ("description", `String spec.description);
    ("inputSchema", spec.input_schema);
  ]

let tool_definitions_json =
  `List (List.map json_of_spec all_specs)
