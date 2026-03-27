(** Top-level bot orchestrator — thin wiring layer.

    Routes Discord messages to the appropriate handler:
    - Commands (! prefix) → Command module → handlers
    - Control channel chat → Claude session with MCP tools
    - Project channel chat → Claude session scoped to project
    - Thread messages → Agent_runner

    Owns no mutable state directly — delegates to Session_store
    and Channel_manager. *)

(** Derive the project source directory from the running executable path. *)
let project_root =
  lazy (
    try
      let exe = Sys.executable_name in
      let exe = if Filename.is_relative exe
        then Filename.concat (Sys.getcwd ()) exe else exe in
      let rec find_root path =
        if Sys.file_exists (Filename.concat path "dune-project") then path
        else
          let parent = Filename.dirname path in
          if parent = path then Sys.getcwd ()
          else find_root parent
      in
      find_root (Filename.dirname exe)
    with _ -> Sys.getcwd ()
  )

type t = {
  config : Config.t;
  rest : Discord_rest.t;
  gateway : Discord_gateway.t;
  projects : Project.t list;
  sessions : Session_store.t;
  channels : Channel_manager.t;
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
}

(** Find a usable working directory for a project. *)
let working_dir_of_project (p : Project.t) =
  if p.is_bare then
    let candidates = ["master"; "main"] in
    match List.find_opt (fun name ->
      let path = Filename.concat p.path name in
      try Sys.is_directory path with Sys_error _ -> false
    ) candidates with
    | Some name -> Ok (Filename.concat p.path name)
    | None -> Error "bare repo has no master/ or main/ worktree"
  else
    Ok p.path

(** System prompt for control channel Claude — knows about MCP tools. *)
let control_system_prompt projects =
  let project_list = String.concat "\n" (List.mapi (fun i (p : Project.t) ->
    Printf.sprintf "  %d. %s (%s)" (i+1) p.name p.path
  ) projects) in
  Printf.sprintf
"You are the control agent for a Discord bot that manages AI coding sessions.

You have MCP tools available:
- start_session: Start a new agent session for a project
- list_projects: List all discovered projects
- list_sessions: List active bot sessions
- list_claude_sessions: Find recent Claude Code sessions to resume
- resume_session: Resume an existing Claude session
- restart_bot: Rebuild and restart the bot
- rename_thread: Rename a Discord thread
- cleanup_channels: Delete stale Discord channels

USE THESE TOOLS. When the user asks to work on a project, start a session, etc., \
call the appropriate tool. Prefer the conversational MCP tools over suggesting \
!commands — the user shouldn't need to use !commands.

When starting a session, ALWAYS provide a short descriptive thread_name (max 80 chars) \
that captures the task — do NOT include the project name. Example: \
\"fix auth token refresh bug\" not \"myproject / fix auth token refresh bug\".

Known projects:
%s

Keep responses concise — this is Discord.

IMPORTANT: When starting sessions, always create a fresh worktree so agents don't \
stomp on each other's work." project_list

(** System prompt for project channel Claude — scoped to one project. *)
let project_system_prompt (project : Project.t) =
  Printf.sprintf
"You are the project overview agent for **%s** (at `%s`).

You have MCP tools available:
- start_session: Start a new agent session (creates a thread + worktree)
- list_projects: List all discovered projects
- list_sessions: List active bot sessions
- list_claude_sessions: Find recent Claude Code sessions to resume
- resume_session: Resume an existing Claude session
- rename_thread: Rename a Discord thread
- restart_bot: Rebuild and restart the bot
- cleanup_channels: Delete stale Discord channels

WHEN TO CREATE THREADS vs CHAT IN-CHANNEL:
- **Chat in-channel** for: questions, discussion, planning, code review, \
explaining things, brainstorming, status updates, or anything conversational.
- **Create a thread** (start_session) ONLY when the user explicitly asks to \
start working on something that needs its own worktree — e.g. \"start a session\", \
\"work on X\", \"fix this bug\", \"implement this feature\". The key signal is that \
code changes will be made.
- When in doubt, just chat. The user will ask for a thread if they want one.

When starting a session, ALWAYS provide a short descriptive thread_name (max 80 chars) \
that captures the task — do NOT include the project name. Example: \
\"fix auth token refresh bug\" not \"myproject / fix auth token refresh bug\".

Prefer the conversational MCP tools over suggesting !commands.
Keep responses concise — this is Discord.

IMPORTANT: When starting sessions, always create a fresh worktree so agents don't \
stomp on each other's work." project.name project.path

let post_response rest ~channel_id text =
  let text = Agent_process.reformat_tables text in
  List.iter (fun chunk ->
    match Discord_rest.create_message rest ~channel_id ~content:chunk () with
    | Ok _ -> ()
    | Error e -> Logs.warn (fun m -> m "bot: post error: %s" e)
  ) (Agent_process.split_message text)

(** Handle a parsed command. *)
let handle_command t msg cmd =
  let channel_id = msg.Discord_types.channel_id in
  let reply text =
    ignore (Discord_rest.create_message t.rest ~channel_id ~content:text ()) in
  match cmd with
  | Command.List_projects ->
    let lines = List.mapi (fun i (p : Project.t) ->
      Printf.sprintf "`%d.` **%s** — `%s`%s"
        (i + 1) p.name p.path (if p.is_bare then " [bare]" else "")
    ) t.projects in
    reply (if lines = [] then "No projects found."
      else "**Projects** (use `!start <name>` or `!start <number>`):\n"
           ^ String.concat "\n" lines)
  | Command.List_sessions ->
    let entries = Session_store.bindings t.sessions in
    let lines = List.map (fun (_tid, (s : Session_store.session)) ->
      Printf.sprintf "- **%s** / %s — %d messages (thread: <#%s>)"
        s.project_name
        (Config.string_of_agent_kind s.agent_kind)
        s.message_count s.thread_id
    ) entries in
    reply (if lines = [] then "No active sessions."
      else "**Sessions:**\n" ^ String.concat "\n" lines)
  | Command.List_claude_sessions ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let sessions = Claude_sessions.discover ~hours:24 () in
      let lines = List.map (fun (s : Claude_sessions.info) ->
        let age_min = int_of_float ((Unix.gettimeofday () -. s.mtime) /. 60.0) in
        let age_str = if age_min < 60 then Printf.sprintf "%dm ago" age_min
          else Printf.sprintf "%dh ago" (age_min / 60) in
        let sid_short = String.sub s.session_id 0
          (min 8 (String.length s.session_id)) in
        Printf.sprintf "- `%s` %s\n  %s — *%s*"
          sid_short age_str s.working_dir
          (if s.summary = "" then "(no summary)" else s.summary)
      ) (List.filteri (fun i _ -> i < 10) sessions) in
      reply (if lines = [] then "No recent Claude sessions found."
        else "**Recent Claude sessions** (last 24h):\n" ^ String.concat "\n" lines
             ^ "\n\nUse `!resume <session_id_prefix>` to attach."))
  | Command.Start_agent { project; kind } ->
    let proj = Command.find_project_fuzzy t.projects project in
    (match proj with
     | None ->
       let q = String.lowercase_ascii project in
       let suggestions = List.filter (fun (p : Project.t) ->
         let name = String.lowercase_ascii p.name in
         let rec has i = if i + String.length q > String.length name then false
           else if String.sub name i (String.length q) = q then true
           else has (i + 1) in has 0
       ) t.projects in
       (match suggestions with
        | [] -> reply (Printf.sprintf "No project matching `%s`. Try `!projects`." project)
        | _ -> reply (Printf.sprintf "No unique match for `%s`. Did you mean:\n%s" project
            (String.concat "\n" (List.map (fun (p : Project.t) ->
              Printf.sprintf "- `!start %s`" p.name) suggestions))))
     | Some p ->
       let kind_str = Config.string_of_agent_kind kind in
       let branch_name = Printf.sprintf "agent/%s-%s"
         kind_str (String.sub (Resource.generate_uuid ()) 0 8) in
       let working_dir, branch_info =
         match Project.create_worktree p ~branch_name with
         | Ok wt -> wt, Some branch_name
         | Error e ->
           Logs.warn (fun m -> m "bot: worktree failed: %s" e);
           (match working_dir_of_project p with
            | Ok wd -> wd, None
            | Error e2 -> reply (Printf.sprintf "No working directory: %s" e2); "", None)
       in
       if working_dir <> "" then begin
         let thread_parent =
           match Channel_manager.find_or_create ~rest:t.rest
                   ~guild_id:t.config.guild_id ~project:p t.channels with
           | Some ch_id -> ch_id
           | None -> channel_id
         in
         match Discord_rest.create_thread_no_message t.rest
                 ~channel_id:thread_parent
                 ~name:(Printf.sprintf "%s / %s" kind_str p.name) () with
         | Error e -> reply (Printf.sprintf "Failed to create thread: %s" e)
         | Ok thread_ch ->
           let session : Session_store.session = {
             project_name = p.name; working_dir; agent_kind = kind;
             session_id = Resource.generate_uuid ();
             thread_id = thread_ch.Discord_types.id;
             system_prompt = None; message_count = 0; processing = false;
           } in
           Session_store.add t.sessions ~thread_id:thread_ch.id session;
           let branch_str = match branch_info with
             | Some b -> Printf.sprintf "\nBranch: `%s`" b | None -> "" in
           ignore (Discord_rest.create_message t.rest ~channel_id:thread_ch.id
             ~content:(Printf.sprintf
               "**%s** session started for **%s**%s\nWorking in: `%s`\nSend a message to interact."
               kind_str p.name branch_str working_dir) ())
       end)
  | Command.Resume_session { session_id } ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let found = Eio_unix.run_in_systhread (fun () ->
        Claude_sessions.find_by_prefix session_id) in
      match found with
      | None -> reply (Printf.sprintf "No Claude session matching `%s`." session_id)
      | Some (full_sid, working_dir) ->
        let sid_short = String.sub full_sid 0 (min 8 (String.length full_sid)) in
        match Discord_rest.create_thread_no_message t.rest
                ~channel_id ~name:(Printf.sprintf "resume / %s" sid_short) () with
        | Error e -> reply (Printf.sprintf "Failed to create thread: %s" e)
        | Ok thread_ch ->
          let session : Session_store.session = {
            project_name = Filename.basename working_dir; working_dir;
            agent_kind = Config.Claude; session_id = full_sid;
            thread_id = thread_ch.Discord_types.id;
            system_prompt = None; message_count = 1; processing = false;
          } in
          Session_store.add t.sessions ~thread_id:thread_ch.id session;
          ignore (Discord_rest.create_message t.rest ~channel_id:thread_ch.id
            ~content:(Printf.sprintf
              "**Resumed** Claude session `%s`\nWorking in: `%s`\nSend a message to continue."
              sid_short working_dir) ()))
  | Command.Stop_session { thread_id } ->
    (match Session_store.find_opt t.sessions ~thread_id with
     | None -> reply "Session not found."
     | Some session ->
       Session_store.remove t.sessions ~thread_id;
       reply (Printf.sprintf "Stopped session for **%s**." session.project_name))
  | Command.Cleanup_channels ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      match Channel_manager.cleanup ~rest:t.rest
              ~guild_id:t.config.guild_id ~projects:t.projects t.channels with
      | Error e -> reply (Printf.sprintf "Cleanup failed: %s" e)
      | Ok 0 -> reply "No stale channels to clean up."
      | Ok n -> reply (Printf.sprintf "Cleaned up %d stale channels." n))
  | Command.Restart ->
    reply "Rebuilding and restarting...";
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      match Eio_unix.run_in_systhread (fun () ->
        let root = Lazy.force project_root in
        let exit_code = Sys.command
          (Printf.sprintf "cd %s && nix develop --command dune build 2>&1"
            (Filename.quote root)) in
        if exit_code <> 0 then `Build_failed
        else begin
          let _pid = Unix.create_process "/bin/sh"
            [| "/bin/sh"; "-c";
               Printf.sprintf "cd %s && nix develop --command dune exec discord-agents &"
                 (Filename.quote root) |]
            Unix.stdin Unix.stdout Unix.stderr in
          `Restarting
        end) with
      | `Build_failed -> reply "Build failed, not restarting."
      | `Restarting ->
        reply "Build succeeded. New instance starting.";
        Eio.Time.sleep (Eio.Stdenv.clock t.env) 30.0)
  | Command.Rename_thread { thread_id; name } ->
    let target_id = match thread_id with
      | Some tid -> tid
      | None -> channel_id  (* rename the current thread *)
    in
    (match Discord_rest.modify_channel t.rest ~channel_id:target_id ~name () with
     | Ok _ -> reply (Printf.sprintf "Renamed to **%s**." name)
     | Error e -> reply (Printf.sprintf "Rename failed: %s" e))
  | Command.Status ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let status_lines = Eio_unix.run_in_systhread (fun () ->
        (* Bot process info *)
        let pid = Unix.getpid () in
        let uptime_str =
          try
            let ic = open_in (Printf.sprintf "/proc/%d/stat" pid) in
            let line = input_line ic in
            close_in ic;
            let parts = String.split_on_char ' ' line in
            let starttime = float_of_string (List.nth parts 21) in
            let ticks_per_sec = 100.0 in
            let boot_ic = open_in "/proc/stat" in
            let rec find_btime () =
              let l = input_line boot_ic in
              if String.length l > 6 && String.sub l 0 6 = "btime " then
                float_of_string (String.trim (String.sub l 6 (String.length l - 6)))
              else find_btime ()
            in
            let btime = find_btime () in
            close_in boot_ic;
            let start_time = btime +. (starttime /. ticks_per_sec) in
            let uptime = Unix.gettimeofday () -. start_time in
            let hours = int_of_float (uptime /. 3600.0) in
            let mins = int_of_float (mod_float (uptime /. 60.0) 60.0) in
            Printf.sprintf "%dh %dm" hours mins
          with _ -> "unknown"
        in
        (* Running claude processes *)
        let claude_procs =
          try
            let ic = Unix.open_process_in
              "ps -eo pid,etimes,args 2>/dev/null | grep 'claude.*stream-json' | grep -v grep" in
            let lines = ref [] in
            (try while true do lines := input_line ic :: !lines done
             with End_of_file -> ());
            ignore (Unix.close_process_in ic);
            List.rev !lines
          with _ -> []
        in
        let claude_lines = List.filter_map (fun line ->
          let parts = String.split_on_char ' ' (String.trim line) in
          let parts = List.filter (fun s -> s <> "") parts in
          match parts with
          | _pid :: elapsed_s :: _rest ->
            let elapsed = try int_of_string elapsed_s with _ -> 0 in
            let mins = elapsed / 60 in
            (* Extract session ID if present *)
            let sid = try
              let re_start = "--resume " in
              let idx = ref 0 in
              let full = String.concat " " _rest in
              let found = ref "" in
              String.iteri (fun i _ ->
                if i + String.length re_start <= String.length full
                   && String.sub full i (String.length re_start) = re_start then
                  let after = String.sub full (i + String.length re_start)
                    (min 36 (String.length full - i - String.length re_start)) in
                  let sid = List.hd (String.split_on_char ' ' after) in
                  if !found = "" then (found := sid; idx := i)
              ) full;
              if !found <> "" then String.sub !found 0 (min 8 (String.length !found))
              else "?"
            with _ -> "?" in
            Some (Printf.sprintf "  `%s` — %dm" sid mins)
          | _ -> None
        ) claude_procs in
        (* Other bot instances *)
        let other_bots =
          try
            let ic = Unix.open_process_in
              "ps -eo pid,etimes,args 2>/dev/null | grep discord-agents | grep -v grep" in
            let lines = ref [] in
            (try while true do lines := input_line ic :: !lines done
             with End_of_file -> ());
            ignore (Unix.close_process_in ic);
            let count = List.length (List.rev !lines) in
            if count > 1 then
              Printf.sprintf "\n**\xe2\x9a\xa0\xef\xb8\x8f %d bot instances running** (expected 1)" count
            else ""
          with _ -> ""
        in
        (* Session summary *)
        let entries = Session_store.bindings t.sessions in
        let total = List.length entries in
        let with_msgs = List.length (List.filter (fun (_, (s : Session_store.session)) ->
          s.message_count > 0) entries) in
        let processing = List.length (List.filter (fun (_, (s : Session_store.session)) ->
          s.processing) entries) in
        let lines = [
          Printf.sprintf "**Bot Status** (pid %d, up %s)" pid uptime_str;
          Printf.sprintf "Sessions: %d total, %d active, %d processing" total with_msgs processing;
          Printf.sprintf "Projects: %d discovered" (List.length t.projects);
        ] in
        let lines = if claude_lines <> [] then
          lines @ [Printf.sprintf "**Running agents** (%d):" (List.length claude_lines)]
          @ claude_lines
        else
          lines @ ["No running agent processes."]
        in
        let lines = if other_bots <> "" then lines @ [other_bots] else lines in
        lines
      ) in
      reply (String.concat "\n" status_lines))
  | Command.Help ->
    reply (String.concat "\n" [
      "**Commands:**";
      "`!projects` — list discovered projects";
      "`!sessions` — list active bot sessions";
      "`!claude-sessions` — list recent Claude sessions";
      "`!start <project> [agent]` — start a session (defaults to claude)";
      "`!resume <session_id>` — resume a Claude session";
      "`!stop <thread_id>` — stop a session";
      "`!rename [thread_id] <name>` — rename a thread";
      "`!status` — bot status and running processes";
      "`!cleanup` — delete stale channels";
      "`!restart` — rebuild and restart";
      "`!help` — this message";
    ])
  | Command.Unknown _ -> ()

(** Resolve the channel name and type for context injection.
    Accepts an optional pre-fetched channel to avoid redundant API calls
    (e.g. when handle_message already looked up the thread). *)
let resolve_channel_context t ~(channel_id : Discord_types.channel_id)
    ~(session : Session_store.session) ?channel_info () =
  let is_control = match t.config.control_channel_id with
    | Some ctl_id -> channel_id = ctl_id | None -> false in
  if is_control then ("control", "control-channel")
  else
    (* Check if this is a project channel (not a thread) *)
    match Channel_manager.project_for_channel t.channels ~channel_id with
    | Some _ -> (session.project_name, "project-channel")
    | None ->
      (* It's a thread — use pre-fetched info or look it up *)
      let ch_opt = match channel_info with
        | Some ch -> Some ch
        | None ->
          (match Discord_rest.get_channel t.rest ~channel_id () with
           | Ok ch -> Some ch | Error _ -> None)
      in
      let name = match ch_opt with
        | Some ch -> Option.value ~default:"unknown" ch.Discord_types.name
        | None -> session.project_name
      in
      (name, "thread")

(** Handle a message in a session thread.
    [channel_info] is passed through when the caller already fetched it. *)
let handle_thread_message t msg ?channel_info () =
  match Session_store.find_opt t.sessions ~thread_id:msg.Discord_types.channel_id with
  | None -> ()
  | Some session ->
    if session.processing then
      ignore (Discord_rest.create_message t.rest
        ~channel_id:msg.channel_id ~content:"Still processing previous message..." ())
    else begin
      session.processing <- true;
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        Fun.protect ~finally:(fun () -> session.processing <- false) (fun () ->
          let channel_id = msg.Discord_types.channel_id in
          ignore (Discord_rest.create_reaction t.rest ~channel_id
            ~message_id:msg.id ~emoji:"\xF0\x9F\x91\x80" ());
          Channel_manager.bump ~rest:t.rest ~guild_id:t.config.guild_id
            ~project_name:session.project_name t.channels;
          let author_name = msg.author.username in
          let (channel_name, channel_type) =
            resolve_channel_context t ~channel_id ~session ?channel_info () in
          match Agent_runner.run ~sw:t.sw ~env:t.env ~rest:t.rest
                  ~session ~channel_id ~prompt:msg.content
                  ~attachments:msg.attachments
                  ~author_name ~channel_name ~channel_type () with
          | Ok () ->
            Session_store.increment_message_count t.sessions session
          | Error _ -> ()))
    end

(** Ensure a session exists for a channel (control channel only).
    Project channels use auto-thread creation instead. *)
let ensure_channel_session t ~channel_id ~project_name ~working_dir ~system_prompt =
  match Session_store.find_opt t.sessions ~thread_id:channel_id with
  | Some _ -> ()
  | None ->
    let session : Session_store.session = {
      project_name; working_dir; agent_kind = Config.Claude;
      session_id = Resource.generate_uuid (); thread_id = channel_id;
      system_prompt; message_count = 0; processing = false;
    } in
    Session_store.add t.sessions ~thread_id:channel_id session;
    Logs.info (fun m -> m "bot: auto-created session for %s" project_name)

(** Route an incoming Discord message. *)
let handle_message t (msg : Discord_types.message) =
  Session_store.maybe_reload t.sessions;
  match msg.author.bot with Some true -> () | _ ->
  if Command.is_command msg.content then
    handle_command t msg (Command.parse msg.content)
  else begin
    let is_control = match t.config.control_channel_id with
      | Some ctl_id -> msg.channel_id = ctl_id | None -> false in
    let project_for_channel =
      Channel_manager.project_for_channel t.channels ~channel_id:msg.channel_id in
    if is_control then begin
      ensure_channel_session t ~channel_id:msg.channel_id
        ~project_name:"control" ~working_dir:(Sys.getcwd ())
        ~system_prompt:(Some (control_system_prompt t.projects));
      handle_thread_message t msg ()
    end else match project_for_channel with
    | Some proj_name ->
      (* Message in a project channel — persistent session (like control channel).
         The project Claude can create threads via MCP tools when needed. *)
      let proj = List.find_opt (fun (p : Project.t) -> p.name = proj_name) t.projects in
      (match proj with
       | Some p ->
         let wd = match working_dir_of_project p with Ok d -> d | Error _ -> p.path in
         ensure_channel_session t ~channel_id:msg.channel_id
           ~project_name:p.name ~working_dir:wd
           ~system_prompt:(Some (project_system_prompt p));
         handle_thread_message t msg ()
       | None -> ())
    | None ->
      (* Check if this is a thread under a project channel *)
      (match Session_store.find_opt t.sessions ~thread_id:msg.channel_id with
       | Some _ -> handle_thread_message t msg ()
       | None ->
         (* Look up the channel to find its parent *)
         (match Discord_rest.get_channel t.rest ~channel_id:msg.channel_id () with
          | Ok ch ->
            let parent_project = match ch.Discord_types.parent_id with
              | Some pid -> Channel_manager.project_for_channel t.channels ~channel_id:pid
              | None -> None
            in
            (match parent_project with
             | Some proj_name ->
               let proj = List.find_opt (fun (p : Project.t) ->
                 p.name = proj_name) t.projects in
               (match proj with
                | Some p ->
                  let wd = match working_dir_of_project p with
                    | Ok d -> d | Error _ -> p.path in
                  (* Worker threads get no system prompt — they're focused agents.
                     They still get MCP tools via agent_process.ml. *)
                  ensure_channel_session t ~channel_id:msg.channel_id
                    ~project_name:p.name ~working_dir:wd
                    ~system_prompt:None;
                  (* Pass the already-fetched channel info to avoid a second API call *)
                  handle_thread_message t msg ~channel_info:ch ()
                | None -> handle_thread_message t msg ())
             | None -> handle_thread_message t msg ())
          | Error e ->
            Logs.warn (fun m -> m "bot: channel lookup failed for %s: %s"
              msg.channel_id e);
            ignore (Discord_rest.create_message t.rest
              ~channel_id:msg.channel_id
              ~content:"Could not set up a session for this thread (channel lookup failed). Try again or use `!start`." ())))
  end

let create ~sw ~(env : Eio_unix.Stdenv.base) config =
  let rest = Discord_rest.create ~sw ~env ~token:config.Config.discord_token in
  let projects = Project.discover ~base_directories:config.base_directories in
  let sessions = Session_store.create () in
  let channels = Channel_manager.create () in
  let gateway = Discord_gateway.create
    ~token:config.discord_token
    ~intents:Discord_gateway.default_intents
    ~handler:(fun _event -> ())
  in
  let bot = { config; rest; gateway; projects; sessions; channels; env; sw } in
  bot.gateway.handler <- (fun event ->
    match event with
    | Discord_gateway.Connected user ->
      Logs.info (fun m -> m "bot: connected as %s" user.Discord_types.username);
      if bot.channels.category_id = None then
        Eio.Fiber.fork ~sw (fun () ->
          Channel_manager.setup ~rest ~guild_id:config.guild_id ~projects bot.channels;
          match config.control_channel_id with
          | Some ch_id ->
            let text = Printf.sprintf "Bot online. %d projects, %d channels, %d sessions."
              (List.length projects) (Channel_manager.count bot.channels)
              (Session_store.count bot.sessions) in
            ignore (Discord_rest.create_message rest ~channel_id:ch_id ~content:text ())
          | None -> ())
    | Discord_gateway.Message_received msg -> handle_message bot msg
    | Discord_gateway.Thread_created ch ->
      Logs.info (fun m -> m "bot: thread created: %s"
        (Option.value ~default:"(unnamed)" ch.Discord_types.name))
    | Discord_gateway.Disconnected reason ->
      Logs.warn (fun m -> m "bot: disconnected: %s" reason));
  bot

let run ~sw:_ ~(env : Eio_unix.Stdenv.base) bot =
  Logs.info (fun m -> m "bot: discovered %d projects" (List.length bot.projects));
  List.iter (fun (p : Project.t) ->
    Logs.info (fun m -> m "  - %s (%s)" p.name p.path)
  ) bot.projects;
  Discord_gateway.connect ~sw:bot.sw ~env bot.gateway
