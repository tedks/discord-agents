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

module Pid_set = Set.Make(Int)

type t = {
  config : Config.t;
  rest : Discord_rest.t;
  gateway : Discord_gateway.t;
  projects : Project.t list;
  sessions : Session_store.t;
  channels : Channel_manager.t;
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  started_at : float;
  mutable draining : bool;
  child_pids : (Pid_set.t ref * Mutex.t);
  mutable wrap_width : int;
}

let register_child_pid t pid =
  let (pids, mu) = t.child_pids in
  Mutex.lock mu;
  pids := Pid_set.add pid !pids;
  Mutex.unlock mu

let unregister_child_pid t pid =
  let (pids, mu) = t.child_pids in
  Mutex.lock mu;
  pids := Pid_set.remove pid !pids;
  Mutex.unlock mu

let get_child_pids t =
  let (pids, mu) = t.child_pids in
  Mutex.lock mu;
  let result = !pids in
  Mutex.unlock mu;
  result

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

(** Shared instructions for agents that can start sessions. *)
let session_starting_instructions =
  "When starting a session, ALWAYS provide:\n\
   - A short descriptive thread_name (max 80 chars) that captures the task — \
   do NOT include the project name. Example: \"fix auth token refresh bug\"\n\
   - An initial_prompt that gives the new agent context about what to do. \
   Summarize the user's request and any relevant context from the conversation. \
   Keep it concise — the agent should be able to start working immediately, \
   but hand control back to the user quickly rather than acting autonomously."

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

%s

Known projects:
%s

Keep responses concise — this is Discord.

IMPORTANT: When starting sessions, always create a fresh worktree so agents don't \
stomp on each other's work.

IMPORTANT: When linking to GitHub PRs, issues, or commits, always use full URLs \
(e.g. https://github.com/owner/repo/pull/1) — never shorthand like owner/repo#1. \
Discord does not render GitHub shorthand as clickable links."
  session_starting_instructions project_list

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

%s

Prefer the conversational MCP tools over suggesting !commands.
Keep responses concise — this is Discord.

IMPORTANT: When linking to GitHub PRs, issues, or commits, always use full URLs \
(e.g. https://github.com/owner/repo/pull/1) — never shorthand like owner/repo#1. \
Discord does not render GitHub shorthand as clickable links.

IMPORTANT: When starting sessions, always create a fresh worktree so agents don't \
stomp on each other's work."
  project.name project.path session_starting_instructions

(** Trigger a graceful restart: drain → reap → build → spawn.
    Callable from command handler or signal handler.
    [notify] is called with status messages (may be a no-op for signal-triggered restarts). *)
let trigger_restart t ~notify =
  if t.draining then begin
    notify "Already restarting.";
  end else begin
  (* Set draining BEFORE forking to close the race window where
     a queued message could slip through before the flag flips. *)
  t.draining <- true;
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    Fun.protect ~finally:(fun () ->
      (* Safety net: always reset draining if we exit without exec'ing.
         Covers unexpected exceptions, build failures, and handoff timeout. *)
      t.draining <- false
    ) (fun () ->
      (* Phase 1: Drain — wait for in-flight sessions to complete *)
      let processing_count () =
        List.length (List.filter (fun (_, (s : Session_store.session)) ->
          s.processing) (Session_store.bindings t.sessions)) in
      let active = processing_count () in
      if active > 0 then begin
        notify (Printf.sprintf "Draining %d active session(s)..." active);
        let clock = Eio.Stdenv.clock t.env in
        let deadline = Eio.Time.now clock +. 300.0 in
        let rec wait () =
          let n = processing_count () in
          if n = 0 then ()
          else if Eio.Time.now clock > deadline then
            notify (Printf.sprintf "Timed out waiting for %d session(s). Proceeding." n)
          else begin
            Eio.Time.sleep clock 1.0;
            wait ()
          end
        in
        wait ()
      end;
      (* Clear any remaining queued messages and notify users *)
      List.iter (fun (_tid, (s : Session_store.session)) ->
        let dropped = ref 0 in
        while not (Queue.is_empty s.pending_queue) do
          let pending = Queue.pop s.pending_queue in
          incr dropped;
          ignore (Discord_rest.delete_own_reaction t.rest
            ~channel_id:pending.msg.Discord_types.channel_id
            ~message_id:pending.msg.id ~emoji:"\xE2\x8F\xB3" ());
          ignore (Discord_rest.create_reaction t.rest
            ~channel_id:pending.msg.Discord_types.channel_id
            ~message_id:pending.msg.id ~emoji:"\xE2\x9D\x8C" ())
        done;
        if !dropped > 0 then
          Logs.info (fun m -> m "bot: dropped %d queued message(s) for %s during restart"
            !dropped s.project_name)
      ) (Session_store.bindings t.sessions);
      (* Phase 2: Reap child processes.
         Split SIGTERM and SIGKILL into separate systhread calls with
         Eio.Time.sleep between to avoid blocking the systhread pool. *)
      let pids = get_child_pids t in
      if not (Pid_set.is_empty pids) then begin
        notify (Printf.sprintf "Terminating %d child process(es)..." (Pid_set.cardinal pids));
        Eio_unix.run_in_systhread (fun () ->
          Pid_set.iter (fun pid ->
            (try Unix.kill pid Sys.sigterm
             with Unix.Unix_error _ -> ())
          ) pids);
        Eio.Time.sleep (Eio.Stdenv.clock t.env) 2.0;
        Eio_unix.run_in_systhread (fun () ->
          Pid_set.iter (fun pid ->
            (try Unix.kill pid 0; Unix.kill pid Sys.sigkill
             with Unix.Unix_error _ -> ())
          ) pids)
      end;
      (* Phase 3: Build and restart *)
      notify "Building...";
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
      | `Build_failed ->
        notify "Build failed, not restarting."
        (* draining reset by Fun.protect finally *)
      | `Restarting ->
        notify "Build succeeded. New instance starting — shutting down in 30s.";
        (* Wait for new instance to take over, then exit. *)
        Eio.Time.sleep (Eio.Stdenv.clock t.env) 30.0;
        Logs.info (fun m -> m "bot: restart handoff timeout, exiting");
        exit 0))
  end

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
             pending_queue = Queue.create (); initial_prompt = None;
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
      | Some (full_sid, raw_working_dir) ->
        let sid_short = String.sub full_sid 0 (min 8 (String.length full_sid)) in
        (* Match working directory to a known project for channel routing *)
        let matched_project = List.find_opt (fun (p : Project.t) ->
          raw_working_dir = p.path
          || (String.length raw_working_dir > String.length p.path + 1
              && String.sub raw_working_dir 0 (String.length p.path + 1)
                 = p.path ^ "/")
        ) t.projects in
        let thread_parent = match matched_project with
          | Some p ->
            (match Channel_manager.find_or_create ~rest:t.rest
                     ~guild_id:t.config.guild_id ~project:p t.channels with
             | Some ch_id -> ch_id
             | None -> channel_id)
          | None -> channel_id
        in
        (* If working_dir is a bare repo root, use the main worktree instead *)
        let working_dir = match matched_project with
          | Some p when p.is_bare && raw_working_dir = p.path ->
            (match working_dir_of_project p with
             | Ok wd -> wd
             | Error _ -> raw_working_dir)
          | _ -> raw_working_dir
        in
        let project_name = match matched_project with
          | Some p -> p.name
          | None -> Filename.basename raw_working_dir
        in
        (match Discord_rest.create_thread_no_message t.rest
                ~channel_id:thread_parent ~name:(Printf.sprintf "resume / %s" sid_short) () with
        | Error e -> reply (Printf.sprintf "Failed to create thread: %s" e)
        | Ok thread_ch ->
          let session : Session_store.session = {
            project_name; working_dir;
            agent_kind = Config.Claude; session_id = full_sid;
            thread_id = thread_ch.Discord_types.id;
            system_prompt = None; message_count = 1; processing = false;
            pending_queue = Queue.create (); initial_prompt = None;
          } in
          Session_store.add t.sessions ~thread_id:thread_ch.id session;
          ignore (Discord_rest.create_message t.rest ~channel_id:thread_ch.id
            ~content:(Printf.sprintf
              "**Resumed** Claude session `%s`\nWorking in: `%s`\nSend a message to continue."
              sid_short working_dir) ())))
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
    trigger_restart t ~notify:reply
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
        let pid = Unix.getpid () in
        let uptime_sec = int_of_float (Unix.gettimeofday () -. t.started_at) in
        let hours = uptime_sec / 3600 in
        let minutes = (uptime_sec mod 3600) / 60 in
        let uptime_str = if hours > 0
          then Printf.sprintf "%dh %dm" hours minutes
          else Printf.sprintf "%dm" minutes in
        (* Running agent child processes — filter by PPID to only show
           processes spawned by this bot, not unrelated Claude CLI usage *)
        let my_pid_str = string_of_int pid in
        let agent_procs =
          try
            let ic = Unix.open_process_in
              (Printf.sprintf
                "ps -eo pid,ppid,etimes,args 2>/dev/null | awk '$2 == %s' | grep -v grep"
                my_pid_str) in
            let lines = ref [] in
            (try while true do lines := input_line ic :: !lines done
             with End_of_file -> ());
            ignore (Unix.close_process_in ic);
            List.rev !lines
          with _ -> []
        in
        let agent_lines = List.filter_map (fun line ->
          let parts = String.split_on_char ' ' (String.trim line) in
          let parts = List.filter (fun s -> s <> "") parts in
          match parts with
          | _pid :: _ppid :: elapsed_s :: _rest ->
            let elapsed = try int_of_string elapsed_s with _ -> 0 in
            let mins = elapsed / 60 in
            let full = String.concat " " _rest in
            (* Identify agent kind from command *)
            let kind =
              if String.length full > 0 then
                let first_arg = List.hd (String.split_on_char ' ' full) in
                Filename.basename first_arg
              else "unknown"
            in
            let sid =
              try
                let re_start = "--resume " in
                let found = ref "" in
                String.iteri (fun i _ ->
                  if i + String.length re_start <= String.length full
                     && String.sub full i (String.length re_start) = re_start then
                    let after = String.sub full (i + String.length re_start)
                      (min 36 (String.length full - i - String.length re_start)) in
                    let sid = List.hd (String.split_on_char ' ' after) in
                    if !found = "" then found := sid
                ) full;
                if !found <> "" then String.sub !found 0 (min 8 (String.length !found))
                else ""
              with _ -> ""
            in
            let label = if sid <> "" then Printf.sprintf "%s `%s`" kind sid
              else kind in
            Some (Printf.sprintf "  %s — %dm" label mins)
          | _ -> None
        ) agent_procs in
        (* Detect multiple bot instances *)
        let other_bots =
          try
            let ic = Unix.open_process_in
              "ps -eo pid,args 2>/dev/null | grep discord-agents | grep -v grep" in
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
        let lines = [
          Printf.sprintf "**%s** (pid %d, up %s)" (Build_info.version_string ()) pid uptime_str;
          Printf.sprintf "Sessions: %d (%d processing)"
            (Session_store.count t.sessions)
            (List.length (List.filter (fun (_, (s : Session_store.session)) ->
              s.processing) (Session_store.bindings t.sessions)));
          Printf.sprintf "Projects: %d  |  Channels: %d"
            (List.length t.projects) (Channel_manager.count t.channels);
        ] in
        let lines = if agent_lines <> [] then
          lines @ [Printf.sprintf "**Running agents** (%d):" (List.length agent_lines)]
          @ agent_lines
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
      "`!restart` — rebuild and restart (warns but doesn't block active sessions)";
      "`!version` — build info and runtime status";
      "`!desktop` — set wrapping to desktop width";
      "`!mobile` — set wrapping to mobile width";
      "`!wrapping [n]` — show or set line wrap width";
      "`!help` — this message";
    ])
  | Command.Desktop ->
    t.wrap_width <- Agent_process.desktop_width;
    reply (Printf.sprintf "Wrapping set to desktop (%d chars)."
      Agent_process.desktop_width)
  | Command.Mobile ->
    t.wrap_width <- Agent_process.mobile_width;
    reply (Printf.sprintf "Wrapping set to mobile (%d chars)."
      Agent_process.mobile_width)
  | Command.Wrapping None ->
    reply (Printf.sprintf "Current wrapping: %d chars." t.wrap_width)
  | Command.Wrapping (Some w) ->
    t.wrap_width <- w;
    reply (Printf.sprintf "Wrapping set to %d chars." w)
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
    [channel_info] is passed through when the caller already fetched it.

    During drain mode (restart pending), messages are still processed but
    the user is warned. This is intentional: blocking messages during drain
    would prevent using other sessions while a long-running task finishes.
    The restart waits for all session.processing flags to go false. *)
let handle_thread_message t msg ?channel_info () =
  if t.draining then
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.Discord_types.channel_id
      ~content:"Bot is restarting and will restart when there are no running processes. Sending more messages may delay restart." ());
  match Session_store.find_opt t.sessions ~thread_id:msg.Discord_types.channel_id with
  | None -> ()
  | Some session ->
    if session.processing then begin
      (* Queue the message and react with hourglass *)
      Queue.add { Session_store.msg; channel_info } session.pending_queue;
      ignore (Discord_rest.create_reaction t.rest
        ~channel_id:msg.Discord_types.channel_id
        ~message_id:msg.id ~emoji:"\xE2\x8F\xB3" ())
    end else begin
      session.processing <- true;
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        Fun.protect ~finally:(fun () ->
          session.processing <- false
        ) (fun () ->
          let rec process_message (msg : Discord_types.message) channel_info =
            let child_pid = ref None in
            Fun.protect ~finally:(fun () ->
              Option.iter (unregister_child_pid t) !child_pid
            ) (fun () ->
              let channel_id = msg.channel_id in
              let message_id = msg.id in
              ignore (Discord_rest.create_reaction t.rest ~channel_id
                ~message_id ~emoji:"\xF0\x9F\x91\x80" ());
              Channel_manager.bump ~rest:t.rest ~guild_id:t.config.guild_id
                ~project_name:session.project_name t.channels;
              let author_name = msg.author.username in
              let (channel_name, channel_type) =
                resolve_channel_context t ~channel_id ~session ?channel_info () in
              let on_pid pid =
                child_pid := Some pid;
                register_child_pid t pid;
                Logs.info (fun m -> m "bot: registered child pid %d" pid) in
              (* On the first message, prepend any initial context from the
                 session creator (e.g. the project channel agent that started
                 this thread). Consumed once, then cleared. *)
              let had_initial_prompt = Option.is_some session.initial_prompt in
              let prompt = match session.initial_prompt with
                | Some ctx ->
                  Printf.sprintf "<session-context>\n%s\n</session-context>\n\n%s"
                    ctx msg.content
                | None -> msg.content
              in
              let result = Agent_runner.run ~sw:t.sw ~env:t.env ~rest:t.rest
                      ~session ~channel_id ~prompt
                      ~attachments:msg.attachments
                      ~author_name ~channel_name ~channel_type
                      ~wrap_width:t.wrap_width ~on_pid () in
              ignore (Discord_rest.delete_own_reaction t.rest ~channel_id
                ~message_id ~emoji:"\xF0\x9F\x91\x80" ());
              (match result with
              | Ok () ->
                ignore (Discord_rest.create_reaction t.rest ~channel_id
                  ~message_id ~emoji:"\xE2\x9C\x85" ());
                (* Clear initial_prompt only after successful run so it
                   survives failures and retries. Cleared before
                   increment_message_count to persist in a single save. *)
                if had_initial_prompt then
                  session.initial_prompt <- None;
                Session_store.increment_message_count t.sessions session
              | Error _ ->
                ignore (Discord_rest.create_reaction t.rest ~channel_id
                  ~message_id ~emoji:"\xE2\x9D\x8C" ())));
            (* Drain the queue: process next pending message if any *)
            match Queue.take_opt session.pending_queue with
            | None -> ()
            | Some pending ->
              (* Remove hourglass, will get eyes when processing starts *)
              ignore (Discord_rest.delete_own_reaction t.rest
                ~channel_id:pending.msg.channel_id
                ~message_id:pending.msg.id ~emoji:"\xE2\x8F\xB3" ());
              process_message pending.msg pending.channel_info
          in
          process_message msg channel_info))
    end

(** Ensure a session exists for a channel (control or project channels).
    Creates a persistent Claude session so the channel can handle chat directly. *)
let ensure_channel_session t ~channel_id ~project_name ~working_dir ~system_prompt =
  match Session_store.find_opt t.sessions ~thread_id:channel_id with
  | Some _ -> ()
  | None ->
    let session : Session_store.session = {
      project_name; working_dir; agent_kind = Config.Claude;
      session_id = Resource.generate_uuid (); thread_id = channel_id;
      system_prompt; message_count = 0; processing = false;
      pending_queue = Queue.create (); initial_prompt = None;
    } in
    Session_store.add t.sessions ~thread_id:channel_id session;
    Logs.info (fun m -> m "bot: auto-created session for %s" project_name)

(** Route an incoming Discord message. *)
let handle_message t (msg : Discord_types.message) =
  Session_store.maybe_reload t.sessions;
  match msg.author.bot with Some true -> () | _ ->
  (* While draining, only allow read-only commands *)
  if t.draining then begin
    if Command.is_command msg.content then
      let cmd = Command.parse msg.content in
      match cmd with
      | Command.Status | Command.List_projects | Command.List_sessions
      | Command.List_claude_sessions | Command.Help ->
        handle_command t msg cmd
      | _ ->
        ignore (Discord_rest.create_message t.rest
          ~channel_id:msg.Discord_types.channel_id
          ~content:"Bot is restarting. Try again shortly." ())
    else
      ignore (Discord_rest.create_message t.rest
        ~channel_id:msg.Discord_types.channel_id
        ~content:"Bot is restarting. Try again shortly." ())
  end else
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
         Channel_manager.bump ~rest:t.rest ~guild_id:t.config.guild_id
           ~project_name:p.name t.channels;
         handle_thread_message t msg ()
       | None -> ())
    | None ->
      (* Check if this is a thread under a project channel.
         Since the control API creates sessions directly in bot memory,
         no disk reload is needed — sessions are always authoritative. *)
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
               (* Thread under a project channel with no session —
                  auto-create one (e.g. manually created in Discord). *)
               let proj = List.find_opt (fun (p : Project.t) ->
                 p.name = proj_name) t.projects in
               (match proj with
                | Some p ->
                  let wd = match working_dir_of_project p with
                    | Ok d -> d | Error _ -> p.path in
                  ensure_channel_session t ~channel_id:msg.channel_id
                    ~project_name:p.name ~working_dir:wd
                    ~system_prompt:None;
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
  let bot = { config; rest; gateway; projects; sessions; channels; env; sw;
               started_at = Unix.gettimeofday ();
               draining = false; child_pids = (ref Pid_set.empty, Mutex.create ());
               wrap_width = Agent_process.desktop_width } in
  bot.gateway.handler <- (fun event ->
    match event with
    | Discord_gateway.Connected user ->
      Logs.info (fun m -> m "bot: connected as %s" user.Discord_types.username);
      if bot.channels.category_id = None then
        Eio.Fiber.fork ~sw (fun () ->
          Channel_manager.setup ~rest ~guild_id:config.guild_id ~projects bot.channels;
          (* Reorder channels by activity. Primary: Discord message count from
             sessions. Fallback: last git commit timestamp, so projects without
             Discord activity still sort by recency. *)
          let discord_scores =
            let tbl = Hashtbl.create 32 in
            List.iter (fun (_tid, (s : Session_store.session)) ->
              let prev = try Hashtbl.find tbl s.project_name with Not_found -> 0 in
              Hashtbl.replace tbl s.project_name (prev + s.message_count)
            ) (Session_store.bindings bot.sessions);
            tbl in
          let git_timestamp project_path =
            Eio_unix.run_in_systhread (fun () ->
              try
                let ic = Unix.open_process_in
                  (Printf.sprintf "git -C %s log -1 --format=%%ct 2>/dev/null"
                    (Filename.quote project_path)) in
                let line = input_line ic in
                ignore (Unix.close_process_in ic);
                int_of_string line
              with _ -> 0) in
          (* Fetch git timestamps in parallel using Eio fibers *)
          let indexed = List.mapi (fun i p -> (i, p)) projects in
          let results = Array.make (List.length projects) ("", 0) in
          Eio.Fiber.List.iter (fun (i, (p : Project.t)) ->
            let discord = try Hashtbl.find discord_scores p.name with Not_found -> 0 in
            let git_ts = git_timestamp p.path in
            (* Score: discord messages * 1_000_000 + git timestamp.
               This ensures any Discord activity dominates, with git
               commit recency as tiebreaker for inactive projects. *)
            results.(i) <- (p.name, discord * 1_000_000 + git_ts)
          ) indexed;
          let activity = Array.to_list results
            |> List.sort (fun (_, s1) (_, s2) -> Int.compare s2 s1) in
          Channel_manager.reorder_by_activity ~rest ~guild_id:config.guild_id
            bot.channels activity;
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
