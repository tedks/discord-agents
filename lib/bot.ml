(** Top-level bot orchestrator — thin wiring layer.

    Routes Discord messages to the appropriate handler:
    - Commands (! prefix) → Command module → handlers
    - Control channel chat → default-agent session with MCP tools
    - Project channel chat → default-agent session scoped to project
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

(** Snapshot of project-related state. Projects and their channel mappings
    must always be consistent with each other, so they're bundled into a
    single immutable record and swapped atomically. This prevents any fiber
    from observing new projects with old channel mappings or vice versa. *)
type project_state = {
  projects : Project.t list;
  channels : Channel_manager.t;
}

(** A single output block, stored for !scroll access. *)
type output_block = {
  lines : string array;          (** Pre-split lines for fast windowing *)
  output_lines_used : int;       (** Line count at truncation time — paging uses this *)
  mutable next_line : int;       (** Next line to display (starts at output_lines_used) *)
}

(** Per-thread scroll state: a stack of truncated output blocks (most recent first)
    and which block is currently targeted by !scroll. *)
type scroll_state = {
  mutable blocks : output_block list;
  mutable current_block : int;  (** 1-indexed from most recent, set by !scroll N *)
}

type t = {
  config : Config.t;
  settings : Runtime_settings.t;
  rest : Discord_rest.t;
  gateway : Discord_gateway.t;
  mutable project_state : project_state;
  sessions : Session_store.t;
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  started_at : float;
  mutable draining : bool;
  child_pids : (Pid_set.t ref * Mutex.t);
  mutable wrap_width : int;
  mutable refreshing : bool;
  mutable output_lines : int;
  mutable policy_sync_clear_last_warning : (string * string) option;
  scroll_states : (Discord_types.channel_id, scroll_state) Hashtbl.t;
}

type top_level_policy_sync_state =
  | Policy_sync_clean
  | Policy_sync_rotation_pending
  | Policy_sync_marker_clear_pending

(** Convenience accessors for the current project state snapshot. *)
let projects t = t.project_state.projects
let channels t = t.project_state.channels
let default_agent t = t.settings.default_agent
let rescue_agent t = t.settings.rescue_agent
let policy_sync_pending t = t.settings.policy_sync_pending
let new_session_block_message () = Disk_health.new_session_block_message ()
let pending_default_rotation kind =
  Session_store.{ kind; origin = Default_rotation }
let pending_session_override kind =
  Session_store.{ kind; origin = Session_override }

let rescue_mode_active_from_snapshot (settings : Runtime_settings.t) disk =
  Disk_health.pressure disk && Option.is_some settings.rescue_agent

let effective_top_level_agent_from_snapshot (settings : Runtime_settings.t) disk =
  match settings.rescue_agent with
  | Some kind when Disk_health.pressure disk -> kind
  | _ -> settings.default_agent

let effective_top_level_agent t =
  effective_top_level_agent_from_snapshot t.settings (Disk_health.snapshot ())

let rescue_mode_active t =
  rescue_mode_active_from_snapshot t.settings (Disk_health.snapshot ())

let rescue_agent_notice t =
  let disk = Disk_health.snapshot () in
  match t.settings.rescue_agent with
  | Some kind when Disk_health.pressure disk ->
    Some (Printf.sprintf
      "Disk pressure is active, so new top-level sessions currently use rescue agent `%s`."
      (Config.string_of_agent_kind kind))
  | Some kind ->
    Some (Printf.sprintf "Rescue agent `%s` is inactive until disk pressure."
      (Config.string_of_agent_kind kind))
  | None ->
    None

let string_of_top_level_policy_sync_state = function
  | Policy_sync_clean -> "clean"
  | Policy_sync_rotation_pending -> "rotation-pending"
  | Policy_sync_marker_clear_pending -> "marker-clear-pending"

let reraise_if_fatal_policy_exception exn =
  match exn with
  | Eio.Cancel.Cancelled _ -> raise exn
  | Out_of_memory
  | Stack_overflow
  | Sys.Break
  | Assert_failure _
  | Match_failure _
  | Invalid_argument _ ->
    raise exn
  | _ -> ()

let refresh_disk_state () =
  ignore (Disk_health.preflight_state_mutation ())

let is_control_channel t ~(channel_id : Discord_types.channel_id) =
  match t.config.control_channel_id with
  | Some ctl_id -> channel_id = ctl_id
  | None -> false

let is_project_channel t ~(channel_id : Discord_types.channel_id) =
  Option.is_some (Channel_manager.project_for_channel (channels t) ~channel_id)

let is_persistent_channel t ~(channel_id : Discord_types.channel_id) =
  is_control_channel t ~channel_id || is_project_channel t ~channel_id

let is_known_project_name t project_name =
  List.exists (fun (project : Project.t) ->
    String.equal project.Project.name project_name) (projects t)

let is_persistent_session t ~thread_id (session : Session_store.session) =
  is_persistent_channel t ~channel_id:thread_id
  || (Option.is_some session.system_prompt
      && is_known_project_name t session.project_name)

let refresh_session_disk_state (session : Session_store.session) =
  ignore (Disk_health.preflight_write session.working_dir)

let refresh_persistent_session_disk_state t =
  Session_store.bindings t.sessions
  |> List.iter (fun (thread_id, (session : Session_store.session)) ->
    if is_persistent_session t ~thread_id session then
      refresh_session_disk_state session)

let refresh_top_level_disk_state t =
  refresh_disk_state ();
  refresh_persistent_session_disk_state t

let session_converged_to_top_level_policy expected_agent
    (session : Session_store.session) =
  match session.session_override_kind, session.pending_agent_change with
  | Some _, _
  | _, Some { origin = Session_store.Session_override; _ } ->
    true
  | None, Some { origin = Session_store.Default_rotation; _ } ->
    false
  | None, None ->
    Config.equal_agent_kind session.agent_kind expected_agent

let top_level_policy_state_converged_for_agent t expected_agent =
  Session_store.bindings t.sessions
  |> List.for_all (fun (thread_id, (session : Session_store.session)) ->
    not (is_persistent_channel t ~channel_id:thread_id)
    || session_converged_to_top_level_policy expected_agent session)

let rec top_level_policy_sync_state t =
  let expected_agent = effective_top_level_agent t in
  top_level_policy_sync_state_for_agent t expected_agent

and top_level_policy_sync_state_for_agent t expected_agent =
  if top_level_policy_state_converged_for_agent t expected_agent then
    if policy_sync_pending t then
      Policy_sync_marker_clear_pending
    else
      Policy_sync_clean
  else
    Policy_sync_rotation_pending

let top_level_policy_sync_state_from_snapshot t disk =
  let expected_agent = effective_top_level_agent_from_snapshot t.settings disk in
  top_level_policy_sync_state_for_agent t expected_agent

let note_policy_sync_clear_success t =
  t.policy_sync_clear_last_warning <- None

let should_log_policy_sync_clear_failure t err =
  let state =
    string_of_top_level_policy_sync_state (top_level_policy_sync_state t)
  in
  let warning = Some (state, err) in
  if t.policy_sync_clear_last_warning = warning then
    false
  else begin
    t.policy_sync_clear_last_warning <- warning;
    true
  end

type persistent_rotation = {
  reset_count : int;
  busy_count : int;
  current_busy_kind : Config.agent_kind option;
  current_override_kind : Config.agent_kind option;
}

type stop_outcome =
  | Session_not_found
  | Session_stopped of {
      project_name : string;
      dropped_count : int;
    }
  | Session_stopping of {
      project_name : string;
      had_running_process : bool;
      dropped_count : int;
    }
  | Session_already_stopping of { project_name : string }
  | Session_stop_failed of string

let fresh_session_like (session : Session_store.session)
    ~agent_kind ~session_override_kind =
  Session_store.make_session
    ~project_name:session.project_name
    ~working_dir:session.working_dir
    ~agent_kind
    ~session_override_kind
    ~session_id:(Resource.generate_uuid ())
    ~thread_id:session.thread_id
    ~system_prompt:session.system_prompt
    ~initial_prompt:None ()

(** Drop scroll-state entries for threads that no longer have an active
    session.  Called when sessions are removed wholesale (e.g. after
    Channel_manager.cleanup), since per-session removal goes through
    [Stop_session] which already prunes its own entry. *)
let prune_stale_scroll_states t =
  let stale = Hashtbl.fold (fun tid _ acc ->
    match Session_store.find_opt t.sessions ~thread_id:tid with
    | Some _ -> acc
    | None -> tid :: acc) t.scroll_states [] in
  List.iter (Hashtbl.remove t.scroll_states) stale;
  List.length stale

let register_child_pid t (session : Session_store.session) pid =
  let (pids, mu) = t.child_pids in
  Mutex.lock mu;
  pids := Pid_set.add pid !pids;
  session.child_pid <- Some pid;
  Mutex.unlock mu

let unregister_child_pid t (session : Session_store.session) pid =
  let (pids, mu) = t.child_pids in
  Mutex.lock mu;
  pids := Pid_set.remove pid !pids;
  if session.child_pid = Some pid then
    session.child_pid <- None;
  Mutex.unlock mu

let get_child_pids t =
  let (pids, mu) = t.child_pids in
  Mutex.lock mu;
  let result = !pids in
  Mutex.unlock mu;
  result

let child_pid_tracked t pid =
  let (pids, mu) = t.child_pids in
  Mutex.lock mu;
  let result = Pid_set.mem pid !pids in
  Mutex.unlock mu;
  result

let ppid_of_proc_stat_line line =
  match String.rindex_opt line ')' with
  | None -> None
  | Some close_idx ->
    let rest_start = close_idx + 2 in
    if rest_start >= String.length line then
      None
    else
      let rest =
        String.sub line rest_start (String.length line - rest_start)
        |> String.trim
      in
      match String.split_on_char ' ' rest |> List.filter ((<>) "") with
      | _state :: ppid :: _ -> int_of_string_opt ppid
      | _ -> None

type pid_ownership =
  | Direct_child
  | Not_direct_child
  | Unknown_child_ownership

let procfs_available () =
  try Sys.is_directory "/proc" with Sys_error _ -> false

let pid_ownership ?(tracked_child=false) pid =
  let stat_path = Printf.sprintf "/proc/%d/stat" pid in
  if not (Sys.file_exists stat_path) then
    (* Non-Linux hosts do not expose /proc. Falling back is safe only
       for PIDs still tracked from our own spawn path: a direct child
       PID cannot be reused by another process until it is reaped, and
       reaping unregisters it from the tracked set. *)
    if tracked_child && not (procfs_available ()) then Direct_child
    else Not_direct_child
  else
    match
      try Some (Resource.read_file stat_path) with _ -> None
    with
    | None -> Unknown_child_ownership
    | Some stat ->
      match ppid_of_proc_stat_line stat with
      | Some ppid ->
        if ppid = Unix.getpid () then Direct_child else Not_direct_child
      | None -> Unknown_child_ownership

let drop_queued_messages ?(mark_failed=true) ?(async_marking=false) t
    (session : Session_store.session) ~reason =
  let dropped = ref 0 in
  let pending_to_mark : Session_store.pending_message list ref = ref [] in
  while not (Queue.is_empty session.pending_queue) do
    let pending = Queue.pop session.pending_queue in
    incr dropped;
    if mark_failed then
      pending_to_mark := pending :: !pending_to_mark
  done;
  let mark_failed_messages () =
    List.rev !pending_to_mark |> List.iter (fun pending ->
      ignore (Discord_rest.delete_own_reaction t.rest
        ~channel_id:pending.Session_store.msg.Discord_types.channel_id
        ~message_id:pending.Session_store.msg.id ~emoji:"\xE2\x8F\xB3" ());
      ignore (Discord_rest.create_reaction t.rest
        ~channel_id:pending.Session_store.msg.Discord_types.channel_id
        ~message_id:pending.Session_store.msg.id ~emoji:"\xE2\x9D\x8C" ())
    )
  in
  if mark_failed && !pending_to_mark <> [] then
    if async_marking then
      Eio.Fiber.fork ~sw:t.sw mark_failed_messages
    else
      mark_failed_messages ();
  if !dropped > 0 then
    Logs.info (fun m ->
      m "bot: dropped %d queued message(s) for %s during %s"
        !dropped session.project_name reason);
  !dropped

let request_session_process_stop t (session : Session_store.session) =
  match session.child_pid with
  | None -> false
  | Some pid ->
    let signalled = Eio_unix.run_in_systhread (fun () ->
      match pid_ownership ~tracked_child:(child_pid_tracked t pid) pid with
      | Direct_child ->
        (try
           Unix.kill pid Sys.sigterm;
           true
         with Unix.Unix_error _ -> false)
      | Not_direct_child -> false
      | Unknown_child_ownership ->
        Logs.warn (fun m ->
          m "bot: skipping stop signal for pid %d; could not verify ownership"
            pid);
        false)
    in
    if signalled then
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        Eio.Time.sleep (Eio.Stdenv.clock t.env) 2.0;
        match session.child_pid with
        | Some live_pid when live_pid = pid ->
          Eio_unix.run_in_systhread (fun () ->
            match pid_ownership ~tracked_child:(child_pid_tracked t pid) pid with
            | Direct_child ->
              (try
                 Unix.kill pid 0;
                 Unix.kill pid Sys.sigkill
               with Unix.Unix_error _ -> ())
            | Not_direct_child -> ()
            | Unknown_child_ownership ->
              Logs.warn (fun m ->
                m "bot: skipping forced stop for pid %d; could not verify ownership"
                  pid))
        | _ -> ());
    signalled

let remove_session_now t (session : Session_store.session) =
  try
    Session_store.remove t.sessions ~thread_id:session.thread_id;
    Hashtbl.remove t.scroll_states session.thread_id;
    Ok ()
  with exn ->
    Error (Printexc.to_string exn)

let stop_session t ~thread_id =
  match Session_store.find_opt t.sessions ~thread_id with
  | None -> Session_not_found
  | Some session when session.stop_requested ->
    if session.processing then
      Session_already_stopping { project_name = session.project_name }
    else
      let dropped_count =
        if Queue.is_empty session.pending_queue then 0
        else drop_queued_messages ~async_marking:true t session ~reason:"session stop"
      in
      (match remove_session_now t session with
       | Ok () ->
         Session_stopped {
           project_name = session.project_name;
           dropped_count;
         }
       | Error err -> Session_stop_failed err)
  | Some session ->
    if session.processing then begin
      match Session_store.set_stop_requested t.sessions session true with
      | Error err -> Session_stop_failed err
      | Ok () ->
        let dropped_count =
          if Queue.is_empty session.pending_queue then 0
          else drop_queued_messages ~async_marking:true t session ~reason:"session stop"
        in
        let had_running_process = request_session_process_stop t session in
        Session_stopping {
          project_name = session.project_name;
          had_running_process;
          dropped_count;
        }
    end else
      let dropped_count =
        if Queue.is_empty session.pending_queue then 0
        else
          (* No runner will come back to reconcile these queued messages,
             so prefer deterministic cleanup over best-effort reaction updates. *)
          drop_queued_messages ~mark_failed:false t session ~reason:"session stop"
      in
      match remove_session_now t session with
      | Ok () ->
        Session_stopped {
          project_name = session.project_name;
          dropped_count;
        }
      | Error err -> Session_stop_failed err

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
- stop_session: Stop an active bot session by Discord thread ID
- list_claude_sessions: Find recent Claude Code sessions to resume
- list_codex_sessions: Find recent Codex CLI sessions to resume
- list_gemini_sessions: Find recent Gemini CLI sessions to resume
- resume_session: Resume an existing session (pass kind=claude, kind=codex, or kind=gemini to disambiguate; default tries the current effective top-level agent first)
- default_agent: Show or set the default agent used for new top-level sessions
- rescue_agent: Show or set the rescue agent automatically used for new top-level sessions under disk pressure
- restart_bot: Rebuild and restart the bot
- rename_thread: Rename a Discord thread
- cleanup_channels: Delete stale Discord channels
- refresh_projects: Re-scan for new projects without restarting

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

(** System prompt for the project overview session — scoped to one project. *)
let project_system_prompt (project : Project.t) =
  Printf.sprintf
"You are the project overview agent for **%s** (at `%s`).

You have MCP tools available:
- start_session: Start a new agent session (creates a thread + worktree)
- list_projects: List all discovered projects
- list_sessions: List active bot sessions
- stop_session: Stop an active bot session by Discord thread ID
- list_claude_sessions: Find recent Claude Code sessions to resume
- list_codex_sessions: Find recent Codex CLI sessions to resume
- list_gemini_sessions: Find recent Gemini CLI sessions to resume
- resume_session: Resume an existing session (pass kind=claude, kind=codex, or kind=gemini to disambiguate; default tries the current effective top-level agent first)
- default_agent: Show or set the default agent used for new top-level sessions
- rescue_agent: Show or set the rescue agent automatically used for new top-level sessions under disk pressure
- rename_thread: Rename a Discord thread
- restart_bot: Rebuild and restart the bot
- cleanup_channels: Delete stale Discord channels
- refresh_projects: Re-scan for new projects without restarting

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

let refreshed_system_prompt t (session : Session_store.session) =
  if is_control_channel t ~channel_id:session.thread_id then
    Some (control_system_prompt (projects t))
  else
    match Channel_manager.project_for_channel (channels t)
            ~channel_id:session.thread_id with
    | Some project_name ->
      (match List.find_opt (fun (p : Project.t) -> p.name = project_name)
               (projects t) with
       | Some project -> Some (project_system_prompt project)
       | None -> session.system_prompt)
    | None -> session.system_prompt

let replace_session_agent t (session : Session_store.session)
    ~agent_kind ~session_override_kind =
  let replacement = {
    (fresh_session_like session ~agent_kind ~session_override_kind) with
    system_prompt = refreshed_system_prompt t session;
  } in
  try
    Session_store.add t.sessions ~thread_id:session.thread_id replacement;
    Hashtbl.remove t.scroll_states session.thread_id;
    Ok ()
  with exn ->
    Error (Printexc.to_string exn)

let replacement_session_for_agent t (session : Session_store.session)
    ~agent_kind ~session_override_kind =
  let replacement = {
    (fresh_session_like session ~agent_kind ~session_override_kind) with
    system_prompt = refreshed_system_prompt t session;
  } in
  replacement

let align_persistent_sessions_to_agent
    ?(replacement_session=replacement_session_for_agent)
    t ~current_channel_id ~new_agent =
  let store : Session_store.t = t.sessions in
  let session_entries = Session_store.bindings store in
  let current_override_kind = ref None in
  (match current_channel_id with
   | Some thread_id ->
     (match Session_store.find_opt store ~thread_id with
      | Some session ->
        current_override_kind := session.session_override_kind
      | None -> ())
   | None -> ());
  let current_busy_kind = ref None in
  let reset_count = ref 0 in
  let busy_count = ref 0 in
  let changed = ref false in
  let new_sessions = ref store.sessions in
  let scroll_states_to_clear = ref [] in
  let rollback_actions = ref [] in
  let rollback () =
    List.iter (fun undo -> undo ()) !rollback_actions
  in
  let set_pending_agent_change
      (session : Session_store.session) pending_agent_change =
    if session.pending_agent_change <> pending_agent_change then begin
      let prior = session.pending_agent_change in
      rollback_actions :=
        (fun () -> session.pending_agent_change <- prior) :: !rollback_actions;
      session.pending_agent_change <- pending_agent_change;
      changed := true
    end
  in
  let prepared =
    try
      List.iter (fun (thread_id, (session : Session_store.session)) ->
        if is_persistent_session t ~thread_id session then begin
          if (Config.equal_agent_kind session.agent_kind new_agent
              || Option.is_some session.session_override_kind)
          then
            (match session.pending_agent_change with
             | Some { origin = Session_store.Default_rotation; _ } ->
               set_pending_agent_change session None
             | _ -> ());
          if Option.is_none session.session_override_kind
             && not (Config.equal_agent_kind session.agent_kind new_agent)
          then if session.processing || not (Queue.is_empty session.pending_queue) then begin
            match session.pending_agent_change with
            | Some { origin = Session_store.Session_override; kind } ->
              (match current_channel_id with
               | Some current_channel_id when thread_id = current_channel_id ->
                 current_override_kind := Some kind
               | _ -> ())
            | _ ->
              let target = pending_default_rotation new_agent in
              set_pending_agent_change session (Some target);
              (match current_channel_id with
               | Some current_channel_id when thread_id = current_channel_id ->
                 current_busy_kind := Some session.agent_kind
               | _ -> ());
              incr busy_count
          end else begin
            let replacement =
              replacement_session t session
                ~agent_kind:new_agent ~session_override_kind:None
            in
            new_sessions :=
              Session_store.SessionMap.add thread_id replacement !new_sessions;
            scroll_states_to_clear := thread_id :: !scroll_states_to_clear;
            changed := true;
            incr reset_count
          end
        end
      ) session_entries;
      Ok ()
    with exn ->
      reraise_if_fatal_policy_exception exn;
      rollback ();
      Error (Printf.sprintf
        "failed to prepare top-level session policy for `%s`: %s"
        (Config.string_of_agent_kind new_agent)
        (Printexc.to_string exn))
  in
  match prepared with
  | Error _ as err -> err
  | Ok () ->
    let rotation = {
      reset_count = !reset_count;
      busy_count = !busy_count;
      current_busy_kind = !current_busy_kind;
      current_override_kind = !current_override_kind;
    } in
    if not !changed then
      Ok rotation
    else
      match Session_store.replace_sessions_with
              ~rollback store !new_sessions with
      | Error err ->
        Error (Printf.sprintf
          "failed to persist top-level session policy for `%s`: %s"
          (Config.string_of_agent_kind new_agent) err)
      | Ok () ->
        List.iter (Hashtbl.remove t.scroll_states) !scroll_states_to_clear;
        Ok rotation

let clear_policy_sync_pending t =
  if policy_sync_pending t then
    match Runtime_settings.set_policy_sync_pending t.settings false with
    | Ok () ->
      note_policy_sync_clear_success t;
      Ok ()
    | Error _ as err ->
      err
  else
    Ok ()

let run_top_level_policy_change t ~current_channel_id
    ~default_agent ~rescue_agent ~new_agent ~summary ~policy_label =
  t.policy_sync_clear_last_warning <- None;
  let policy_changed =
    not (Config.equal_agent_kind t.settings.default_agent default_agent)
    || not (Option.equal Config.equal_agent_kind
              t.settings.rescue_agent rescue_agent)
  in
  let must_stage_pending = policy_changed || policy_sync_pending t in
  let stage_result =
    if must_stage_pending then
      Runtime_settings.set_top_level_policy t.settings
        ~default_agent ~rescue_agent ~policy_sync_pending:true
    else
      Ok ()
  in
  match stage_result with
  | Error err ->
    Error (Printf.sprintf
      "Failed to persist the pending %s policy state: %s"
      policy_label err)
  | Ok () ->
    (match align_persistent_sessions_to_agent t
             ~current_channel_id ~new_agent with
     | Error err ->
       Error (Printf.sprintf "%s, but top-level session rotation was incomplete: %s"
         summary err)
     | Ok rotation ->
       if must_stage_pending then
         match clear_policy_sync_pending t with
         | Ok () -> Ok rotation
         | Error err ->
           Error (Printf.sprintf
             "%s, and top-level sessions were updated, but the policy sync marker could not be cleared: %s"
             summary err)
       else
         Ok rotation)

let set_default_agent t ?(current_channel_id=None) kind =
  refresh_top_level_disk_state t;
  let simulated_settings : Runtime_settings.t = {
    t.settings with default_agent = kind;
  } in
  run_top_level_policy_change t ~current_channel_id
    ~default_agent:kind
    ~rescue_agent:t.settings.rescue_agent
    ~new_agent:(effective_top_level_agent_from_snapshot
      simulated_settings (Disk_health.snapshot ()))
    ~summary:(Printf.sprintf "Default agent is now `%s`"
      (Config.string_of_agent_kind kind))
    ~policy_label:"default-agent"

let set_rescue_agent t ?(current_channel_id=None) kind =
  refresh_top_level_disk_state t;
  let simulated_settings : Runtime_settings.t = {
    t.settings with rescue_agent = kind;
  } in
  let summary =
    match kind with
    | Some kind ->
      Printf.sprintf "Rescue agent is now `%s`"
        (Config.string_of_agent_kind kind)
    | None ->
      "Rescue agent is now disabled"
  in
  run_top_level_policy_change t ~current_channel_id
    ~default_agent:t.settings.default_agent
    ~rescue_agent:kind
    ~new_agent:(effective_top_level_agent_from_snapshot
      simulated_settings (Disk_health.snapshot ()))
    ~summary
    ~policy_label:"rescue-agent"

let maybe_apply_pending_session_agent_change t (session : Session_store.session) =
  match session.pending_agent_change with
  | None -> ()
  | Some pending ->
    let target_kind =
      match pending.origin with
      | Session_store.Session_override -> pending.kind
      | Session_store.Default_rotation ->
        (* Default rotations follow the live top-level policy. The persisted
           kind is only the scheduling-time target and can become stale when
           disk pressure changes before a busy session finishes. *)
        refresh_disk_state ();
        refresh_session_disk_state session;
        effective_top_level_agent t
    in
    if pending.origin = Session_store.Default_rotation
       && Option.is_some session.session_override_kind
    then
      (match Session_store.set_pending_agent_change t.sessions session None with
       | Ok () -> ()
       | Error err ->
         Logs.warn (fun m ->
           m "bot: failed to clear stale default-agent rotation for override %s: %s"
             session.thread_id err))
    else if Config.equal_agent_kind session.agent_kind target_kind
            && pending.origin = Session_store.Default_rotation
    then
      (match Session_store.set_pending_agent_change t.sessions session None with
       | Ok () -> ()
       | Error err ->
         Logs.warn (fun m ->
           m "bot: failed to clear pending agent change for %s: %s"
             session.thread_id err))
    else if Config.equal_agent_kind session.agent_kind pending.kind
            && pending.origin = Session_store.Session_override
    then
      (match Session_store.set_override_and_pending_agent_change t.sessions session
               ~session_override_kind:(Some pending.kind)
               ~pending_agent_change:None with
       | Ok () ->
         Logs.info (fun m ->
           m "bot: pinned channel %s to existing %s session after pending agent change"
             session.thread_id (Config.string_of_agent_kind pending.kind))
       | Error err ->
         Logs.warn (fun m ->
           m "bot: failed to pin existing session for %s: %s"
             session.thread_id err))
    else if not session.processing
            && Queue.is_empty session.pending_queue then begin
      let session_override_kind =
        match pending.origin with
        | Session_store.Session_override -> Some pending.kind
        | Session_store.Default_rotation -> None
      in
      match replace_session_agent t session
              ~agent_kind:target_kind
              ~session_override_kind with
      | Ok () ->
         Logs.info (fun m ->
           m "bot: switched channel %s to %s after pending agent change"
             session.thread_id (Config.string_of_agent_kind target_kind))
      | Error err ->
        Logs.warn (fun m ->
          m "bot: failed to switch channel %s to %s: %s"
            session.thread_id
            (Config.string_of_agent_kind target_kind)
            err)
    end

let finalize_session_run ?(notify_stopped=true) t
    (session : Session_store.session) =
  session.processing <- false;
  if session.stop_requested then begin
    ignore (drop_queued_messages ~async_marking:true t session ~reason:"session stop");
    (match remove_session_now t session with
     | Ok () ->
       if notify_stopped then
         ignore (Discord_rest.create_message t.rest
           ~channel_id:session.thread_id
           ~content:"Session stopped." ())
     | Error err ->
       Logs.warn (fun m ->
         m "bot: failed to remove stopped session %s: %s"
           session.thread_id err))
  end else
    maybe_apply_pending_session_agent_change t session

let reconcile_persisted_stop_requests t =
  Session_store.bindings t.sessions
  |> List.iter (fun (_thread_id, (session : Session_store.session)) ->
    if session.stop_requested then
      match remove_session_now t session with
      | Ok () ->
        Logs.info (fun m ->
          m "bot: removed persisted stopping session %s on startup"
            session.thread_id)
      | Error err ->
        Logs.warn (fun m ->
          m "bot: failed to remove persisted stopping session %s: %s"
            session.thread_id err))

let reconcile_persisted_pending_agent_changes t =
  refresh_top_level_disk_state t;
  Session_store.bindings t.sessions
  |> List.iter (fun (_thread_id, (session : Session_store.session)) ->
    match session.pending_agent_change with
    | Some { origin = Session_store.Session_override; _ } ->
      maybe_apply_pending_session_agent_change t session
    | Some { origin = Session_store.Default_rotation; _ }
    | None -> ());
  match align_persistent_sessions_to_agent t
          ~current_channel_id:None ~new_agent:(effective_top_level_agent t) with
  | Ok _ ->
    (match clear_policy_sync_pending t with
     | Ok () -> ()
     | Error err ->
       if should_log_policy_sync_clear_failure t err then
         Logs.warn (fun m ->
           m "bot: failed to clear top-level policy sync marker after reconcile: %s"
             err))
  | Error err ->
    Logs.warn (fun m ->
      m "bot: failed to reconcile top-level default agent sessions: %s" err)

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
        ignore (drop_queued_messages t s ~reason:"restart")
      ) (Session_store.bindings t.sessions);
      (* Phase 2: Reap child processes.
         Split SIGTERM and SIGKILL into separate systhread calls with
         Eio.Time.sleep between to avoid blocking the systhread pool. *)
      let pids = get_child_pids t in
      if not (Pid_set.is_empty pids) then begin
        notify (Printf.sprintf "Terminating %d child process(es)..." (Pid_set.cardinal pids));
        Eio_unix.run_in_systhread (fun () ->
          Pid_set.iter (fun pid ->
            match pid_ownership ~tracked_child:true pid with
            | Direct_child ->
              (try Unix.kill pid Sys.sigterm
               with Unix.Unix_error _ -> ())
            | Not_direct_child -> ()
            | Unknown_child_ownership ->
              Logs.warn (fun m ->
                m "bot: skipping restart SIGTERM for pid %d; could not verify ownership"
                  pid)
          ) pids);
        Eio.Time.sleep (Eio.Stdenv.clock t.env) 2.0;
        Eio_unix.run_in_systhread (fun () ->
          Pid_set.iter (fun pid ->
            match pid_ownership ~tracked_child:true pid with
            | Direct_child ->
              (try Unix.kill pid 0; Unix.kill pid Sys.sigkill
               with Unix.Unix_error _ -> ())
            | Not_direct_child -> ()
            | Unknown_child_ownership ->
              Logs.warn (fun m ->
                m "bot: skipping restart SIGKILL for pid %d; could not verify ownership"
                  pid)
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
        t.gateway.shutdown <- true;
        (match t.gateway.ws with
         | Some ws ->
           (try Websocket.send_close ws with exn ->
              Websocket.raise_if_cancelled exn)
         | None -> ());
        notify "Build succeeded. New instance starting — shutting down in 30s.";
        (* Wait for new instance to take over, then exit. *)
        Eio.Time.sleep (Eio.Stdenv.clock t.env) 30.0;
        Logs.info (fun m -> m "bot: restart handoff timeout, exiting");
        exit 0))
  end

(** Re-run project discovery and update all derived state atomically.
    Separates blocking I/O (filesystem/git scanning) from Eio I/O
    (Discord REST for channel setup). Must be called from an Eio fiber.

    Builds a complete new project_state snapshot (projects + channel map)
    and swaps it in one assignment. No fiber can observe an intermediate
    state where projects and channels disagree.

    Returns (old_count, new_count), or None if a refresh is already
    in progress (single-flight: concurrent callers are rejected). *)
let refresh_projects t =
  if t.refreshing then begin
    Logs.info (fun m -> m "bot: refresh already in progress, skipping");
    None
  end else begin
    t.refreshing <- true;
    Fun.protect ~finally:(fun () -> t.refreshing <- false) (fun () ->
      let old_count = List.length (projects t) in
      (* Phase 1: Blocking filesystem scan — runs in a system thread
         to avoid stalling the Eio event loop *)
      let new_projects = Eio_unix.run_in_systhread (fun () ->
        Project.discover ~base_directories:t.config.base_directories) in
      (* Phase 2: Build new channel mappings in a fresh manager.
         Must run in Eio context because Channel_manager.setup uses
         Discord REST (cohttp-eio). The old project_state stays valid
         for any concurrent message handling during this call. *)
      let new_channels = Channel_manager.create () in
      Channel_manager.set_category_id new_channels
        (Channel_manager.category_id (channels t));
      Channel_manager.setup ~rest:t.rest ~guild_id:t.config.guild_id
        ~projects:new_projects new_channels;
      (* Phase 3: Atomic swap — single assignment, no intermediate state *)
      t.project_state <- { projects = new_projects; channels = new_channels };
      let new_count = List.length new_projects in
      Logs.info (fun m -> m "bot: refreshed projects: %d -> %d" old_count new_count);
      Some (old_count, new_count))
  end

(** Render a "recent sessions" Discord listing in a uniform shape so
    [!claude-sessions] and [!gemini-sessions] don't drift in formatting.
    Caller maps their per-agent info record to a tuple of the four
    fields actually used and supplies the human label + resume hint. *)
let format_session_listing
    ~label ~resume_hint
    (entries : (string * string * string * float) list) =
  if entries = [] then
    Printf.sprintf "No recent %s sessions found." label
  else
    let now = Unix.gettimeofday () in
    let lines = List.map (fun (sid, wd, summary, mtime) ->
      let age_min = int_of_float ((now -. mtime) /. 60.0) in
      let age_str =
        if age_min < 60 then Printf.sprintf "%dm ago" age_min
        else Printf.sprintf "%dh ago" (age_min / 60)
      in
      let sid_short = Resource.short_id sid in
      (* Single-line both fields: a literal newline in [summary]
         (Codex/Gemini prompts are often multi-paragraph) would land
         the rest of the entry at column 0, where Discord parses it
         as a sibling top-level bullet. Working dirs don't normally
         contain newlines but defensive sanitization is free. *)
      let wd_str =
        if wd = "" then "(unknown project)"
        else Resource.single_line wd
      in
      let summary_str =
        if summary = "" then "(no summary)"
        else Resource.single_line summary
      in
      Printf.sprintf "- `%s` %s\n  %s — *%s*"
        sid_short age_str wd_str summary_str
    ) entries in
    Printf.sprintf
      "**Recent %s sessions** (last 24h):\n%s\n\nUse `%s` to attach."
      label (String.concat "\n" lines) resume_hint

(** Build the user-facing "session not found" error for the Resume
    handlers (Discord command, MCP tool). All three agents now have
    discoverable session stores, so the message is uniform. *)
let resume_not_found_message ~kind ~sid_prefix =
  match kind with
  | Some k ->
    Printf.sprintf "No %s session matching %S."
      (Config.string_of_agent_kind k) sid_prefix
  | None ->
    Printf.sprintf "No session matching %S." sid_prefix

(** Routing data needed to attach a resumed session to a Discord
    thread. Computed once per Resume invocation, used by both the
    Discord command handler and the MCP control_api handler. *)
type resume_target = {
  thread_parent : Discord_types.channel_id;
  working_dir : string;
  project_name : string;
}

(** Given the working_dir reported by disk discovery, pick the right
    Discord parent channel and resolve any path nuances (bare-repo
    pointer → master worktree, missing project → kind-named
    placeholder). [fallback_channel] is used when no project channel
    matches — typically the channel where the resume was invoked
    (Discord) or the bot's control channel (MCP). *)
let resolve_resume_target t ~raw_working_dir ~kind_label ~fallback_channel =
  let matched_project = List.find_opt (fun (p : Project.t) ->
    raw_working_dir = p.path
    || (String.length raw_working_dir > String.length p.path + 1
        && String.sub raw_working_dir 0 (String.length p.path + 1)
           = p.path ^ "/")
  ) (projects t) in
  let thread_parent = match matched_project with
    | Some p ->
      (match Channel_manager.find_or_create ~rest:t.rest
               ~guild_id:t.config.guild_id ~project:p (channels t) with
       | Some ch_id -> ch_id
       | None -> fallback_channel)
    | None -> fallback_channel
  in
  let working_dir = match matched_project with
    | Some p when p.is_bare && raw_working_dir = p.path ->
      (match working_dir_of_project p with
       | Ok wd -> wd | Error _ -> raw_working_dir)
    | _ -> raw_working_dir
  in
  let project_name = match matched_project with
    | Some p -> p.name
    | None ->
      if raw_working_dir = "" then kind_label ^ "-session"
      else Filename.basename raw_working_dir
  in
  { thread_parent; working_dir; project_name }

(** Create a fresh persistent channel session with an explicit agent kind. *)
let create_persistent_channel_session t ~channel_id ~project_name ~working_dir
    ~system_prompt ~agent_kind ~session_override_kind =
  let session = Session_store.make_session
    ~project_name ~working_dir ~agent_kind
    ~session_override_kind
    ~session_id:(Resource.generate_uuid ())
    ~thread_id:channel_id
    ~system_prompt ~initial_prompt:None () in
  Session_store.add t.sessions ~thread_id:channel_id session

(** Ensure or create a persistent top-level channel session for an
    explicit agent. Returns [true] if the channel context was known and
    a session was created. *)
let create_explicit_channel_session t ~channel_id ~agent_kind =
  if is_control_channel t ~channel_id then begin
    create_persistent_channel_session t ~channel_id
      ~project_name:"control" ~working_dir:(Sys.getcwd ())
      ~system_prompt:(Some (control_system_prompt (projects t)))
      ~agent_kind
      ~session_override_kind:(Some agent_kind);
    true
  end else
    match Channel_manager.project_for_channel (channels t) ~channel_id with
    | None -> false
    | Some proj_name ->
      match List.find_opt (fun (p : Project.t) -> p.name = proj_name) (projects t) with
      | None -> false
      | Some p ->
        let working_dir =
          match working_dir_of_project p with
          | Ok d -> d
          | Error _ -> p.path
        in
        create_persistent_channel_session t ~channel_id
          ~project_name:p.name ~working_dir
          ~system_prompt:(Some (project_system_prompt p))
          ~agent_kind
          ~session_override_kind:(Some agent_kind);
        true

let cleanup_orphan_thread t ~thread_id ~context =
  match Discord_rest.delete_channel t.rest ~channel_id:thread_id () with
  | Ok _ -> ()
  | Error err ->
    Logs.warn (fun m ->
      m "bot: failed to clean up orphan thread %s after %s: %s"
        thread_id context err)

let cleanup_orphan_worktree ~project ~branch_info ~working_dir ~context =
  match branch_info with
  | None -> ()
  | Some branch_name ->
    (match Project.remove_worktree project ~branch_name
             ~worktree_path:working_dir with
     | Ok () -> ()
     | Error err ->
       Logs.warn (fun m ->
         m "bot: failed to clean up orphan worktree %s after %s: %s"
           working_dir context err))

let remove_persisted_session_for_cleanup t ~thread_id ~context =
  match Session_store.find_opt t.sessions ~thread_id with
  | None -> true
  | Some _ ->
    try
      Session_store.remove t.sessions ~thread_id;
      true
    with exn ->
      Logs.warn (fun m ->
        m "bot: failed to remove persisted session %s during %s cleanup: %s"
          thread_id context (Printexc.to_string exn));
      false

let cleanup_start_artifacts t ~project ~branch_info ~working_dir
    ~thread_id ~session_persisted ~context =
  let session_removed =
    (not session_persisted)
    || remove_persisted_session_for_cleanup t ~thread_id ~context
  in
  if session_removed then begin
    cleanup_orphan_thread t ~thread_id ~context;
    cleanup_orphan_worktree ~project ~branch_info ~working_dir ~context
  end

let cleanup_thread_session_artifacts t ~thread_id ~session_persisted ~context =
  let session_removed =
    (not session_persisted)
    || remove_persisted_session_for_cleanup t ~thread_id ~context
  in
  if session_removed then
    cleanup_orphan_thread t ~thread_id ~context

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
    ) (projects t) in
    reply (if lines = [] then "No projects found."
      else "**Projects** (use `!start <name>` or `!start <number>`):\n"
           ^ String.concat "\n" lines)
  | Command.List_sessions ->
    let entries = Session_store.bindings t.sessions in
    let lines = List.map (fun (_tid, (s : Session_store.session)) ->
      Printf.sprintf "- **%s** / %s — %d messages (thread: <#%s>)"
        (Resource.single_line s.project_name)
        (Config.string_of_agent_kind s.agent_kind)
        s.message_count s.thread_id
    ) entries in
    reply (if lines = [] then "No active sessions."
      else "**Sessions:**\n" ^ String.concat "\n" lines)
  | Command.List_claude_sessions ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let entries =
        Claude_sessions.discover ~hours:24 ()
        |> List.filteri (fun i _ -> i < 10)
        |> List.map (fun (s : Claude_sessions.info) ->
          (s.session_id, s.working_dir, s.summary, s.mtime))
      in
      reply (format_session_listing ~label:"Claude"
        ~resume_hint:"!resume <session_id_prefix>" entries))
  | Command.Start_agent { project; kind } ->
    (match new_session_block_message () with
     | Some msg ->
       reply msg
     | None ->
       let kind = Option.value kind ~default:(effective_top_level_agent t) in
       let proj = Command.find_project_fuzzy (projects t) project in
       match proj with
       | None ->
       let q = String.lowercase_ascii project in
       let suggestions = List.filter (fun (p : Project.t) ->
         let name = String.lowercase_ascii p.name in
         let rec has i = if i + String.length q > String.length name then false
           else if String.sub name i (String.length q) = q then true
           else has (i + 1) in has 0
       ) (projects t) in
       (* Sanitize the user-supplied [project] before echoing it back:
          Discord allows multi-line messages, so [project] can contain
          a literal newline (Command.parse only splits on spaces). An
          unsanitized echo would let the user inject markdown into
          our error replies. *)
       let project_safe = Resource.single_line project in
       (match suggestions with
        | [] -> reply (Printf.sprintf "No project matching `%s`. Try `!projects`." project_safe)
        | _ -> reply (Printf.sprintf "No unique match for `%s`. Did you mean:\n%s" project_safe
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
                   ~guild_id:t.config.guild_id ~project:p (channels t) with
           | Some ch_id -> ch_id
           | None -> channel_id
         in
         match Discord_rest.create_thread_no_message t.rest
                 ~channel_id:thread_parent
                 ~name:(Printf.sprintf "%s / %s" kind_str p.name) () with
         | Error e ->
           cleanup_orphan_worktree ~project:p ~branch_info ~working_dir
             ~context:"thread creation failure";
           reply (Printf.sprintf "Failed to create thread: %s" e)
         | Ok thread_ch ->
           let session = Session_store.make_session
             ~project_name:p.name ~working_dir ~agent_kind:kind
             ~session_id:(Resource.generate_uuid ())
             ~thread_id:thread_ch.Discord_types.id
             ~system_prompt:None ~initial_prompt:None () in
           let session_persisted = ref false in
           (try
              Session_store.add t.sessions ~thread_id:thread_ch.id session;
              session_persisted := true;
              let branch_str = match branch_info with
                | Some b -> Printf.sprintf "\nBranch: `%s`" b | None -> "" in
              ignore (Discord_rest.create_message t.rest ~channel_id:thread_ch.id
                ~content:(Printf.sprintf
                  "**%s** session started for **%s**%s\nWorking in: `%s`\nSend a message to interact."
                  kind_str p.name branch_str working_dir) ())
            with exn ->
              cleanup_start_artifacts t ~project:p ~branch_info ~working_dir
                ~thread_id:thread_ch.id
                ~session_persisted:!session_persisted
                ~context:"session startup failure";
              reply (Printf.sprintf "Failed to persist session: %s"
                (Printexc.to_string exn)))
       end)
  | Command.List_codex_sessions ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let entries =
        Codex_sessions.discover ~hours:24 ()
        |> List.filteri (fun i _ -> i < 10)
        |> List.map (fun (s : Codex_sessions.info) ->
          (s.session_id, s.working_dir, s.summary, s.mtime))
      in
      reply (format_session_listing ~label:"Codex"
        ~resume_hint:"!resume codex <session_id_prefix>" entries))
  | Command.List_gemini_sessions ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let entries =
        Gemini_sessions.discover ~hours:24 ()
        |> List.filteri (fun i _ -> i < 10)
        |> List.map (fun (s : Gemini_sessions.info) ->
          (s.session_id, s.working_dir, s.summary, s.mtime))
      in
      reply (format_session_listing ~label:"Gemini"
        ~resume_hint:"!resume gemini <session_id_prefix>" entries))
  | Command.Resume_session { session_id; kind } ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      match new_session_block_message () with
      | Some msg ->
        reply msg
      | None ->
      (* Locate the session in the requested store, or try the current
         effective top-level agent first if [kind] is unspecified. *)
      let try_claude () =
        match Claude_sessions.find_by_prefix session_id with
        | Some (sid, wd) -> Some (Config.Claude, sid, wd)
        | None -> None
      in
      let try_codex () =
        match Codex_sessions.find_by_prefix session_id with
        | Some (sid, wd) -> Some (Config.Codex, sid, wd)
        | None -> None
      in
      let try_gemini () =
        match Gemini_sessions.find_by_prefix session_id with
        | Some (sid, wd) -> Some (Config.Gemini, sid, wd)
        | None -> None
      in
      let try_kind = function
        | Config.Claude -> try_claude ()
        | Config.Codex -> try_codex ()
        | Config.Gemini -> try_gemini ()
      in
      (* Explicit kind hits exactly one store; otherwise try the current
         effective top-level agent first, then the remaining stores. *)
      let found = match kind with
        | Some k -> try_kind k
        | None -> Config.find_with_preferred_agent (effective_top_level_agent t) try_kind
      in
      match found with
      | None ->
        reply (resume_not_found_message ~kind ~sid_prefix:session_id)
      | Some (_, full_sid, "") ->
        (* Gemini sessions whose projectHash isn't in
           ~/.gemini/projects.json come back with working_dir = "".
           Spawning a child with an empty cwd would write
           [.gemini/settings.json] and [.git/info/exclude] in the
           bot's own directory, polluting unrelated state. Reject
           with a clear message; the session is still discoverable
           via [!gemini-sessions], just not resumable. *)
        reply (Printf.sprintf
          "Cannot resume session `%s`: its working directory could \
           not be resolved. Start a fresh session with \
           `!start <project>` instead."
          (Resource.short_id full_sid))
      | Some (found_kind, full_sid, raw_working_dir) ->
        let kind_label = Config.string_of_agent_kind found_kind in
        let kind_title = String.capitalize_ascii kind_label in
        let sid_short = Resource.short_id full_sid in
        let { thread_parent; working_dir; project_name } =
          resolve_resume_target t
            ~raw_working_dir ~kind_label ~fallback_channel:channel_id
        in
        (match Discord_rest.create_thread_no_message t.rest
                ~channel_id:thread_parent
                ~name:(Printf.sprintf "resume %s / %s" kind_label sid_short) () with
        | Error e -> reply (Printf.sprintf "Failed to create thread: %s" e)
        | Ok thread_ch ->
          (* session_id_confirmed:true is critical here: we resolved
             [full_sid] from disk discovery, so it's a real id the
             agent will recognize. Without this override the
             make_session default for Gemini (caller_pinned=false)
             would mark the session unconfirmed and the next turn's
             gemini_args would omit --resume, starting a fresh chat. *)
          let session = Session_store.make_session
            ~project_name ~working_dir ~agent_kind:found_kind
            ~session_id:full_sid ~session_id_confirmed:true
            ~message_count:1
            ~thread_id:thread_ch.Discord_types.id
            ~system_prompt:None ~initial_prompt:None () in
          let session_persisted = ref false in
          (try
             Session_store.add t.sessions ~thread_id:thread_ch.id session;
             session_persisted := true;
             ignore (Discord_rest.create_message t.rest ~channel_id:thread_ch.id
               ~content:(Printf.sprintf
                 "**Resumed** %s session `%s`\nWorking in: `%s`\nSend a message to continue."
                 kind_title sid_short working_dir) ())
           with exn ->
             cleanup_thread_session_artifacts t ~thread_id:thread_ch.id
               ~session_persisted:!session_persisted
               ~context:"resume startup failure";
             reply (Printf.sprintf "Failed to persist resumed session: %s"
               (Printexc.to_string exn)))))
  | Command.Stop_session { thread_id } ->
    (match stop_session t ~thread_id with
     | Session_not_found ->
       reply "Session not found."
    | Session_stopped { project_name; dropped_count } ->
      let dropped_text =
        if dropped_count = 0 then ""
        else Printf.sprintf " Dropped %d queued message%s."
          dropped_count (if dropped_count = 1 then "" else "s")
      in
      reply (Printf.sprintf "Stopped session for **%s**.%s"
        project_name dropped_text)
     | Session_stopping { project_name; had_running_process; dropped_count } ->
      let process_text =
        if had_running_process then
          " Terminating the active agent process."
        else
          " The active session will stop as soon as its current turn or agent startup finishes."
       in
       let dropped_text =
         if dropped_count = 0 then ""
         else Printf.sprintf " Dropped %d queued message%s."
           dropped_count (if dropped_count = 1 then "" else "s")
       in
       reply (Printf.sprintf
         "Stopping session for **%s**.%s%s"
         project_name process_text dropped_text)
     | Session_already_stopping { project_name } ->
       reply (Printf.sprintf "Session for **%s** is already stopping." project_name)
     | Session_stop_failed err ->
       reply (Printf.sprintf "Failed to stop session: %s" err))
  | Command.Default_agent None ->
    refresh_disk_state ();
    let base = Printf.sprintf "Default agent: `%s`."
      (Config.string_of_agent_kind (default_agent t)) in
    reply (match rescue_agent_notice t with
      | Some notice -> base ^ " " ^ notice
      | None -> base)
  | Command.Default_agent (Some kind) ->
    let kind_str = Config.string_of_agent_kind kind in
    (match set_default_agent t ~current_channel_id:(Some channel_id) kind with
     | Error err -> reply err
     | Ok rotation ->
       let reset_suffix =
         if rotation.reset_count = 0 then ""
         else
           Printf.sprintf " Reset %d top-level session%s immediately."
             rotation.reset_count
             (if rotation.reset_count = 1 then "" else "s")
       in
       let busy_suffix =
         match rotation.current_override_kind, rotation.current_busy_kind with
         | Some override_kind, _ ->
           let others =
             if rotation.busy_count = 0 then ""
             else
               Printf.sprintf
                 " %d other busy top-level session%s will switch after their queued work finishes."
                 rotation.busy_count
                 (if rotation.busy_count = 1 then "" else "s")
           in
           Printf.sprintf
             " This channel keeps its session override on `%s`.%s"
             (Config.string_of_agent_kind override_kind) others
         | None, Some current_kind ->
           let other_busy = rotation.busy_count - 1 in
           let others =
             if other_busy <= 0 then ""
             else
               Printf.sprintf " %d other busy top-level session%s will switch after their queued work finishes."
                 other_busy
                 (if other_busy = 1 then "" else "s")
           in
           Printf.sprintf
             " This channel is still running `%s`; it will follow the effective top-level agent after its queued work finishes.%s"
             (Config.string_of_agent_kind current_kind) others
         | None, None ->
           if rotation.busy_count = 0 then ""
           else
             Printf.sprintf " %d busy top-level session%s will switch after their queued work finishes."
               rotation.busy_count
               (if rotation.busy_count = 1 then "" else "s")
      in
       let rescue_suffix =
         match rescue_agent_notice t with
         | Some notice when not (Config.equal_agent_kind (effective_top_level_agent t) kind) ->
           " " ^ notice
         | _ -> ""
       in
       reply (Printf.sprintf "Default agent set to `%s`.%s%s%s"
         kind_str reset_suffix busy_suffix rescue_suffix))
  | Command.Rescue_agent None ->
    refresh_disk_state ();
    (match rescue_agent t with
     | Some kind ->
       let base = Printf.sprintf "Rescue agent: `%s`."
         (Config.string_of_agent_kind kind) in
       reply (match rescue_agent_notice t with
         | Some notice -> base ^ " " ^ notice
         | None -> base)
     | None ->
       reply "Rescue agent: disabled.")
  | Command.Rescue_agent (Some requested) ->
    let requested_label = match requested with
      | Some kind -> Printf.sprintf "`%s`" (Config.string_of_agent_kind kind)
      | None -> "disabled"
    in
    let rescue_was_active = rescue_mode_active t in
    (match set_rescue_agent t ~current_channel_id:(Some channel_id) requested with
     | Error err -> reply err
     | Ok rotation ->
       let reset_suffix =
         if rotation.reset_count = 0 then ""
         else
           Printf.sprintf " Reset %d top-level session%s immediately."
             rotation.reset_count
             (if rotation.reset_count = 1 then "" else "s")
       in
       let busy_suffix =
         if rotation.busy_count = 0 then ""
         else
           Printf.sprintf " %d busy top-level session%s will switch after their queued work finishes."
             rotation.busy_count
             (if rotation.busy_count = 1 then "" else "s")
       in
       let effective_suffix =
         let effective = effective_top_level_agent t in
         match requested with
         | Some _ when rescue_mode_active t ->
           Printf.sprintf
             " Disk pressure is active, so top-level sessions are currently using `%s`."
             (Config.string_of_agent_kind effective)
         | None when rescue_was_active ->
           Printf.sprintf
             " Disk pressure is active, so top-level sessions are currently using the default agent `%s`."
             (Config.string_of_agent_kind effective)
         | _ -> ""
       in
       reply (Printf.sprintf "Rescue agent set to %s.%s%s%s"
         requested_label reset_suffix busy_suffix effective_suffix))
  | Command.Session_agent None ->
    (match Session_store.find_opt t.sessions ~thread_id:channel_id with
     | Some session ->
       (match session.pending_agent_change with
        | Some { origin = Session_store.Session_override; kind } ->
          reply (Printf.sprintf
            "Session agent: `%s` (will start a fresh `%s` session after queued work finishes)."
            (Config.string_of_agent_kind session.agent_kind)
            (Config.string_of_agent_kind kind))
        | _ when Option.is_some session.session_override_kind ->
          reply (Printf.sprintf "Session agent: `%s` (session override)."
            (Config.string_of_agent_kind session.agent_kind))
        | _ ->
          reply (Printf.sprintf "Session agent: `%s`."
            (Config.string_of_agent_kind session.agent_kind)))
     | None when is_persistent_channel t ~channel_id ->
       reply (Printf.sprintf
         "No session exists in this channel yet. It will start with top-level agent `%s`."
         (Config.string_of_agent_kind (effective_top_level_agent t)))
     | None ->
       reply "No session exists in this channel.")
  | Command.Session_agent (Some kind) ->
    let kind_str = Config.string_of_agent_kind kind in
    (match Session_store.find_opt t.sessions ~thread_id:channel_id with
     | Some session when Config.equal_agent_kind session.agent_kind kind
                         && Option.is_none session.pending_agent_change ->
       (match session.session_override_kind with
        | Some override_kind when Config.equal_agent_kind override_kind kind ->
          reply (Printf.sprintf "Session agent is already `%s`." kind_str)
        | _ ->
          (match Session_store.set_session_override_kind t.sessions session
                   (Some kind) with
           | Ok () ->
             reply (Printf.sprintf "Session agent pinned to `%s`." kind_str)
           | Error err ->
             reply (Printf.sprintf "Failed to persist session agent pin: %s"
               err)))
     | Some session when session.processing || not (Queue.is_empty session.pending_queue) ->
       (match Session_store.set_pending_agent_change t.sessions session
                (Some (pending_session_override kind)) with
        | Ok () ->
          reply (Printf.sprintf
            "A fresh `%s` session will start for this channel after queued work finishes."
            kind_str)
        | Error err ->
          reply (Printf.sprintf "Failed to persist session agent change: %s" err))
     | Some session ->
       (match replace_session_agent t session ~agent_kind:kind
                ~session_override_kind:(Some kind) with
        | Ok () ->
          reply (Printf.sprintf "Started a fresh `%s` session for this channel."
            kind_str)
        | Error err ->
          reply (Printf.sprintf
            "Failed to start a fresh `%s` session for this channel: %s"
            kind_str err))
     | None ->
       (match new_session_block_message () with
        | Some msg ->
          reply msg
        | None when create_explicit_channel_session t ~channel_id ~agent_kind:kind ->
          reply (Printf.sprintf "Started a fresh `%s` session for this channel."
            kind_str)
        | None ->
          reply "No session exists in this channel. Use `!start` or `!resume`."
       ))
  | Command.Cleanup_channels ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      match Channel_manager.cleanup ~rest:t.rest
              ~guild_id:t.config.guild_id ~projects:(projects t) (channels t) with
      | Error e -> reply (Printf.sprintf "Cleanup failed: %s" e)
      | Ok 0 ->
        let pruned = prune_stale_scroll_states t in
        reply (Printf.sprintf "No stale channels to clean up.%s"
          (if pruned > 0
           then Printf.sprintf " Pruned %d orphaned scroll state(s)." pruned
           else ""))
      | Ok n ->
        let pruned = prune_stale_scroll_states t in
        reply (Printf.sprintf "Cleaned up %d stale channels.%s" n
          (if pruned > 0
           then Printf.sprintf " Pruned %d orphaned scroll state(s)." pruned
           else "")))
  | Command.Restart ->
    trigger_restart t ~notify:reply
  | Command.Refresh ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      match refresh_projects t with
      | None ->
        reply "Refresh already in progress."
      | Some (old_count, new_count) ->
        let delta = new_count - old_count in
        if delta > 0 then
          reply (Printf.sprintf "Refreshed: found %d new project%s (%d total)."
            delta (if delta = 1 then "" else "s") new_count)
        else if delta < 0 then
          reply (Printf.sprintf "Refreshed: %d project%s removed (%d total)."
            (abs delta) (if abs delta = 1 then "" else "s") new_count)
        else
          reply (Printf.sprintf "Refreshed: no changes (%d total)." new_count)
      | exception exn ->
        Logs.warn (fun m -> m "bot: refresh failed: %s" (Printexc.to_string exn));
        reply (Printf.sprintf "Refresh failed: %s" (Printexc.to_string exn)))
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
        ignore (Disk_health.preflight_state_mutation ());
        refresh_persistent_session_disk_state t;
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
          Printf.sprintf "Default agent: %s"
            (Config.string_of_agent_kind (default_agent t));
          (match rescue_agent t with
           | Some kind ->
             Printf.sprintf "Rescue agent: %s%s"
               (Config.string_of_agent_kind kind)
               (if rescue_mode_active t then " (active)" else "")
           | None -> "Rescue agent: disabled");
          Printf.sprintf "Effective top-level agent: %s"
            (Config.string_of_agent_kind (effective_top_level_agent t));
          Printf.sprintf "Top-level policy sync: %s"
            (string_of_top_level_policy_sync_state
              (top_level_policy_sync_state t));
          Disk_health.status_summary ();
          Printf.sprintf "Sessions: %d (%d processing)"
            (Session_store.count t.sessions)
            (List.length (List.filter (fun (_, (s : Session_store.session)) ->
              s.processing) (Session_store.bindings t.sessions)));
          Printf.sprintf "Projects: %d  |  Channels: %d"
            (List.length (projects t)) (Channel_manager.count (channels t));
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
      "`!codex-sessions` — list recent Codex sessions";
      "`!gemini-sessions` — list recent Gemini sessions";
      "`!start <project> [agent]` — start a session (defaults to the current effective top-level agent)";
      "`!default-agent [agent]` / `!default_agent [agent]` — show or set the default agent (claude|codex|gemini)";
      "`!rescue-agent [agent|off]` / `!rescue_agent [agent|off]` — show or set the rescue agent used under disk pressure";
      "`!session-agent [agent]` / `!session_agent [agent]` — show or set the current channel session agent";
      "`!resume [agent] <session_id>` — resume a session (no agent = try the current effective top-level agent first)";
      "`!stop <thread_id>` — stop a session";
      "`!rename [thread_id] <name>` — rename a thread";
      "`!status` — bot status and running processes";
      "`!refresh` — re-scan for new projects";
      "`!cleanup` — delete stale channels";
      "`!restart` — rebuild and restart (warns but doesn't block active sessions)";
      "`!version` — build info and runtime status";
      "`!desktop` — set wrapping to desktop width";
      "`!mobile` — set wrapping to mobile width";
      "`!wrapping [n]` — show or set line wrap width";
      "`!lines [n]` — show or set output lines for tool/code display";
      "`!scroll [n]` — view truncated output (n=block: 1=last, 2=2nd last; repeats advance)";
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
  | Command.Lines None ->
    reply (Printf.sprintf "Output lines: %d." t.output_lines)
  | Command.Lines (Some n) ->
    if n > 1000 then
      reply (Printf.sprintf
        "Output lines must be \u{2264}1000 (got %d) \u{2014} larger \
         values would scan huge tool results unnecessarily." n)
    else begin
      t.output_lines <- n;
      reply (Printf.sprintf "Output lines set to %d." n)
    end
  | Command.Scroll target ->
    let channel_id = msg.channel_id in
    (match Hashtbl.find_opt t.scroll_states channel_id with
     | None ->
       reply "No scrollable output in this thread."
     | Some state ->
       let n_blocks = List.length state.blocks in
       if n_blocks = 0 then
         reply "No scrollable output in this thread."
       else
         (* Resolve block index.  None = continue current block.
            Positive counts from most recent (1 = last).
            Negative uses Python-style indexing (-1 = most recent). *)
         let idx = match target with
           | None -> state.current_block
           | Some n when n > 0 -> n
           | Some n -> -n  (* Python-style: -1 → 1 (most recent), -2 → 2 *)
         in
         if idx < 1 || idx > n_blocks then
           reply (Printf.sprintf "Only %d output block(s) available." n_blocks)
         else begin
           state.current_block <- idx;
           let block = List.nth state.blocks (idx - 1) in
           let total = Array.length block.lines in
           let page_size = block.output_lines_used in
           (* If we've hit the end, wrap back to the first hidden page
              and surface content — not just a notice. *)
           let wrapped = block.next_line >= total in
           let start = if wrapped then block.output_lines_used
                       else block.next_line in
           let remaining = Array.sub block.lines start
             (min page_size (total - start)) in
           let t = Agent_process.truncate_for_display
             ~max_lines:(Array.length remaining)
             ~max_chars:Agent_process.max_output_display_chars
             (Array.to_list remaining) in
           (* Advance by exactly the lines shown so nothing is skipped. *)
           block.next_line <- start + t.shown;
           let end_line = start + t.shown in
           let display = String.concat "\n" t.display in
           let prefix = if wrapped
             then "*(looped back)* "
             else "" in
           let info = Printf.sprintf
             "%s*Block %d/%d \u{2014} lines %d\u{2013}%d of %d*"
             prefix idx n_blocks (start + 1) end_line total in
           reply (Printf.sprintf "```\n%s\n```\n%s"
             (Agent_process.escape_code_fences display) info)
         end)
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
    match Channel_manager.project_for_channel (channels t) ~channel_id with
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
(** Run the agent for one message and drain any messages that queued
    behind it. Caller must have set [session.processing <- true] and
    is responsible for resetting it on exit (typically via Fun.protect
    in the fiber that called us).

    Extracted from the body of [handle_thread_message] so the auto-trigger
    path in [fork_initial_prompt_run] can reuse it without going
    back through the queue check (which would cause it to queue itself
    behind the very flag we set to close the gateway race). *)
let rec process_session_message t session
    (msg : Discord_types.message) channel_info =
  let child_pid = ref None in
  Fun.protect ~finally:(fun () ->
    Option.iter (unregister_child_pid t session) !child_pid
  ) (fun () ->
    let channel_id = msg.channel_id in
    let message_id = msg.id in
    ignore (Discord_rest.create_reaction t.rest ~channel_id
      ~message_id ~emoji:"\xF0\x9F\x91\x80" ());
    Channel_manager.bump ~rest:t.rest ~guild_id:t.config.guild_id
      ~project_name:session.Session_store.project_name (channels t);
    let author_name = msg.author.username in
    let (channel_name, channel_type) =
      resolve_channel_context t ~channel_id ~session ?channel_info () in
    let on_pid pid =
      child_pid := Some pid;
      register_child_pid t session pid;
      if session.stop_requested then
        ignore (request_session_process_stop t session);
      Logs.info (fun m -> m "bot: registered child pid %d" pid) in
    (* Forward-compat: [session.initial_prompt] is no longer set by any
       current caller (control_api now posts the prompt visibly and
       feeds it to handle_thread_message directly — see
       control_api.handle_start_session). The prepend stays so a
       sessions.json persisted before that change still gets the
       intended preface on its first message after a bot restart.
       Removable once we're sure no on-disk session still carries
       a non-None [initial_prompt]. *)
    let had_initial_prompt = Option.is_some session.initial_prompt in
    let prompt = match session.initial_prompt with
      | Some ctx ->
        Printf.sprintf "<session-context>\n%s\n</session-context>\n\n%s"
          ctx msg.content
      | None -> msg.content
    in
    let on_scroll_content chunks lines_used =
      (* Cap stored content at ~100KB to prevent memory bloat.
         The cap is a budget for take_fitting_prefix (which is
         fence-aware: ``` costs 6); for fence-free text this
         matches raw bytes within ±a single chunk. Tail-recursive
         to avoid stack growth on highly fragmented output. *)
      let rec take_bytes budget acc = function
        | [] -> List.rev acc
        | _ when budget <= 0 -> List.rev acc
        | c :: rest ->
          let len = String.length c in
          if len >= budget then
            List.rev (
              String.sub c 0
                (Agent_process.take_fitting_prefix
                   ~max_chars:budget c)
              :: acc)
          else
            take_bytes (budget - len - 1) (c :: acc) rest
      in
      let capped = take_bytes 100_000 [] chunks in
      let block = { lines = Array.of_list capped;
                    output_lines_used = lines_used;
                    next_line = lines_used } in
      let state = match Hashtbl.find_opt t.scroll_states channel_id with
        | Some s -> s
        | None -> { blocks = []; current_block = 1 } in
      (* Push new block to front (most recent first), cap at 20 *)
      let blocks = block :: (if List.length state.blocks >= 20
        then List.filteri (fun i _ -> i < 19) state.blocks
        else state.blocks) in
      state.blocks <- blocks;
      state.current_block <- 1;
      Hashtbl.replace t.scroll_states channel_id state in
    let on_session_id sid =
      try
        Session_store.set_session_id t.sessions session
          ~session_id:sid
      with exn ->
        Logs.warn (fun m ->
          m "bot: failed to persist session id for %s: %s"
            session.thread_id (Printexc.to_string exn))
    in
    let result = Agent_runner.run ~sw:t.sw ~env:t.env ~rest:t.rest
            ~session ~channel_id ~prompt
            ~attachments:msg.attachments
            ~author_name ~channel_name ~channel_type
            ~wrap_width:t.wrap_width
            ~output_lines:t.output_lines
            ~on_scroll_content ~on_pid ~on_session_id () in
    ignore (Discord_rest.delete_own_reaction t.rest ~channel_id
      ~message_id ~emoji:"\xF0\x9F\x91\x80" ());
    (match result with
    | Ok () ->
      ignore (Discord_rest.create_reaction t.rest ~channel_id
        ~message_id ~emoji:"\xE2\x9C\x85" ());
      let prior_message_count = session.message_count in
      let prior_initial_prompt = session.initial_prompt in
      if had_initial_prompt then
        session.initial_prompt <- None;
      session.message_count <- session.message_count + 1;
      (try
         Session_store.save t.sessions
       with exn ->
         session.message_count <- prior_message_count;
         session.initial_prompt <- prior_initial_prompt;
         Logs.warn (fun m ->
           m "bot: failed to persist message completion for %s: %s"
             session.thread_id (Printexc.to_string exn)))
    | Error _ ->
      ignore (Discord_rest.create_reaction t.rest ~channel_id
        ~message_id ~emoji:"\xE2\x9D\x8C" ())));
  (* Drain the queue: process next pending message if any *)
  if session.stop_requested then
    ()
  else
    match Queue.take_opt session.pending_queue with
    | None -> ()
    | Some pending ->
      (* Remove hourglass, will get eyes when processing starts *)
      ignore (Discord_rest.delete_own_reaction t.rest
        ~channel_id:pending.msg.channel_id
        ~message_id:pending.msg.id ~emoji:"\xE2\x8F\xB3" ());
      process_session_message t session pending.msg pending.channel_info

let handle_thread_message t msg ?channel_info () =
  if t.draining then
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.Discord_types.channel_id
      ~content:"Bot is restarting and will restart when there are no running processes. Sending more messages may delay restart." ());
  match Session_store.find_opt t.sessions ~thread_id:msg.Discord_types.channel_id with
  | None -> ()
  | Some session ->
    if session.stop_requested then
      ignore (Discord_rest.create_message t.rest
        ~channel_id:msg.Discord_types.channel_id
        ~content:"Session is stopping. Start or resume a new session after it finishes." ())
    else if session.processing then begin
      (* Queue the message and react with hourglass *)
      Queue.add { Session_store.msg; channel_info } session.pending_queue;
      ignore (Discord_rest.create_reaction t.rest
        ~channel_id:msg.Discord_types.channel_id
        ~message_id:msg.id ~emoji:"\xE2\x8F\xB3" ())
    end else begin
      session.processing <- true;
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        Fun.protect ~finally:(fun () ->
          finalize_session_run t session
        ) (fun () ->
          process_session_message t session msg channel_info))
    end

(** Fork an agent run on a fresh session whose [processing] flag is
    already locked by the caller. Used by
    control_api.handle_start_session to fire the auto-trigger
    immediately, bypassing the queue check that
    [handle_thread_message] would otherwise impose on a busy session.

    Order requirement: caller MUST set [session.processing <- true]
    *before* calling [Session_store.add], so that any user message
    landing in the new thread between [add] and our fork queues
    correctly behind us — and MUST keep that flag set until calling
    here. The fork's [Fun.protect] resets the flag when the run
    completes (after the queue drain). *)
let fork_initial_prompt_run t ~session ~msg =
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    Fun.protect ~finally:(fun () ->
      finalize_session_run t session
    ) (fun () ->
      process_session_message t session msg None))

(** Ensure a session exists for a channel (control or project channels).
    Creates a persistent session using the current effective top-level agent. *)
let ensure_channel_session t ~channel_id ~project_name ~working_dir ~system_prompt =
  match Session_store.find_opt t.sessions ~thread_id:channel_id with
  | Some _ -> Ok ()
  | None ->
    match new_session_block_message () with
    | Some msg -> Error msg
    | None ->
      (try
         create_persistent_channel_session t ~channel_id ~project_name
           ~working_dir ~system_prompt
           ~agent_kind:(effective_top_level_agent t)
           ~session_override_kind:None;
         Logs.info (fun m -> m "bot: auto-created session for %s" project_name);
         Ok ()
       with exn ->
         Error (Printf.sprintf "Failed to persist session: %s"
           (Printexc.to_string exn)))

let sync_top_level_agent_policy t =
  refresh_top_level_disk_state t;
  match align_persistent_sessions_to_agent t
          ~current_channel_id:None
          ~new_agent:(effective_top_level_agent t) with
  | Error _ as err -> err
  | Ok rotation ->
    (match clear_policy_sync_pending t with
     | Ok () -> ()
     | Error err ->
       if should_log_policy_sync_clear_failure t err then
         Logs.warn (fun m ->
           m "bot: failed to clear top-level policy sync marker after runtime sync: %s"
             err));
    Ok rotation

let sync_top_level_agent_policy_best_effort t =
  match sync_top_level_agent_policy t with
  | Ok _ -> ()
  | Error err ->
    Logs.warn (fun m ->
      m "bot: top-level policy sync skipped during message routing: %s" err)

let handle_command_safely t msg cmd =
  try
    handle_command t msg cmd
  with exn ->
    Logs.warn (fun m ->
      m "bot: command failed in %s: %s"
        msg.Discord_types.channel_id (Printexc.to_string exn));
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.Discord_types.channel_id
      ~content:(Printf.sprintf "Command failed: %s" (Printexc.to_string exn)) ())

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
      | Command.List_claude_sessions | Command.List_codex_sessions
      | Command.List_gemini_sessions
      | Command.Help ->
        handle_command_safely t msg cmd
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
    handle_command_safely t msg (Command.parse msg.content)
  else begin
    let is_control = match t.config.control_channel_id with
      | Some ctl_id -> msg.channel_id = ctl_id | None -> false in
    let project_for_channel =
      Channel_manager.project_for_channel (channels t) ~channel_id:msg.channel_id in
    if is_control then begin
      sync_top_level_agent_policy_best_effort t;
      (match ensure_channel_session t ~channel_id:msg.channel_id
              ~project_name:"control" ~working_dir:(Sys.getcwd ())
              ~system_prompt:(Some (control_system_prompt (projects t))) with
       | Ok () -> handle_thread_message t msg ()
       | Error err ->
         ignore (Discord_rest.create_message t.rest
           ~channel_id:msg.channel_id ~content:err ()))
    end else match project_for_channel with
    | Some proj_name ->
      sync_top_level_agent_policy_best_effort t;
      (* Message in a project channel — persistent session (like control channel).
         The project Claude can create threads via MCP tools when needed. *)
      let proj = List.find_opt (fun (p : Project.t) -> p.name = proj_name) (projects t) in
      (match proj with
       | Some p ->
         let wd = match working_dir_of_project p with Ok d -> d | Error _ -> p.path in
         (match ensure_channel_session t ~channel_id:msg.channel_id
                  ~project_name:p.name ~working_dir:wd
                  ~system_prompt:(Some (project_system_prompt p)) with
          | Ok () ->
            Channel_manager.bump ~rest:t.rest ~guild_id:t.config.guild_id
              ~project_name:p.name (channels t);
            handle_thread_message t msg ()
          | Error err ->
            ignore (Discord_rest.create_message t.rest
              ~channel_id:msg.channel_id ~content:err ()))
       | None -> ())
    | None ->
      (* Check if this is a thread under a project channel.
         Since the control API creates sessions directly in bot memory,
         no disk reload is needed — sessions are always authoritative. *)
      match Session_store.find_opt t.sessions ~thread_id:msg.channel_id with
      | Some _ -> handle_thread_message t msg ()
      | None ->
        (* Look up the channel to find its parent *)
        (match Discord_rest.get_channel t.rest ~channel_id:msg.channel_id () with
         | Ok ch ->
           let parent_project =
             match ch.Discord_types.parent_id with
             | Some pid ->
               Channel_manager.project_for_channel (channels t) ~channel_id:pid
             | None -> None
           in
           (match parent_project with
            | Some proj_name ->
              (* Thread under a project channel with no session —
                 auto-create one (e.g. manually created in Discord).

                 Race-safety: defer the auto-create by 2s and re-
                 check the session store, in case
                 [control_api.handle_start_session] is in the middle
                 of an HTTP roundtrip to create a session for this
                 thread. Without the wait, a user typing into a
                 freshly-bot-created thread would race the
                 start_session HTTP and get a default-worktree
                 session here instead of the agent-specific
                 worktree the MCP caller asked for. The cost is a
                 one-time 2s delay for messages in manually-
                 created threads (rare in this bot's usage). *)
              let proj =
                List.find_opt (fun (p : Project.t) -> p.name = proj_name)
                  (projects t)
              in
              (match proj with
               | Some p ->
                 let wd =
                   match working_dir_of_project p with
                   | Ok d -> d
                   | Error _ -> p.path
                 in
                 Eio.Fiber.fork ~sw:t.sw (fun () ->
                   Eio.Time.sleep (Eio.Stdenv.clock t.env) 2.0;
                   (* If the bot started draining during the wait,
                      don't start fresh work — the restart's drain
                      phase may have already moved on past its
                      processing-flag wait, and a session born now
                      could orphan its child at handoff timeout. *)
                   if t.draining then
                     ignore (Discord_rest.create_message t.rest
                       ~channel_id:msg.channel_id
                       ~content:"Bot is restarting. Try again \
                                 shortly." ())
                   else
                     match Session_store.find_opt t.sessions
                             ~thread_id:msg.channel_id with
                     | Some _ ->
                       (* start_session won the race; route normally
                          (handle_thread_message queues if processing). *)
                       handle_thread_message t msg ~channel_info:ch ()
                     | None ->
                       (match ensure_channel_session t
                                ~channel_id:msg.channel_id
                                ~project_name:p.name ~working_dir:wd
                                ~system_prompt:None with
                        | Ok () ->
                          handle_thread_message t msg ~channel_info:ch ()
                        | Error err ->
                          ignore (Discord_rest.create_message t.rest
                            ~channel_id:msg.channel_id ~content:err ())))
               | None -> handle_thread_message t msg ())
            | None -> handle_thread_message t msg ())
         | Error e ->
           Logs.warn (fun m -> m "bot: channel lookup failed for %s: %s"
             msg.channel_id e);
           ignore (Discord_rest.create_message t.rest
             ~channel_id:msg.channel_id
             ~content:"Could not set up a session for this thread (channel lookup failed). Try again or use `!start`." ()))
  end

let create ~sw ~(env : Eio_unix.Stdenv.base) config =
  let rest = Discord_rest.create ~sw ~env ~token:config.Config.discord_token in
  (* Local bindings deliberately named to avoid shadowing the accessor
     functions [projects] and [channels]. If a closure accidentally uses
     these names, the stale data is obvious. Using [projects] without
     [bot] resolves to the accessor, producing a type error — catching
     stale-closure bugs at compile time. *)
  let discovered_projects = Project.discover ~base_directories:config.base_directories in
  let sessions = Session_store.create () in
  let initial_channels = Channel_manager.create () in
  let gateway = Discord_gateway.create
    ~token:config.discord_token
    ~intents:Discord_gateway.default_intents
    ~handler:(fun _event -> ())
  in
  let project_state = { projects = discovered_projects; channels = initial_channels } in
  let bot = { config; settings = Runtime_settings.load ();
               rest; gateway; project_state; sessions; env; sw;
               started_at = Unix.gettimeofday ();
               draining = false; child_pids = (ref Pid_set.empty, Mutex.create ());
               wrap_width = Agent_process.desktop_width;
               refreshing = false;
               output_lines = Agent_process.default_output_lines;
               policy_sync_clear_last_warning = None;
               scroll_states = Hashtbl.create 64 } in
  reconcile_persisted_stop_requests bot;
  reconcile_persisted_pending_agent_changes bot;
  bot.gateway.handler <- (fun event ->
    match event with
    | Discord_gateway.Connected user ->
      Logs.info (fun m -> m "bot: connected as %s" user.Discord_types.username);
      if Channel_manager.category_id bot.project_state.channels = None then
        Eio.Fiber.fork ~sw (fun () ->
          Channel_manager.setup ~rest ~guild_id:config.guild_id
            ~projects:bot.project_state.projects bot.project_state.channels;
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
          let current_projects = projects bot in
          let indexed = List.mapi (fun i p -> (i, p)) current_projects in
          let results = Array.make (List.length current_projects) ("", 0) in
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
            bot.project_state.channels activity;
          match config.control_channel_id with
          | Some ch_id ->
            let text = Printf.sprintf "Bot online. %d projects, %d channels, %d sessions."
              (List.length bot.project_state.projects)
              (Channel_manager.count bot.project_state.channels)
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
  Logs.info (fun m -> m "bot: discovered %d projects" (List.length bot.project_state.projects));
  List.iter (fun (p : Project.t) ->
    Logs.info (fun m -> m "  - %s (%s)" p.name p.path)
  ) bot.project_state.projects;
  Discord_gateway.connect ~sw:bot.sw ~env bot.gateway
