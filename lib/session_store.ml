(** Session state management with controlled mutation and persistence.

    All session mutations go through this module. It owns the sessions map,
    handles persistence to disk, and provides atomic operations.
    Cross-process safety via flock on sessions.json.lock. *)

module SessionMap = Map.Make(String)

type pending_message = {
  msg : Discord_types.message;
  channel_info : Discord_types.channel option;
}

type pending_agent_origin =
  | Default_rotation
  | Session_override

type pending_agent_change = {
  kind : Config.agent_kind;
  origin : pending_agent_origin;
}

let string_of_pending_agent_origin = function
  | Default_rotation -> "default_rotation"
  | Session_override -> "session_override"

let pending_agent_origin_of_string = function
  | "default_rotation" -> Some Default_rotation
  | "session_override" -> Some Session_override
  | _ -> None

let pending_agent_origin_of_json = function
  | `String origin ->
    (match pending_agent_origin_of_string origin with
     | Some origin -> origin
     | None ->
       Logs.warn (fun m ->
         m "session_store: unknown pending_agent_origin %S, defaulting to default_rotation"
           origin);
       Default_rotation)
  | _ -> Default_rotation

type session = {
  project_name : string;
  working_dir : string;
  agent_kind : Config.agent_kind;
  (* Persisted top-level session pin set via [!session-agent]. When
     present, default-agent rotations must leave this session on the
     recorded agent kind. *)
  mutable session_override_kind : Config.agent_kind option;
  (* Mutable because Codex and Gemini assign their session ids
     server-side: the pre-generated UUID is overwritten once the
     first event arrives (Codex's [thread.started] / Gemini's
     [init]). Claude accepts a caller-supplied id, so its value
     never changes after creation. See [Config.caller_pinned_session_id]. *)
  mutable session_id : string;
  (* True once the agent has acknowledged [session_id] as resumable.
     Always true for Claude (caller-supplied ids). For Codex and
     Gemini, starts false and flips true when the first server-side
     event echoes the id back. Used by the resume gate so a
     first-turn failure that occurred before/after the id assignment
     is handled correctly. *)
  mutable session_id_confirmed : bool;
  thread_id : Discord_types.channel_id;  (* threads are channels in Discord *)
  system_prompt : string option;
  mutable message_count : int;
  mutable processing : bool;
  pending_queue : pending_message Queue.t;
  mutable pending_agent_change : pending_agent_change option;
  mutable initial_prompt : string option;  (* One-shot context for the first message *)
  mutable child_pid : int option;  (* Runtime-only current agent subprocess *)
  mutable stop_requested : bool;  (* Persisted stop latch for active sessions *)
}

type t = {
  mutable sessions : session SessionMap.t;
  mutable last_reload : float;
}

let sessions_file () =
  Filename.concat (Resource.app_config_dir ()) "sessions.json"

let backup_file () = sessions_file () ^ ".bak"
let lock_file () = sessions_file () ^ ".lock"

(** Serialize sessions to JSON. *)
let sessions_to_json sessions =
  let entries = SessionMap.bindings sessions in
  `List (List.map (fun (_tid, s) ->
    `Assoc ([
      ("project_name", `String s.project_name);
      ("working_dir", `String s.working_dir);
      ("agent_kind", `String (Config.string_of_agent_kind s.agent_kind));
      ("session_id", `String s.session_id);
      ("session_id_confirmed", `Bool s.session_id_confirmed);
      ("thread_id", `String s.thread_id);
      ("message_count", `Int s.message_count);
    ] @ (match s.system_prompt with
         | Some sp -> [("system_prompt", `String sp)]
         | None -> [])
      @ (match s.session_override_kind with
         | Some kind ->
           [("session_override_kind",
             `String (Config.string_of_agent_kind kind))]
         | None -> [])
      @ (match s.pending_agent_change with
         | Some pending ->
           [ ("pending_agent_kind",
              `String (Config.string_of_agent_kind pending.kind));
             ("pending_agent_origin",
              `String (string_of_pending_agent_origin pending.origin)) ]
         | None -> [])
      @ (if s.stop_requested then
           [("stop_requested", `Bool true)]
         else [])
      @ (match s.initial_prompt with
         | Some ip -> [("initial_prompt", `String ip)]
         | None -> []))
  ) entries)

(** Deserialize sessions from JSON. *)
let sessions_of_json json =
  let open Yojson.Safe.Util in
  let entries = to_list json |> List.map (fun j ->
    let thread_id = j |> member "thread_id" |> to_string in
    let agent_kind = (match Config.agent_kind_of_string
      (j |> member "agent_kind" |> to_string) with
      | Ok k -> k | Error _ -> Config.Claude) in
    (* For sessions written before session_id_confirmed existed,
       derive the default from the agent's id origin: caller-pinned
       agents (Claude) are always confirmed; server-allocated ones
       (Codex, Gemini) default to false so the next run starts
       fresh rather than resuming a placeholder. *)
    let session_id_confirmed = match j |> member "session_id_confirmed" with
      | `Bool b -> b
      | _ -> Config.caller_pinned_session_id agent_kind in
    let pending_agent_change =
      match j |> member "pending_agent_kind" with
      | `String s ->
        (match Config.agent_kind_of_string s with
         | Ok kind ->
           let origin =
             pending_agent_origin_of_json (j |> member "pending_agent_origin")
           in
           Some { kind; origin }
         | Error _ -> None)
      | _ -> None
    in
    let session = {
      project_name = j |> member "project_name" |> to_string;
      working_dir = j |> member "working_dir" |> to_string;
      agent_kind;
      session_override_kind =
        (match j |> member "session_override_kind" with
         | `String s ->
           (match Config.agent_kind_of_string s with
            | Ok kind -> Some kind
            | Error _ -> None)
         | _ -> None);
      session_id = j |> member "session_id" |> to_string;
      session_id_confirmed;
      thread_id;
      system_prompt = j |> member "system_prompt" |> to_string_option;
      message_count = j |> member "message_count" |> to_int;
      processing = false;
      pending_queue = Queue.create ();
      pending_agent_change;
      initial_prompt = j |> member "initial_prompt" |> to_string_option;
      child_pid = None;
      stop_requested =
        (match j |> member "stop_requested" with
         | `Bool b -> b
         | _ -> false);
    } in
    (thread_id, session)
  ) in
  List.fold_left (fun acc (tid, s) -> SessionMap.add tid s acc)
    SessionMap.empty entries

(** Save sessions to disk with file locking. *)
let log_visible_but_unconfirmed path exn =
  Logs.warn (fun m ->
    m "session_store: write to %s is visible but durability could not be confirmed: %s"
      path (Printexc.to_string exn))

let save_with
    ?(preflight_write=Disk_health.preflight_write)
    ?(note_write_success=Disk_health.note_write_success)
    ?(note_write_failure=Disk_health.note_write_failure)
    ~write_file (t : t) =
  let json = sessions_to_json t.sessions in
  let path = sessions_file () in
  let backup = backup_file () in
  let rendered = Yojson.Safe.pretty_to_string json in
  let primary_warning = ref None in
  match preflight_write path with
  | Error err ->
    failwith err
  | Ok () ->
    let saw_disk_issue = ref false in
    (try
       Resource.with_flock (lock_file ()) (fun () ->
         Resource.cleanup_atomic_write_temps path;
         Resource.cleanup_atomic_write_temps backup;
         (try write_file path rendered with
          | Resource.Durable_write_visible_but_unconfirmed (path, exn) ->
            saw_disk_issue := true;
            note_write_failure path exn;
            primary_warning := Some (path, exn));
         (try write_file backup rendered with
          | Resource.Durable_write_visible_but_unconfirmed (path, exn) ->
            saw_disk_issue := true;
            note_write_failure path exn;
            log_visible_but_unconfirmed path exn
          | exn ->
            note_write_failure backup exn;
            Logs.warn (fun m ->
              m "session_store: failed to update backup %s: %s"
                backup (Printexc.to_string exn))));
       Option.iter (fun (path, exn) ->
         log_visible_but_unconfirmed path exn) !primary_warning;
       if not !saw_disk_issue then
         note_write_success path
     with exn ->
       note_write_failure path exn;
       raise exn)

let save t =
  save_with ~write_file:(fun path rendered ->
    Resource.write_file_atomic path rendered) t

let load_file path =
  let contents = Resource.read_file path in
  sessions_of_json (Yojson.Safe.from_string contents)

(** Load sessions from disk. *)
let load_from_disk () =
  let path = sessions_file () in
  let backup = backup_file () in
  match Sys.file_exists path, Sys.file_exists backup with
  | false, false -> SessionMap.empty
  | false, true ->
    (match load_file backup with
     | sessions ->
       Logs.warn (fun m ->
         m "session_store: primary missing; recovered from backup %s" backup);
       sessions
     | exception backup_exn ->
       Logs.warn (fun m ->
         m "session_store: backup load error from %s: %s"
           backup (Printexc.to_string backup_exn));
       SessionMap.empty)
  | true, _ ->
    (match load_file path with
     | sessions -> sessions
     | exception exn ->
       Logs.warn (fun m ->
         m "session_store: load error from %s: %s"
           path (Printexc.to_string exn));
       (match load_file backup with
        | sessions ->
          Logs.warn (fun m ->
            m "session_store: recovered from backup %s" backup);
          sessions
        | exception backup_exn ->
          Logs.warn (fun m ->
            m "session_store: backup load error from %s: %s"
              backup (Printexc.to_string backup_exn));
          SessionMap.empty))

(** Create a session store, loading persisted sessions from disk. *)
let create () =
  { sessions = load_from_disk (); last_reload = Unix.gettimeofday () }

(** Construct a session record with sensible defaults. The
    [session_id_confirmed] default is derived from the agent: Claude
    pins its own id (confirmed at creation), while Codex and Gemini
    allocate server-side and start unconfirmed until the parser sees
    the first event. Callers can override via the optional arg. *)
let make_session ~project_name ~working_dir ~agent_kind ~session_id
    ~thread_id ~system_prompt ~initial_prompt
    ?(message_count = 0)
    ?(session_override_kind = None)
    ?(pending_agent_change = None)
    ?session_id_confirmed () =
  let session_id_confirmed = match session_id_confirmed with
    | Some b -> b
    | None -> Config.caller_pinned_session_id agent_kind
  in
  { project_name; working_dir; agent_kind; session_override_kind; session_id;
    session_id_confirmed; thread_id; system_prompt;
    message_count; processing = false;
    pending_queue = Queue.create (); pending_agent_change; initial_prompt;
    child_pid = None; stop_requested = false }

let persist_or_rollback rollback f =
  try
    let result = f () in
    Ok result
  with exn ->
    rollback ();
    Error (Printexc.to_string exn)

(** Add a session and persist to disk. *)
let add t ~(thread_id : Discord_types.channel_id) session =
  let prior = t.sessions in
  t.sessions <- SessionMap.add thread_id session t.sessions;
  match persist_or_rollback (fun () -> t.sessions <- prior) (fun () -> save t) with
  | Ok () -> ()
  | Error err -> failwith err

(** Remove a session and persist to disk. *)
let remove t ~(thread_id : Discord_types.channel_id) =
  let prior = t.sessions in
  t.sessions <- SessionMap.remove thread_id t.sessions;
  match persist_or_rollback (fun () -> t.sessions <- prior) (fun () -> save t) with
  | Ok () -> ()
  | Error err -> failwith err

(** Find a session by thread ID. *)
let find_opt t ~(thread_id : Discord_types.channel_id) =
  SessionMap.find_opt thread_id t.sessions

(** Get all sessions as (thread_id, session) pairs. *)
let bindings t = SessionMap.bindings t.sessions

(** Number of active sessions. *)
let count t = SessionMap.cardinal t.sessions

(** Increment message count for a session and persist. *)
let increment_message_count t session =
  let prior = session.message_count in
  session.message_count <- session.message_count + 1;
  match persist_or_rollback (fun () -> session.message_count <- prior)
          (fun () -> save t) with
  | Ok () -> ()
  | Error err -> failwith err

let set_pending_agent_change t session pending_agent_change =
  let prior = session.pending_agent_change in
  if prior = pending_agent_change then
    Ok ()
  else begin
    session.pending_agent_change <- pending_agent_change;
    persist_or_rollback
      (fun () -> session.pending_agent_change <- prior)
      (fun () -> save t)
  end

let set_session_override_kind t session session_override_kind =
  let prior = session.session_override_kind in
  if prior = session_override_kind then
    Ok ()
  else begin
    session.session_override_kind <- session_override_kind;
    persist_or_rollback
      (fun () -> session.session_override_kind <- prior)
      (fun () -> save t)
  end

let set_override_and_pending_agent_change t session
    ~session_override_kind ~pending_agent_change =
  let prior_override = session.session_override_kind in
  let prior_pending = session.pending_agent_change in
  if prior_override = session_override_kind
     && prior_pending = pending_agent_change
  then
    Ok ()
  else begin
    session.session_override_kind <- session_override_kind;
    session.pending_agent_change <- pending_agent_change;
    persist_or_rollback
      (fun () ->
        session.session_override_kind <- prior_override;
        session.pending_agent_change <- prior_pending)
      (fun () -> save t)
  end

(** Update a session's id and mark it confirmed for resume.
    Used when an agent assigns its id server-side (Codex's
    thread.started) so the pre-generated UUID is replaced before the
    next resume. Persisting [session_id_confirmed] here is the
    load-bearing bit: it tells the next invocation to issue
    [codex exec resume] rather than start a fresh session. *)
let set_session_id t session ~session_id =
  let already = session.session_id = session_id
                && session.session_id_confirmed in
  if not already then begin
    let prior_id = session.session_id in
    let prior_confirmed = session.session_id_confirmed in
    session.session_id <- session_id;
    session.session_id_confirmed <- true;
    match persist_or_rollback
            (fun () ->
              session.session_id <- prior_id;
              session.session_id_confirmed <- prior_confirmed)
            (fun () -> save t) with
    | Ok () -> ()
    | Error err -> failwith err
  end

let set_stop_requested t session stop_requested =
  let prior = session.stop_requested in
  if prior = stop_requested then
    Ok ()
  else begin
    session.stop_requested <- stop_requested;
    persist_or_rollback
      (fun () -> session.stop_requested <- prior)
      (fun () -> save t)
  end

(** Reload sessions from disk if the file changed.
    Rate-limited to once per 5 seconds. Merges new sessions
    from disk without overwriting in-memory state.
    NOTE: With the control API, the bot is the sole session writer.
    This is kept for crash recovery (loading persisted state on startup)
    but is no longer needed for cross-process synchronization. *)
let maybe_reload t =
  let now = Unix.gettimeofday () in
  if now -. t.last_reload >= 5.0 then begin
    t.last_reload <- now;
    let path = sessions_file () in
    if Sys.file_exists path then
      try
        let stat = Unix.stat path in
        if stat.Unix.st_mtime > t.last_reload -. 5.0 then begin
          let loaded = load_from_disk () in
          SessionMap.iter (fun tid session ->
            if not (SessionMap.mem tid t.sessions) then
              t.sessions <- SessionMap.add tid session t.sessions
          ) loaded;
          Logs.debug (fun m -> m "session_store: reloaded (%d total)"
            (SessionMap.cardinal t.sessions))
        end
      with Unix.Unix_error _ -> ()
  end
