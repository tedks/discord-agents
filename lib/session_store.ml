(** Session state management with controlled mutation and persistence.

    All session mutations go through this module. It owns the sessions map,
    handles persistence to disk, and provides atomic operations.
    Cross-process safety via flock on sessions.json.lock. *)

module SessionMap = Map.Make(String)

type pending_message = {
  msg : Discord_types.message;
  channel_info : Discord_types.channel option;
}

type session = {
  project_name : string;
  working_dir : string;
  agent_kind : Config.agent_kind;
  (* Mutable because Codex assigns its session id server-side: the
     pre-generated UUID is overwritten once the first thread.started
     event arrives. Claude accepts a caller-supplied id, so its value
     never changes after creation. *)
  mutable session_id : string;
  (* True once the agent has acknowledged [session_id] as resumable.
     Always true for Claude/Gemini (caller-supplied ids). For Codex,
     starts false and flips true when thread.started arrives. Used by
     the Codex resume gate so a first-turn failure that occurred
     before/after the id assignment is handled correctly. *)
  mutable session_id_confirmed : bool;
  thread_id : Discord_types.channel_id;  (* threads are channels in Discord *)
  system_prompt : string option;
  mutable message_count : int;
  mutable processing : bool;
  pending_queue : pending_message Queue.t;
  mutable initial_prompt : string option;  (* One-shot context for the first message *)
}

type t = {
  mutable sessions : session SessionMap.t;
  mutable last_reload : float;
}

let sessions_file () =
  let home = Sys.getenv "HOME" in
  Filename.concat home ".config/discord-agents/sessions.json"

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
      @ (match s.initial_prompt with
         | Some ip -> [("initial_prompt", `String ip)]
         | None -> []))
  ) entries)

(** Deserialize sessions from JSON. *)
let sessions_of_json json =
  try
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
      let session = {
        project_name = j |> member "project_name" |> to_string;
        working_dir = j |> member "working_dir" |> to_string;
        agent_kind;
        session_id = j |> member "session_id" |> to_string;
        session_id_confirmed;
        thread_id;
        system_prompt = j |> member "system_prompt" |> to_string_option;
        message_count = j |> member "message_count" |> to_int;
        processing = false;
        pending_queue = Queue.create ();
        initial_prompt = j |> member "initial_prompt" |> to_string_option;
      } in
      (thread_id, session)
    ) in
    List.fold_left (fun acc (tid, s) -> SessionMap.add tid s acc)
      SessionMap.empty entries
  with exn ->
    Logs.warn (fun m -> m "session_store: parse error: %s" (Printexc.to_string exn));
    SessionMap.empty

(** Save sessions to disk with file locking. *)
let save (t : t) =
  let json = sessions_to_json t.sessions in
  let path = sessions_file () in
  Resource.with_flock (lock_file ()) (fun () ->
    Resource.write_file_atomic path (Yojson.Safe.pretty_to_string json))

(** Load sessions from disk. *)
let load_from_disk () =
  let path = sessions_file () in
  if not (Sys.file_exists path) then SessionMap.empty
  else
    try
      let contents = Resource.read_file path in
      sessions_of_json (Yojson.Safe.from_string contents)
    with exn ->
      Logs.warn (fun m -> m "session_store: load error: %s" (Printexc.to_string exn));
      SessionMap.empty

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
    ?session_id_confirmed () =
  let session_id_confirmed = match session_id_confirmed with
    | Some b -> b
    | None -> Config.caller_pinned_session_id agent_kind
  in
  { project_name; working_dir; agent_kind; session_id;
    session_id_confirmed; thread_id; system_prompt;
    message_count; processing = false;
    pending_queue = Queue.create (); initial_prompt }

(** Add a session and persist to disk. *)
let add t ~(thread_id : Discord_types.channel_id) session =
  t.sessions <- SessionMap.add thread_id session t.sessions;
  save t

(** Remove a session and persist to disk. *)
let remove t ~(thread_id : Discord_types.channel_id) =
  t.sessions <- SessionMap.remove thread_id t.sessions;
  save t

(** Find a session by thread ID. *)
let find_opt t ~(thread_id : Discord_types.channel_id) =
  SessionMap.find_opt thread_id t.sessions

(** Get all sessions as (thread_id, session) pairs. *)
let bindings t = SessionMap.bindings t.sessions

(** Number of active sessions. *)
let count t = SessionMap.cardinal t.sessions

(** Increment message count for a session and persist. *)
let increment_message_count t session =
  session.message_count <- session.message_count + 1;
  save t

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
    session.session_id <- session_id;
    session.session_id_confirmed <- true;
    save t
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
