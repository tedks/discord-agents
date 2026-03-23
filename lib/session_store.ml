(** Session state management with controlled mutation and persistence.

    All session mutations go through this module. It owns the sessions map,
    handles persistence to disk, and provides atomic operations.
    Cross-process safety via flock on sessions.json.lock. *)

module SessionMap = Map.Make(String)

type session = {
  project_name : string;
  working_dir : string;
  agent_kind : Config.agent_kind;
  session_id : string;
  thread_id : string;
  system_prompt : string option;
  mutable message_count : int;
  mutable processing : bool;
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
      ("thread_id", `String s.thread_id);
      ("message_count", `Int s.message_count);
    ] @ (match s.system_prompt with
         | Some sp -> [("system_prompt", `String sp)]
         | None -> []))
  ) entries)

(** Deserialize sessions from JSON. *)
let sessions_of_json json =
  try
    let open Yojson.Safe.Util in
    let entries = to_list json |> List.map (fun j ->
      let thread_id = j |> member "thread_id" |> to_string in
      let session = {
        project_name = j |> member "project_name" |> to_string;
        working_dir = j |> member "working_dir" |> to_string;
        agent_kind = (match Config.agent_kind_of_string
          (j |> member "agent_kind" |> to_string) with
          | Ok k -> k | Error _ -> Config.Claude);
        session_id = j |> member "session_id" |> to_string;
        thread_id;
        system_prompt = j |> member "system_prompt" |> to_string_option;
        message_count = j |> member "message_count" |> to_int;
        processing = false;
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

(** Add a session and persist to disk. *)
let add t ~thread_id session =
  t.sessions <- SessionMap.add thread_id session t.sessions;
  save t

(** Remove a session and persist to disk. *)
let remove t ~thread_id =
  t.sessions <- SessionMap.remove thread_id t.sessions;
  save t

(** Find a session by thread ID. *)
let find_opt t ~thread_id =
  SessionMap.find_opt thread_id t.sessions

(** Get all sessions as (thread_id, session) pairs. *)
let bindings t = SessionMap.bindings t.sessions

(** Number of active sessions. *)
let count t = SessionMap.cardinal t.sessions

(** Increment message count for a session and persist. *)
let increment_message_count t session =
  session.message_count <- session.message_count + 1;
  save t

(** Reload sessions from disk if the file changed.
    Rate-limited to once per 5 seconds. Merges new sessions
    from disk without overwriting in-memory state. *)
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
