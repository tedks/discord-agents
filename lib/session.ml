(** Agent session — the bridge between a Discord thread and an agent subprocess.

    A session owns:
    - A Discord thread (for user communication)
    - An agent subprocess (claude/codex/gemini in a worktree)
    - The I/O bridge between them *)

type state =
  | Starting
  | Active
  | Stopping
  | Stopped
[@@deriving show]

type t = {
  id : string; (** Unique session ID *)
  project_name : string;
  agent_kind : Config.agent_kind;
  thread_id : string; (** Discord thread ID *)
  worktree_path : string;
  mutable process : Agent_process.t option;
  mutable state : state;
}

let generate_id () =
  let buf = Bytes.create 8 in
  let ic = open_in "/dev/urandom" in
  really_input ic buf 0 8;
  close_in ic;
  let hex = Buffer.create 16 in
  Bytes.iter (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c))) buf;
  Buffer.contents hex

let create ~project_name ~agent_kind ~thread_id ~worktree_path =
  {
    id = generate_id ();
    project_name;
    agent_kind;
    thread_id;
    worktree_path;
    process = None;
    state = Starting;
  }

(** Start the agent subprocess. *)
let start session =
  match session.state with
  | Starting ->
    let proc = Agent_process.spawn
      ~kind:session.agent_kind
      ~working_dir:session.worktree_path
      ~initial_prompt:""
    in
    session.process <- Some proc;
    session.state <- Active;
    Ok ()
  | other ->
    Error (Printf.sprintf "cannot start session in state %s" (show_state other))

(** Send a Discord message to the agent. *)
let send_to_agent session text =
  match session.process with
  | Some proc when session.state = Active ->
    Agent_process.send_input proc text;
    Ok ()
  | Some _ ->
    Error "session not active"
  | None ->
    Error "no process"

(** Poll for agent output. Returns accumulated text if any. *)
let poll_output session =
  match session.process with
  | Some proc ->
    let buf = Buffer.create 256 in
    let rec drain () =
      match Agent_process.read_output proc with
      | Some chunk ->
        Buffer.add_string buf chunk;
        drain ()
      | None -> ()
    in
    drain ();
    if Buffer.length buf > 0 then
      Some (Buffer.contents buf)
    else
      None
  | None -> None

(** Stop the session and clean up. *)
let stop session =
  session.state <- Stopping;
  (match session.process with
   | Some proc -> Agent_process.stop proc
   | None -> ());
  session.state <- Stopped
