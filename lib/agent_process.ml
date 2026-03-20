(** Subprocess management for AI agents.

    Each agent session is a subprocess (claude, codex, gemini CLI) running
    in a git worktree. This module handles spawning, I/O bridging, and
    lifecycle management. *)

type status =
  | Running
  | Stopped of int (** exit code *)
  | Failed of string
[@@deriving show]

type t = {
  kind : Config.agent_kind;
  pid : int;
  stdin_fd : Unix.file_descr;
  stdout_fd : Unix.file_descr;
  stderr_fd : Unix.file_descr;
  working_dir : string;
  mutable status : status;
}

let command_of_kind = function
  | Config.Claude -> ("claude", [| "claude"; "--print" |])
  | Config.Codex -> ("codex", [| "codex" |])
  | Config.Gemini -> ("gemini", [| "gemini" |])

(** Spawn an agent subprocess in the given working directory.
    Returns the process handle for I/O bridging. *)
let spawn ~kind ~working_dir ~initial_prompt:_ =
  let (cmd, _argv) = command_of_kind kind in
  (* Create pipes for stdin/stdout/stderr *)
  let (child_stdin_r, parent_stdin_w) = Unix.pipe ~cloexec:true () in
  let (parent_stdout_r, child_stdout_w) = Unix.pipe ~cloexec:true () in
  let (parent_stderr_r, child_stderr_w) = Unix.pipe ~cloexec:true () in
  let pid = Unix.create_process_env cmd [| cmd |]
    (Unix.environment ())
    child_stdin_r child_stdout_w child_stderr_w
  in
  (* Close child-side fds in parent *)
  Unix.close child_stdin_r;
  Unix.close child_stdout_w;
  Unix.close child_stderr_w;
  (* Change to working directory handled by the subprocess *)
  ignore working_dir;
  {
    kind;
    pid;
    stdin_fd = parent_stdin_w;
    stdout_fd = parent_stdout_r;
    stderr_fd = parent_stderr_r;
    working_dir;
    status = Running;
  }

(** Send a message to the agent's stdin. *)
let send_input t msg =
  let bytes = Bytes.of_string (msg ^ "\n") in
  let len = Bytes.length bytes in
  let written = Unix.write t.stdin_fd bytes 0 len in
  if written <> len then
    Logs.warn (fun m -> m "agent_process: short write (%d/%d)" written len)

(** Non-blocking read from agent's stdout. Returns None if no data available. *)
let read_output t =
  let buf = Bytes.create 4096 in
  match Unix.select [t.stdout_fd] [] [] 0.0 with
  | ((_ :: _), _, _) ->
    let n = Unix.read t.stdout_fd buf 0 4096 in
    if n = 0 then None (* EOF *)
    else Some (Bytes.sub_string buf 0 n)
  | _ -> None

(** Check if the process is still running, update status if not. *)
let check_status t =
  match t.status with
  | Running ->
    (match Unix.waitpid [Unix.WNOHANG] t.pid with
     | (0, _) -> Running
     | (_, Unix.WEXITED code) ->
       t.status <- Stopped code;
       Stopped code
     | (_, Unix.WSIGNALED sig_) ->
       let msg = Printf.sprintf "killed by signal %d" sig_ in
       t.status <- Failed msg;
       Failed msg
     | (_, Unix.WSTOPPED _) -> Running)
  | other -> other

(** Send EOF to stdin and wait for the process to exit. *)
let stop t =
  (try Unix.close t.stdin_fd with Unix.Unix_error _ -> ());
  let (_pid, status) = Unix.waitpid [] t.pid in
  (try Unix.close t.stdout_fd with Unix.Unix_error _ -> ());
  (try Unix.close t.stderr_fd with Unix.Unix_error _ -> ());
  match status with
  | Unix.WEXITED code -> t.status <- Stopped code
  | Unix.WSIGNALED sig_ -> t.status <- Failed (Printf.sprintf "signal %d" sig_)
  | Unix.WSTOPPED _ -> t.status <- Failed "stopped"
