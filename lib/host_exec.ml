(** Run commands on a host: locally, or on an executor over SSH.

    [run_capture] is the short-command path (git, probes): run to
    completion, collect merged output. [run_streaming_ssh] is the
    agent-turn path: a long-running command whose stdout lines are
    delivered as they arrive, killed on the remote side when the local
    ssh process dies. The script protocol is in [Remote_script].

    Every failure says which stage failed, because the right message
    to a Discord user differs: "framework0 is unreachable" and "the
    agent exited 1" call for different next steps. *)

type failure =
  | Unreachable of string
      (** ssh exited 255 before the remote script started: no route,
          refused, auth failure. Carries ssh's diagnostics. *)
  | Setup_failed of string
      (** The remote shell could not set up the run (e.g. a missing
          [base64], [flock] or [setsid], or a lock directory it cannot
          create), or the command itself was rejected locally. *)
  | Busy
      (** Another run holding the same lock key is alive on that host. *)
  | Missing_cwd of string
  | Transport_lost of string
      (** The command started but the connection ended before it
          reported an exit status. Also what a deliberate stop (killing
          the local ssh process) looks like from here. *)
  | Exited of int * string
      (** The command ran and exited non-zero (128+N for signal N),
          with its merged output (capture) or ssh stderr (streaming). *)

let failure_detail = function
  | Unreachable s | Setup_failed s | Transport_lost s -> s
  | Exited (_, s) -> s
  | Missing_cwd dir -> dir
  | Busy -> ""

let describe_failure ~host failure =
  match failure with
  | Unreachable _ -> Printf.sprintf "%s is unreachable" host
  | Setup_failed s -> Printf.sprintf "could not start the command on %s: %s" host s
  | Busy -> Printf.sprintf "a previous run of this session is still alive on %s" host
  | Missing_cwd dir -> Printf.sprintf "%s does not exist on %s" dir host
  | Transport_lost _ -> Printf.sprintf "lost the connection to %s" host
  | Exited (code, _) -> Printf.sprintf "the command exited %d on %s" code host

(** Options for every SSH call. [-T]: no pty, so no "Pseudo-terminal
    will not be allocated" noise and no SIGHUP semantics to reason
    about. [BatchMode]: never prompt. [LogLevel=ERROR]: drop banners
    and host-key warnings that remote ssh configs emit. The keepalives
    bound a silent partition to about 30 s. *)
let ssh_options = [
  "-T";
  "-o"; "BatchMode=yes";
  "-o"; "ConnectTimeout=5";
  "-o"; "LogLevel=ERROR";
  "-o"; "ServerAliveInterval=10";
  "-o"; "ServerAliveCountMax=3";
]

(** [--] ends ssh's option parsing, so a destination can never be read
    as an option even if validation upstream were bypassed. *)
let ssh_argv ~ssh ~destination =
  (ssh :: ssh_options) @ ["--"; destination; Remote_script.bootstrap]

let resolve_ssh ssh =
  match Resource.executable_in_path ssh with
  | Some path -> Ok path
  | None -> Error (Setup_failed (Printf.sprintf "ssh client %S not found in PATH" ssh))

(* ── Capture ─────────────────────────────────────────────────────── *)

let read_all fd =
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  let rec loop () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf chunk 0 n; loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ();
  Buffer.contents buf

let rec write_all fd s off =
  if off < String.length s then
    match Unix.write_substring fd s off (String.length s - off) with
    | n -> write_all fd s (off + n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s off

let rec waitpid_retry pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_retry pid

let exit_code = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n

(** The local half of [run_capture]: [sh -c 'cd DIR && CMD 2>&1'],
    exactly what [Project.run_capture] has always done, so moving
    Project onto this module changes nothing for single-host installs. *)
let local_capture ?cwd argv =
  let prefix = match cwd with
    | Some dir -> "cd " ^ Filename.quote dir ^ " && "
    | None -> "" in
  let cmd = prefix ^ String.concat " " (List.map Filename.quote argv) ^ " 2>&1" in
  let ic = Unix.open_process_in cmd in
  let output = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string output (input_line ic);
       Buffer.add_char output '\n'
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok (Buffer.contents output)
  | status -> Error (Exited (exit_code status, Buffer.contents output))

let find_sub ?(from = 0) s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go from

let rfind_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i < 0 then None
    else if String.sub s i m = sub then Some i
    else go (i - 1)
  in
  go (n - m)

(** Split the merged output of a remote capture into the command's own
    output and its exit status, using the nonce-tagged markers. Pure,
    so every outcome is testable without SSH. *)
let classify_capture ~nonce ~cwd ~ssh_status output =
  let started =
    Remote_script.(marker started_tag nonce "0") ^ "\n" in
  match find_sub output started with
  | None ->
    let missing = Remote_script.(marker error_tag nonce "missing_cwd") in
    if Option.is_some (find_sub output missing) then Error (Missing_cwd cwd)
    else if ssh_status = 255 then Error (Unreachable (String.trim output))
    else Error (Setup_failed (String.trim output))
  | Some i ->
    let body_start = i + String.length started in
    let exit_prefix = "\n" ^ Remote_script.exit_tag ^ " " ^ nonce ^ " " in
    let body_and_trailer =
      String.sub output body_start (String.length output - body_start) in
    match rfind_sub body_and_trailer exit_prefix with
    | None -> Error (Transport_lost body_and_trailer)
    | Some j ->
      let body = String.sub body_and_trailer 0 j in
      let rest_start = j + String.length exit_prefix in
      (* Only the trailer's own line is the status. Anything after it is
         ssh's stderr (merged into the same pipe) written after the
         remote command exited, not command output. *)
      let status_line =
        let rest =
          String.sub body_and_trailer rest_start
            (String.length body_and_trailer - rest_start) in
        match String.index_opt rest '\n' with
        | Some k -> String.sub rest 0 k
        | None -> rest
      in
      match int_of_string_opt (String.trim status_line) with
      | Some 0 -> Ok body
      | Some rc -> Error (Exited (rc, body))
      | None -> Error (Transport_lost body)

(** Run [argv] in [cwd] on [host] and return its merged stdout/stderr.

    Synchronous (Unix, not Eio): call it from a system thread, as the
    existing [Project] git helpers are. Relies on SIGPIPE being ignored,
    which [Eio_main.run] does process-wide, so an ssh that dies before
    reading its stdin is an [EPIPE] here rather than a dead bot. Without
    [cwd] a remote command runs in the login directory. *)
let run_capture ?(ssh = "ssh") ?cwd (host : Host.t) argv =
  match host.transport with
  | Host.Local -> local_capture ?cwd argv
  | Host.Ssh destination ->
    let cwd = Option.value cwd ~default:"." in
    let nonce = Remote_script.fresh_nonce () in
    match Remote_script.script ~nonce ~mode:Capture ~cwd ~env:[] ~argv with
    | Error e -> Error (Setup_failed e)
    | Ok script ->
      match resolve_ssh ssh with
      | Error _ as e -> e
      | Ok ssh ->
        let in_r, in_w = Unix.pipe ~cloexec:true () in
        let out_r, out_w = Unix.pipe ~cloexec:true () in
        let close fd = try Unix.close fd with Unix.Unix_error _ -> () in
        let pid =
          try
            Unix.create_process ssh
              (Array.of_list (ssh_argv ~ssh ~destination)) in_r out_w out_w
          with exn ->
            List.iter close [in_r; in_w; out_r; out_w];
            raise exn
        in
        close in_r;
        close out_w;
        (* An early-exiting ssh (unreachable host) closes its stdin: the
           write fails with EPIPE and the output below explains why. *)
        (try write_all in_w (Remote_script.stdin_line script) 0
         with Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> ());
        close in_w;
        let output =
          Fun.protect ~finally:(fun () -> close out_r) (fun () -> read_all out_r)
        in
        let status = waitpid_retry pid in
        classify_capture ~nonce ~cwd ~ssh_status:(exit_code status) output

(* ── Streaming over SSH ──────────────────────────────────────────── *)

let max_stderr_bytes = 64 * 1024
let max_line_bytes = 1024 * 1024

(** Read a flow to EOF, keeping only the first [limit] bytes. Unlike
    [Buf_read.take_all ~max_size], it keeps draining past the limit, so
    a chatty child can never block on a full stderr pipe. *)
let drain_capped flow ~limit =
  let buf = Buffer.create 1024 in
  let chunk = Cstruct.create 4096 in
  (try
     while true do
       let n = Eio.Flow.single_read flow chunk in
       let keep = min n (limit - Buffer.length buf) in
       if keep > 0 then
         Buffer.add_string buf (Cstruct.to_string ~len:keep chunk)
     done
   with End_of_file -> ());
  Buffer.contents buf

type stream_state = {
  mutable deferred_empty : int;
  (* Empty lines held back: the last one before the exit marker is the
     wrapper's own separator, not command output. *)
  mutable remote_pid : int option;
  mutable exit_status : int option;
  mutable remote_error : string option;
  mutable pre_start_noise : string list;  (* newest first, capped *)
}

let classify_stream ~cwd ~ssh_status ~stderr state =
  match state.exit_status, state.remote_error, state.remote_pid with
  | Some 0, _, _ -> Ok ()
  | Some rc, _, _ -> Error (Exited (rc, stderr))
  | None, Some "busy", _ -> Error Busy
  | None, Some "missing_cwd", _ -> Error (Missing_cwd cwd)
  | None, Some code, _ -> Error (Setup_failed (code ^ (if stderr = "" then "" else ": " ^ stderr)))
  | None, None, Some _ -> Error (Transport_lost stderr)
  | None, None, None ->
    let noise = String.concat "\n" (List.rev state.pre_start_noise) in
    let detail = String.trim (String.concat "\n" [stderr; noise]) in
    if ssh_status = 255 then Error (Unreachable detail)
    else Error (Setup_failed detail)

(** Run one agent turn on [destination] in [cwd], delivering each stdout
    line to [on_line] as it arrives.

    [on_spawn] gets the local ssh pid immediately after spawn; stopping
    the run means signalling that process (or its group, when [prefix]
    is [setsid --wait]): the remote watchdog sees stdin close and kills
    the command's process group. [on_remote_pid] gets the remote pid
    once the command has started. Marker lines are never passed to
    [on_line]. If a callback raises, stdin is closed before the
    exception propagates, which stops the remote command too. *)
let run_streaming_ssh ~sw ~mgr ?(ssh = "ssh") ?(prefix = []) ~destination
    ~cwd ~lock_key ?(env = []) ~argv ~on_line ?on_spawn ?on_remote_pid () =
  let nonce = Remote_script.fresh_nonce () in
  match Remote_script.script ~nonce ~mode:(Run { lock_key }) ~cwd ~env ~argv with
  | Error e -> Error (Setup_failed e)
  | Ok script ->
    match resolve_ssh ssh with
    | Error _ as e -> e
    | Ok ssh ->
      let close_noerr flow = try Eio.Resource.close flow with _ -> () in
      let stdin_r, stdin_w = Eio.Process.pipe ~sw mgr in
      let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
      let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
      let close_all () =
        close_noerr stdin_r; close_noerr stdin_w;
        close_noerr stdout_r; close_noerr stdout_w;
        close_noerr stderr_r; close_noerr stderr_w
      in
      let proc =
        try
          Eio.Process.spawn ~sw mgr ~stdin:stdin_r ~stdout:stdout_w
            ~stderr:stderr_w (prefix @ ssh_argv ~ssh ~destination)
        with exn -> close_all (); raise exn
      in
      close_noerr stdin_r;
      close_noerr stdout_w;
      close_noerr stderr_w;
      let state = {
        deferred_empty = 0;
        remote_pid = None; exit_status = None; remote_error = None;
        pre_start_noise = [];
      } in
      let flush_empty () =
        for _ = 1 to state.deferred_empty do on_line "" done;
        state.deferred_empty <- 0
      in
      let stderr_text = ref "" in
      Fun.protect
        ~finally:(fun () ->
          (* Closing stdin is the remote stop signal. Always do it, so an
             exception or cancellation here never leaves a remote agent
             running. *)
          close_noerr stdin_w;
          close_noerr stdout_r;
          close_noerr stderr_r)
        (fun () ->
          Option.iter (fun f -> f (Eio.Process.pid proc)) on_spawn;
          (try Eio.Flow.copy_string (Remote_script.stdin_line script) stdin_w
           with
           | Eio.Cancel.Cancelled _ as exn -> raise exn
           | _ -> () (* ssh already gone; the exit status explains why *));
          Eio.Fiber.both
            (fun () -> stderr_text := drain_capped stderr_r ~limit:max_stderr_bytes)
            (fun () ->
              let reader = Eio.Buf_read.of_flow ~max_size:max_line_bytes stdout_r in
              try
                while true do
                  let line = Eio.Buf_read.line reader in
                  match Remote_script.parse_marker ~nonce line with
                  | Remote_script.Started pid ->
                    state.remote_pid <- Some pid;
                    Option.iter (fun f -> f pid) on_remote_pid
                  | Remote_script.Exit rc ->
                    state.deferred_empty <- max 0 (state.deferred_empty - 1);
                    flush_empty ();
                    state.exit_status <- Some rc
                  | Remote_script.Remote_error code -> state.remote_error <- Some code
                  | Remote_script.Not_marker ->
                    if Option.is_some state.remote_pid
                    && Option.is_none state.exit_status then begin
                      if line = "" then
                        state.deferred_empty <- state.deferred_empty + 1
                      else begin
                        flush_empty ();
                        on_line line
                      end
                    end
                    else if List.length state.pre_start_noise < 20 then
                      state.pre_start_noise <- line :: state.pre_start_noise
                done
              with End_of_file -> ());
          let ssh_status =
            match Eio.Process.await proc with
            | `Exited n -> n
            | `Signaled n -> 128 + n
          in
          classify_stream ~cwd ~ssh_status ~stderr:(String.trim !stderr_text) state)
