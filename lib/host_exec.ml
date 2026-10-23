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

(** Write [payload] to [in_w] while reading [out_r] to EOF, so neither
    side can deadlock the other: a remote command that stops reading its
    input (it failed, or never reads stdin) cannot block the write once
    pipe and SSH window buffers fill, and output it writes before reading
    cannot block it either.

    The write runs on its own thread, which owns [in_w] and closes it
    exactly once (closing an fd twice can close whatever another thread
    opened under the reused number). A thread rather than [select],
    which cannot watch fds at or above FD_SETSIZE (1024), and the bot
    raises its fd limit. If the read raises, the writer is not joined:
    it ends when the caller's cleanup kills ssh and the write fails. *)
let exchange ~in_w ~payload ~out_r =
  let writer = Thread.create (fun () ->
    Fun.protect ~finally:(fun () -> try Unix.close in_w with Unix.Unix_error _ -> ())
      (fun () ->
        try write_all in_w payload 0
        with Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
          (* The reader is gone; its output says why. *) ()))
    () in
  let output = read_all out_r in
  Thread.join writer;
  output

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
    output and its exit status, using the nonce-tagged markers. Markers
    are only recognised as whole lines (each is printed after a newline
    of its own). Pure, so every outcome is testable without SSH. *)
let classify_capture ~nonce ~cwd ~ssh_status output =
  let whole_line m = "\n" ^ m ^ "\n" in
  let started = whole_line Remote_script.(marker started_tag nonce "0") in
  match find_sub output started with
  | None ->
    let missing = whole_line Remote_script.(marker error_tag nonce "missing_cwd") in
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
    [cwd] a remote command runs in the login directory.

    [input] is sent to the remote command's stdin (after the script
    line), for writing files without putting their contents in argv.
    Remote only: a local command with [input] is rejected rather than
    silently run without it. *)
let run_capture ?(ssh = "ssh") ?timeout_s ?cwd ?(input = "") (host : Host.t) argv =
  match host.transport with
  | Host.Local when input <> "" ->
    Error (Setup_failed "input is only supported for remote commands")
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
        let close fd = try Unix.close fd with Unix.Unix_error _ -> () in
        let in_r, in_w = Unix.pipe ~cloexec:true () in
        let out_r, out_w = Unix.pipe ~cloexec:true () in
        (* [timeout_s] bounds the whole call with coreutils timeout (KILL
           5 s after TERM), so a hung executor cannot hold a system
           thread, or a stop requested meanwhile, indefinitely. *)
        (* A requested bound that cannot be enforced is refused rather
           than silently dropped. *)
        let argv =
          let base = ssh_argv ~ssh ~destination in
          match timeout_s, Resource.executable_in_path "timeout" with
          | Some t, Some timeout -> Ok ([timeout; "-k"; "5"; string_of_int t] @ base, true)
          | Some _, None ->
            Error (Setup_failed
              "a time limit was requested but coreutils `timeout` is not in PATH")
          | None, _ -> Ok (base, false)
        in
        match argv with
        | Error _ as e -> e
        | Ok (argv, in_own_group) ->
        match
          Unix.create_process (List.hd argv) (Array.of_list argv) in_r out_w out_w
        with
        | exception Unix.Unix_error (e, _, _) ->
          List.iter close [in_r; in_w; out_r; out_w];
          Error (Setup_failed (Printf.sprintf "could not start %s: %s"
            ssh (Unix.error_message e)))
        | pid ->
          close in_r;
          close out_w;
          (* Whatever happens below, ssh is reaped: killed first if the
             exchange failed part way, so waitpid cannot block. Not on
             success, where a late signal could turn a clean exit into a
             signal status. *)
          let reaped = ref false in
          let output, status =
            Fun.protect
              ~finally:(fun () ->
                close out_r;
                if not !reaped then begin
                  (* KILL, not TERM: a wait after a signal the process may
                     ignore could hang this cleanup forever. Killing the
                     local ssh is also what stops the remote side. *)
                  (* coreutils timeout puts itself in a new process group;
                     signal the group so its ssh child goes too. Signal the
                     pid as well: right after spawn the group may not exist
                     yet, and the pid alone is what waitpid needs gone. *)
                  if in_own_group then
                    (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
                  (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
                  ignore (waitpid_retry pid)
                end)
              (fun () ->
                let output =
                  exchange ~in_w
                    ~payload:(Remote_script.stdin_line script ^ input) ~out_r in
                let status = waitpid_retry pid in
                reaped := true;
                output, status)
          in
          (match timeout_s, exit_code status with
           | Some t, (124 | 137) when not (String.length output > 0
                                          && Option.is_some (find_sub output
                                               ("\n" ^ Remote_script.exit_tag ^ " " ^ nonce ^ " "))) ->
             Error (Transport_lost (Printf.sprintf "timed out after %d s" t))
           | _ -> classify_capture ~nonce ~cwd ~ssh_status:(exit_code status) output)

(* ── Streaming over SSH ──────────────────────────────────────────── *)

let max_stderr_bytes = 64 * 1024
let max_line_bytes = 1024 * 1024

(** Read a flow to EOF, keeping only the first [limit] bytes and saying
    so when more was dropped. Unlike [Buf_read.take_all ~max_size], it
    keeps draining past the limit, so a chatty child can never block on
    a full stderr pipe. *)
let drain_capped flow ~limit =
  let buf = Buffer.create 1024 in
  let dropped = ref 0 in
  let chunk = Cstruct.create 4096 in
  (try
     while true do
       let n = Eio.Flow.single_read flow chunk in
       let keep = min n (limit - Buffer.length buf) in
       if keep > 0 then
         Buffer.add_string buf (Cstruct.to_string ~len:keep chunk);
       dropped := !dropped + (n - max keep 0)
     done
   with End_of_file -> ());
  if !dropped > 0 then
    Buffer.add_string buf
      (Printf.sprintf "\n[... %d more bytes of stderr dropped]" !dropped);
  Buffer.contents buf

type stream_state = {
  mutable deferred_empty : int;
  (* Empty lines held back: the last one before the exit marker is the
     wrapper's own separator, not command output. *)
  mutable remote_pid : int option;
  mutable exit_status : int option;
  mutable remote_error : string option;
  mutable unexpected_lines : string list;
  (* Output outside the started..exit window (login-shell banners,
     stragglers after exit), newest first, capped; diagnostics only. *)
}

(** Markers are a small state machine: an error only before start, one
    start, then one exit. A marker out of that order is treated as
    ordinary output, so a command echoing marker-shaped text cannot
    restart or end the run early. [locally_signalled]: ssh was killed on
    this side (a stop), which is not a remote setup failure whether or
    not the command had started. *)
let classify_stream ~cwd ~ssh_status ~locally_signalled ~stderr state =
  match state.exit_status, state.remote_error, state.remote_pid with
  | Some 0, _, _ -> Ok ()
  | Some rc, _, _ -> Error (Exited (rc, stderr))
  | None, Some "busy", _ -> Error Busy
  | None, Some "missing_cwd", _ -> Error (Missing_cwd cwd)
  | None, Some code, _ ->
    Error (Setup_failed (code ^ (if stderr = "" then "" else ": " ^ stderr)))
  | None, None, Some _ -> Error (Transport_lost stderr)
  | None, None, None when locally_signalled -> Error (Transport_lost stderr)
  | None, None, None ->
    let noise = String.concat "\n" (List.rev state.unexpected_lines) in
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
    [on_line].

    If a callback raises or the fiber is cancelled, stdin is closed (which
    stops the remote command) and the local ssh is terminated and reaped
    before the exception propagates. *)
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
      match
        Eio.Process.spawn ~sw mgr ~stdin:stdin_r ~stdout:stdout_w
          ~stderr:stderr_w (prefix @ ssh_argv ~ssh ~destination)
      with
      | exception (Eio.Cancel.Cancelled _ as exn) -> close_all (); raise exn
      | exception exn ->
        close_all ();
        Error (Setup_failed (Printf.sprintf "could not start %s: %s"
          ssh (Printexc.to_string exn)))
      | proc ->
      close_noerr stdin_r;
      close_noerr stdout_w;
      close_noerr stderr_w;
      let state = {
        deferred_empty = 0;
        remote_pid = None; exit_status = None; remote_error = None;
        unexpected_lines = [];
      } in
      let flush_empty () =
        for _ = 1 to state.deferred_empty do on_line "" done;
        state.deferred_empty <- 0
      in
      let note_unexpected line =
        if List.length state.unexpected_lines < 20 then
          state.unexpected_lines <- line :: state.unexpected_lines
      in
      let stderr_text = ref "" in
      let awaited = ref None in
      let await () =
        match !awaited with
        | Some status -> status
        | None ->
          let status = Eio.Process.await proc in
          awaited := Some status;
          status
      in
      Fun.protect
        ~finally:(fun () ->
          (* Closing stdin is the remote stop signal. Always do it, so an
             exception or cancellation here never leaves a remote agent
             running, and reap ssh so it never outlives this call. *)
          close_noerr stdin_w;
          close_noerr stdout_r;
          close_noerr stderr_r;
          if Option.is_none !awaited then
            Eio.Cancel.protect (fun () ->
              (* KILL so the await below is bounded even if ssh ignores
                 TERM. *)
              (try Eio.Process.signal proc Sys.sigkill with _ -> ());
              ignore (await ())))
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
              let running () =
                Option.is_some state.remote_pid && Option.is_none state.exit_status in
              try
                while true do
                  let line = Eio.Buf_read.line reader in
                  match Remote_script.parse_marker ~nonce line with
                  | Remote_script.Started pid
                    when Option.is_none state.remote_pid
                      && Option.is_none state.remote_error ->
                    state.remote_pid <- Some pid;
                    Option.iter (fun f -> f pid) on_remote_pid
                  | Remote_script.Exit rc when running () ->
                    state.deferred_empty <- max 0 (state.deferred_empty - 1);
                    flush_empty ();
                    state.exit_status <- Some rc
                  | Remote_script.Remote_error code
                    when Option.is_none state.remote_pid ->
                    state.remote_error <- Some code
                  | _ when running () ->
                    if line = "" then
                      state.deferred_empty <- state.deferred_empty + 1
                    else begin
                      flush_empty ();
                      on_line line
                    end
                  | _ -> if line <> "" then note_unexpected line
                done
              with End_of_file ->
                (* No exit marker (connection lost): held-back empty lines
                   were the command's own. *)
                if running () then flush_empty ());
          let ssh_status, locally_signalled =
            match await () with
            | `Exited n -> n, false
            | `Signaled n -> 128 + n, true
          in
          classify_stream ~cwd ~ssh_status ~locally_signalled
            ~stderr:(String.trim !stderr_text) state)
