(** discord-agents — Discord harness for AI agents.

    Usage: discord-agents [--test [CHANNEL_ID]]

    Reads configuration from ~/.config/discord-agents/config.json
    (or the path given by --config), discovers projects in the configured
    base directories, connects to Discord, and waits for commands.

    --test           Run REST + gateway smoke test, then exit
    --test CHANNEL   Also send a test message to the given channel *)

let setup_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

let pidfile_path () =
  let home = Sys.getenv "HOME" in
  Filename.concat home ".config/discord-agents/discord-agents.pid"

(** Acquire an exclusive pidfile lock using Unix.lockf.
    Kills any existing instance first (via the lock holder's PID).
    The returned fd must stay open — closing it releases the lock. *)
let acquire_pidfile () =
  let path = pidfile_path () in
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o700;
  (* Try to acquire the lock non-blocking first *)
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
  (try
    Unix.lockf fd Unix.F_TLOCK 0
  with Unix.Unix_error (Unix.EAGAIN, _, _) | Unix.Unix_error (Unix.EACCES, _, _) ->
    (* Lock held by another instance — read its PID and kill it *)
    let ic = open_in path in
    let pid = try int_of_string (String.trim (input_line ic)) with _ -> 0 in
    close_in ic;
    if pid > 0 then begin
      Logs.info (fun m -> m "killing existing instance (pid %d)" pid);
      (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
      Unix.sleepf 2.0;
      (try Unix.kill pid 0;
        Unix.kill pid Sys.sigkill;
        Unix.sleepf 1.0
      with Unix.Unix_error _ -> ())
    end;
    (* Retry lock *)
    Unix.lockf fd Unix.F_LOCK 0);
  (* Write our PID *)
  ignore (Unix.lseek fd 0 Unix.SEEK_SET);
  let _ = Unix.ftruncate fd 0 in
  let pid_str = string_of_int (Unix.getpid ()) ^ "\n" in
  let _ = Unix.write_substring fd pid_str 0 (String.length pid_str) in
  (* Keep fd open — lock is held until process exits.
     Do NOT delete the pidfile on exit: the lockf serves as the
     liveness indicator. Deleting it races with the replacement
     instance which may have already written its PID. *)
  at_exit (fun () ->
    (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
    (try Unix.close fd with _ -> ()));
  fd

(** Smoke test: verify REST client works, optionally send a test message,
    then connect to gateway briefly to confirm Hello/Identify/READY. *)
let run_test ~sw ~env config test_channel =
  Logs.info (fun m -> m "test: creating REST client...");
  let rest = Discord_agents.Discord_rest.create ~sw ~env
    ~token:config.Discord_agents.Config.discord_token in
  Logs.info (fun m -> m "test: calling GET /gateway/bot...");
  (match Discord_agents.Discord_rest.get_gateway rest with
   | Ok url -> Logs.info (fun m -> m "test: gateway URL = %s" url)
   | Error e -> Logs.err (fun m -> m "test: get_gateway failed: %s" e); exit 1);
  (match test_channel with
   | Some channel_id ->
     Logs.info (fun m -> m "test: sending message to channel %s..." channel_id);
     (match Discord_agents.Discord_rest.create_message rest
              ~channel_id ~content:"discord-agents test: REST client works" () with
      | Ok msg ->
        Logs.info (fun m -> m "test: message sent, id=%s" msg.Discord_agents.Discord_types.id)
      | Error e ->
        Logs.err (fun m -> m "test: create_message failed: %s" e); exit 1)
   | None ->
     Logs.info (fun m -> m "test: skipping message send (no channel_id given)"));
  Logs.info (fun m -> m "test: connecting to gateway...");
  let ready = ref false in
  let gateway = Discord_agents.Discord_gateway.create
    ~token:config.discord_token
    ~intents:Discord_agents.Discord_gateway.default_intents
    ~handler:(fun event ->
      match event with
      | Discord_agents.Discord_gateway.Connected user ->
        Logs.info (fun m -> m "test: READY as %s — gateway works" user.Discord_agents.Discord_types.username);
        ready := true
      | _ -> ())
  in
  let clock = Eio.Stdenv.clock env in
  let deadline = Eio.Time.now clock +. 15.0 in
  (try
    Eio.Switch.run @@ fun test_sw ->
    Eio.Fiber.fork ~sw:test_sw (fun () ->
      Discord_agents.Discord_gateway.connect ~sw:test_sw ~env gateway);
    let rec wait () =
      if !ready then begin
        Logs.info (fun m -> m "test: all tests passed");
        Eio.Switch.fail test_sw Exit
      end else if Eio.Time.now clock > deadline then begin
        Logs.err (fun m -> m "test: timed out waiting for READY");
        Eio.Switch.fail test_sw (Failure "timeout")
      end else begin
        Eio.Time.sleep clock 0.5;
        wait ()
      end
    in
    wait ()
  with Exit -> ())

let () =
  setup_logging ();
  let args = Array.to_list Sys.argv |> List.tl in
  let test_mode = List.mem "--test" args in
  let test_channel =
    match args with
    | "--test" :: ch :: _ when not (String.length ch > 0 && ch.[0] = '-') -> Some ch
    | _ -> None
  in
  Logs.info (fun m -> m "discord-agents starting%s" (if test_mode then " (test mode)" else ""));
  (* Acquire pidfile lock — keeps fd open for process lifetime *)
  let _lock_fd = if not test_mode then Some (acquire_pidfile ()) else None in
  Random.self_init ();  (* Seed PRNG for heartbeat jitter *)
  let config = Discord_agents.Config.load () in
  if config.discord_token = "" then (
    Logs.err (fun m -> m "no discord token configured");
    Logs.err (fun m -> m "set discord_token in %s" (Discord_agents.Config.config_path ()));
    exit 1
  );
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if test_mode then
    run_test ~sw ~env config test_channel
  else begin
    let bot = Discord_agents.Bot.create ~sw ~env config in
    (* Shutdown via pipe: signal handler writes a byte, fiber reads it.
       No Eio I/O in the signal handler — just a raw Unix write. *)
    let shutdown_r, shutdown_w = Unix.pipe ~cloexec:true () in
    let handle_signal _signum =
      (* Write a single byte to wake up the shutdown fiber.
         Unix.write on a pipe is async-signal-safe. *)
      ignore (Unix.write_substring shutdown_w "!" 0 1)
    in
    Sys.set_signal Sys.sigint (Sys.Signal_handle handle_signal);
    Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_signal);
    (* SIGUSR1 triggers graceful restart via a separate pipe.
       Used by the MCP server to signal restart without going through Discord. *)
    let restart_r, restart_w = Unix.pipe ~cloexec:true () in
    Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _signum ->
      ignore (Unix.write_substring restart_w "!" 0 1)));
    (* Fiber that waits for the shutdown signal *)
    Eio.Fiber.fork ~sw (fun () ->
      let buf = Bytes.create 1 in
      (* Block until signal handler writes to the pipe *)
      let _n = Eio_unix.run_in_systhread (fun () ->
        Unix.read shutdown_r buf 0 1) in
      Logs.info (fun m -> m "shutdown: signal received");
      bot.gateway.shutdown <- true;
      (match bot.gateway.ws with
       | Some ws -> (try Discord_agents.Websocket.send_close ws with _ -> ())
       | None -> ());
      Unix.close shutdown_r;
      Unix.close shutdown_w);
    (* Fiber that waits for SIGUSR1 restart signal *)
    Eio.Fiber.fork ~sw (fun () ->
      let buf = Bytes.create 1 in
      let _n = Eio_unix.run_in_systhread (fun () ->
        Unix.read restart_r buf 0 1) in
      Logs.info (fun m -> m "restart: SIGUSR1 received");
      Unix.close restart_r;
      Unix.close restart_w;
      Discord_agents.Bot.trigger_restart bot ~notify:(fun msg ->
        Logs.info (fun m -> m "restart: %s" msg)));
    Discord_agents.Bot.run ~sw ~env bot
  end
