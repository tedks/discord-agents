(** Host_exec and Remote_script.

    The SSH paths run through test/fake_ssh.py, a relay that executes the
    "remote" command locally behind its own pipes, so the whole protocol
    (bootstrap, stdin script, markers, flock, watchdog) is exercised in
    [dune runtest] without a network. *)

module Host = Discord_agents.Host
module Host_exec = Discord_agents.Host_exec
module Remote_script = Discord_agents.Remote_script

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } ->
    Sys.readdir path |> Array.iter (fun n -> rm_rf (Filename.concat path n));
    Unix.rmdir path
  | _ -> Unix.unlink path

(* A private HOME (the remote lock dir lives under it) and a fake ssh
   executable that runs fake_ssh.py with an isolated interpreter. *)
let with_env f =
  let base = Filename.temp_dir "discord_agents_host_exec_" "" in
  let old_home = Sys.getenv_opt "HOME" in
  Fun.protect
    ~finally:(fun () ->
      (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
      rm_rf base)
    (fun () ->
      Unix.putenv "HOME" base;
      let relay = Filename.concat (Sys.getcwd ()) "fake_ssh.py" in
      let ssh = Filename.concat base "fake-ssh" in
      let oc = open_out ssh in
      Printf.fprintf oc "#!/bin/sh\nexec python3 -I %s \"$@\"\n"
        (Filename.quote relay);
      close_out oc;
      Unix.chmod ssh 0o755;
      f ~base ~ssh)

let remote = {
  Host.name = "remote";
  transport = Host.Ssh "remote";
  writer_ssh = None;
  base_directories = None;
  enabled = true;
}

let unreachable = { remote with transport = Host.Ssh "unreachable" }
let local = Host.local_host "local"

let failure_name = function
  | Host_exec.Unreachable _ -> "Unreachable"
  | Setup_failed _ -> "Setup_failed"
  | Busy -> "Busy"
  | Missing_cwd _ -> "Missing_cwd"
  | Transport_lost _ -> "Transport_lost"
  | Exited (n, _) -> Printf.sprintf "Exited %d" n

let check_ok label expected = function
  | Ok out -> Alcotest.(check string) label expected out
  | Error f ->
    Alcotest.failf "%s: %s (%s)" label (failure_name f) (Host_exec.failure_detail f)

let check_error label expected = function
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label expected
  | Error f -> Alcotest.(check string) label expected (failure_name f)

(* run_capture is synchronous but relies on SIGPIPE being ignored, which
   Eio_main.run guarantees in the bot. *)
let in_eio f = Eio_main.run (fun _ -> f ())

(* ── Pure ────────────────────────────────────────────────────────── *)

let test_quote_roundtrip () =
  List.iter (fun s ->
    let ic = Unix.open_process_args_in "/bin/sh"
        [| "/bin/sh"; "-c"; "printf %s " ^ Remote_script.quote s |] in
    let out = In_channel.input_all ic in
    ignore (Unix.close_process_in ic);
    Alcotest.(check string) (String.escaped s) s out)
    [""; "plain"; "it's"; "''"; "a b"; "$HOME `id` $(id)"; "\\n"; "x\ny";
     "\"dq\""; "é✓"; "-n"; "*"]

let script ?(mode = Remote_script.Capture) ?(env = []) ?(cwd = "/") argv =
  Remote_script.script ~nonce:"n" ~mode ~cwd ~env ~argv

let test_script_rejects () =
  let rejected label r =
    Alcotest.(check bool) label false (Result.is_ok r) in
  rejected "empty argv" (script []);
  rejected "NUL in argv" (script ["a\000b"]);
  rejected "NUL in cwd" (script ~cwd:"/a\000" ["true"]);
  rejected "bad env name" (script ~env:["A-B", "x"] ["true"]);
  rejected "env name injection" (script ~env:["A=1;id;B", "x"] ["true"]);
  rejected "NUL in env value" (script ~env:["A", "x\000"] ["true"]);
  rejected "lock key traversal"
    (script ~mode:(Run { lock_key = "../x" }) ["true"]);
  rejected "lock key slash"
    (script ~mode:(Run { lock_key = "a/b" }) ["true"]);
  rejected "empty lock key" (script ~mode:(Run { lock_key = "" }) ["true"]);
  rejected "cwd dash" (script ~cwd:"-" ["true"]);
  rejected "relative cwd" (script ~cwd:"a/b" ["true"]);
  (* exec would fail with E2BIG on one argument this long. *)
  rejected "oversized argument" (script ["printf"; String.make (128 * 1024) 'x']);
  rejected "oversized env value"
    (script ~env:["A", String.make (128 * 1024) 'x'] ["true"]);
  Alcotest.(check bool) "dot cwd" true (Result.is_ok (script ~cwd:"." ["true"]));
  rejected "reserved env name" (script ~env:["lockdir", "/tmp"] ["true"]);
  (* The limit covers NAME=value: a value under it with a long name is
     still too long. *)
  rejected "env entry with long name"
    (script ~env:[String.make 100 'A', String.make (128 * 1024 - 50) 'x'] ["true"]);
  Alcotest.(check bool) "ok" true
    (Result.is_ok (script ~mode:(Run { lock_key = "abc-123_x.y" }) ["true"]))

let test_parse_marker () =
  let p = Remote_script.parse_marker ~nonce:"abc" in
  let is_started = function Remote_script.Started 42 -> true | _ -> false in
  Alcotest.(check bool) "started" true
    (is_started (p "discord_agents.remote_started abc 42"));
  Alcotest.(check bool) "wrong nonce is plain output" true
    (p "discord_agents.remote_started xyz 42" = Remote_script.Not_marker);
  Alcotest.(check bool) "exit" true
    (p "discord_agents.remote_exit abc 3" = Remote_script.Exit 3);
  Alcotest.(check bool) "error" true
    (p "discord_agents.remote_error abc busy" = Remote_script.Remote_error "busy");
  Alcotest.(check bool) "agent json" true
    (p {|{"type":"assistant"}|} = Remote_script.Not_marker)

let test_classify_capture () =
  let c ?(status = 0) out =
    Host_exec.classify_capture ~nonce:"n" ~cwd:"/w" ~ssh_status:status out in
  let started = "\ndiscord_agents.remote_started n 0\n" in
  let exit rc = Printf.sprintf "\ndiscord_agents.remote_exit n %d\n" rc in
  check_ok "body kept exactly" "abc" (c (started ^ "abc" ^ exit 0));
  check_ok "trailing newline kept" "abc\n" (c (started ^ "abc\n" ^ exit 0));
  check_ok "empty output" "" (c (started ^ exit 0));
  check_error "nonzero" "Exited 2" (c (started ^ "x" ^ exit 2));
  check_ok "ssh noise after the trailer" "x"
    (c (started ^ "x" ^ exit 0 ^ "Connection to remote closed.\n"));
  check_error "no trailer" "Transport_lost" (c ~status:255 (started ^ "partial"));
  check_error "unreachable" "Unreachable" (c ~status:255 "ssh: no route\n");
  check_error "setup" "Setup_failed" (c ~status:127 "sh: base64: not found\n");
  check_error "missing cwd" "Missing_cwd"
    (c ~status:1 "\ndiscord_agents.remote_error n missing_cwd\n");
  (* Markers count only as whole lines: inside another line they are text. *)
  check_error "started marker mid-line" "Setup_failed"
    (c "banner discord_agents.remote_started n 0\nx");
  check_ok "banner before the marker" "x"
    (c ("Welcome, no newline" ^ started ^ "x" ^ exit 0));
  (* Output that imitates a trailer with another nonce is just output. *)
  check_ok "forged trailer ignored"
    "\ndiscord_agents.remote_exit other 0\nreal"
    (c (started ^ "\ndiscord_agents.remote_exit other 0\nreal" ^ exit 0))

(* ── Capture over (fake) SSH ─────────────────────────────────────── *)

let test_capture_roundtrips_arguments () =
  with_env (fun ~base:_ ~ssh ->
    in_eio (fun () ->
      let nasty = [
        "it's \"quoted\" $HOME `id` $(id) ; | & > <";
        "line one\nline two\n";
        "é ✓ \\ %s %n";
        "-n";
      ] in
      List.iter (fun arg ->
        check_ok (String.escaped arg) arg
          (Host_exec.run_capture ~ssh remote ["printf"; "%s"; arg])) nasty))

(* sshd hands the remote command to the login shell as one argument, and
   Linux caps a single argument at 128 KiB. The script rides on stdin, so
   a payload well past that cap must survive. *)
let test_capture_large_payload () =
  with_env (fun ~base:_ ~ssh ->
    in_eio (fun () ->
      let chunk c = String.make 100_000 c in
      let args = [chunk 'a'; chunk 'b'; chunk 'c'] in
      check_ok "300 KB through stdin" (String.concat "" args)
        (Host_exec.run_capture ~ssh remote ("printf" :: "%s%s%s" :: args))))

let test_capture_failures () =
  with_env (fun ~base ~ssh ->
    in_eio (fun () ->
      (match Host_exec.run_capture ~ssh remote
               ["sh"; "-c"; "echo out; echo err >&2; exit 3"] with
       | Error (Host_exec.Exited (3, out)) ->
         Alcotest.(check string) "merged output" "out\nerr\n" out
       | r -> check_error "exit 3" "Exited 3" (Result.map (fun _ -> ()) r));
      check_ok "cwd honoured" (base ^ "\n")
        (Host_exec.run_capture ~ssh ~cwd:base remote ["pwd"]);
      check_error "missing cwd" "Missing_cwd"
        (Host_exec.run_capture ~ssh ~cwd:(Filename.concat base "nope")
           remote ["true"]);
      check_error "unreachable" "Unreachable"
        (Host_exec.run_capture ~ssh unreachable ["true"]);
      check_error "no ssh client" "Setup_failed"
        (Host_exec.run_capture ~ssh:"/nonexistent/ssh" remote ["true"])))

let test_local_capture_unchanged () =
  (* The local path appends a newline to the last line, as
     Project.run_capture always has; the remote path returns exact bytes.
     Callers trim. *)
  check_ok "local ok" "x\n" (Host_exec.run_capture local ["printf"; "x"]);
  check_error "local failure" "Exited 4"
    (Host_exec.run_capture local ["sh"; "-c"; "exit 4"])

(* ── Streaming over (fake) SSH ───────────────────────────────────── *)

let with_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  f ~sw ~mgr:(Eio.Stdenv.process_mgr env) ~clock:(Eio.Stdenv.clock env)

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | exception Unix.Unix_error (Unix.EPERM, _, _) -> true

let wait_until ~clock ?(timeout = 10.0) label cond =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if cond () then ()
    else if Unix.gettimeofday () > deadline then Alcotest.failf "timed out: %s" label
    else (Eio.Time.sleep clock 0.05; loop ())
  in
  loop ()

let stream ~sw ~mgr ~ssh ?(destination = "remote") ?(cwd = "/")
    ?(lock_key = "k") ?env ?on_spawn ?on_remote_pid ?(on_line = ignore) argv =
  Host_exec.run_streaming_ssh ~sw ~mgr ~ssh ~destination ~cwd ~lock_key
    ?env ~argv ~on_line ?on_spawn ?on_remote_pid ()

let test_stream_lines_and_status () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock:_ ->
      let lines = ref [] and remote_pid = ref None and local_pid = ref None in
      let r =
        stream ~sw ~mgr ~ssh ~env:["GREETING", "hi there"]
          ~on_spawn:(fun p -> local_pid := Some p)
          ~on_remote_pid:(fun p -> remote_pid := Some p)
          ~on_line:(fun l -> lines := l :: !lines)
          ["sh"; "-c"; {|echo '{"n":1}'; echo "$GREETING"; printf 'no newline'|}]
      in
      check_ok "exit 0" "" (Result.map (fun () -> "") r);
      Alcotest.(check (list string)) "lines, no markers"
        [{|{"n":1}|}; "hi there"; "no newline"] (List.rev !lines);
      (* Genuine empty lines survive; only the wrapper's separator goes. *)
      let lines = ref [] in
      check_ok "empty lines" "" (Result.map (fun () -> "")
        (stream ~sw ~mgr ~ssh ~on_line:(fun l -> lines := l :: !lines)
           ["printf"; "a\n\nb\n\n"]));
      Alcotest.(check (list string)) "exact lines" ["a"; ""; "b"; ""]
        (List.rev !lines);
      Alcotest.(check bool) "remote pid reported" true (Option.is_some !remote_pid);
      Alcotest.(check bool) "local pid reported" true (Option.is_some !local_pid);
      check_error "exit code" "Exited 3"
        (stream ~sw ~mgr ~ssh ["sh"; "-c"; "exit 3"])))

let test_stream_setup_failures () =
  with_env (fun ~base ~ssh ->
    with_eio (fun ~sw ~mgr ~clock:_ ->
      check_error "missing cwd" "Missing_cwd"
        (stream ~sw ~mgr ~ssh ~cwd:(Filename.concat base "gone") ["true"]);
      check_error "unreachable" "Unreachable"
        (stream ~sw ~mgr ~ssh ~destination:"unreachable" ["true"]);
      check_error "bad lock key" "Setup_failed"
        (stream ~sw ~mgr ~ssh ~lock_key:"../escape" ["true"])))

let test_stream_busy () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let started = ref false in
      let first = ref None in
      Eio.Fiber.both
        (fun () ->
          first := Some (stream ~sw ~mgr ~ssh ~lock_key:"same"
                           ~on_remote_pid:(fun _ -> started := true)
                           ["sleep"; "2"]))
        (fun () ->
          wait_until ~clock "first run started" (fun () -> !started);
          check_error "second run refused" "Busy"
            (stream ~sw ~mgr ~ssh ~lock_key:"same" ["true"]);
          check_ok "other key fine" ""
            (Result.map (fun () -> "")
               (stream ~sw ~mgr ~ssh ~lock_key:"other" ["true"])));
      match !first with
      | Some r -> check_ok "first run" "" (Result.map (fun () -> "") r)
      | None -> Alcotest.fail "first run did not finish"))

(* Killing the local ssh process is how the bot stops a remote run and
   what a dropped connection looks like. The remote command's whole
   process group, grandchildren included, must die. *)
let test_stream_stop_kills_remote_group () =
  with_env (fun ~base ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let pidfile = Filename.concat base "grandchild.pid" in
      let local_pid = ref None and remote_pid = ref None in
      let result = ref None in
      Eio.Fiber.both
        (fun () ->
          result := Some (stream ~sw ~mgr ~ssh
            ~on_spawn:(fun p -> local_pid := Some p)
            ~on_remote_pid:(fun p -> remote_pid := Some p)
            ["sh"; "-c"; Printf.sprintf "sleep 300 & echo $! > %s; sleep 300"
                           (Filename.quote pidfile)]))
        (fun () ->
          wait_until ~clock "grandchild started" (fun () ->
            Sys.file_exists pidfile
            && String.trim (In_channel.with_open_text pidfile In_channel.input_all) <> "");
          Unix.kill (Option.get !local_pid) Sys.sigterm);
      check_error "stopped run" "Transport_lost"
        (Result.map (fun () -> "") (Option.get !result));
      let grandchild =
        int_of_string (String.trim (In_channel.with_open_text pidfile In_channel.input_all)) in
      let child = Option.get !remote_pid in
      wait_until ~clock "remote command killed" (fun () -> not (alive child));
      wait_until ~clock "grandchild killed" (fun () -> not (alive grandchild))))

(* A callback that raises must not leave the remote command running:
   closing stdin on the way out is the stop signal. *)
let test_stream_callback_exception_stops_remote () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let remote_pid = ref None in
      (match
         stream ~sw ~mgr ~ssh
           ~on_remote_pid:(fun p -> remote_pid := Some p)
           ~on_line:(fun _ -> failwith "boom")
           ["sh"; "-c"; "echo first; sleep 300"]
       with
       | _ -> Alcotest.fail "expected the callback's exception"
       | exception Failure msg -> Alcotest.(check string) "propagated" "boom" msg);
      let pid = Option.get !remote_pid in
      wait_until ~clock "remote command killed" (fun () -> not (alive pid))))

let with_putenv vars f =
  let saved = List.map (fun (k, _) -> k, Sys.getenv_opt k) vars in
  List.iter (fun (k, v) -> Unix.putenv k v) vars;
  Fun.protect f ~finally:(fun () ->
    List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)

let read_pid path =
  int_of_string (String.trim (In_channel.with_open_text path In_channel.input_all))

let file_ready path =
  Sys.file_exists path
  && String.trim (In_channel.with_open_text path In_channel.input_all) <> ""

(* Review finding: when the group leader died on TERM, the wrapper used
   to cancel the watchdog before its KILL, so a descendant that ignores
   TERM survived and the lock was released under it. *)
let test_stream_stop_kills_term_ignoring_descendant () =
  with_env (fun ~base ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let pidfile = Filename.concat base "stubborn.pid" in
      let local_pid = ref None in
      Eio.Fiber.both
        (fun () ->
          ignore (stream ~sw ~mgr ~ssh ~on_spawn:(fun p -> local_pid := Some p)
            ["sh"; "-c";
             Printf.sprintf "sh -c 'trap \"\" TERM; echo $$ > %s; exec sleep 300' & \
                             sleep 300" (Filename.quote pidfile)]))
        (fun () ->
          wait_until ~clock "descendant started" (fun () -> file_ready pidfile);
          Unix.kill (Option.get !local_pid) Sys.sigterm);
      let stubborn = read_pid pidfile in
      wait_until ~clock ~timeout:12.0 "TERM-ignoring descendant killed"
        (fun () -> not (alive stubborn))))

(* Missing tools are setup failures, not "busy" (flock) or "the command
   exited 127" (setsid). PATH is exported before the checks, so an empty
   or partial directory hides the tools from the remote script only. *)
let test_stream_missing_tools () =
  with_env (fun ~base ~ssh ->
    with_eio (fun ~sw ~mgr ~clock:_ ->
      let empty = Filename.concat base "empty-bin" in
      Unix.mkdir empty 0o755;
      let only_flock = Filename.concat base "flock-bin" in
      Unix.mkdir only_flock 0o755;
      (match Discord_agents.Resource.executable_in_path "flock" with
       | Some flock -> Unix.symlink flock (Filename.concat only_flock "flock")
       | None -> Alcotest.skip ());
      let check_missing label path tool =
        match stream ~sw ~mgr ~ssh ~env:["PATH", path] ["true"] with
        | Error (Host_exec.Setup_failed detail) ->
          Alcotest.(check bool) (label ^ ": " ^ detail) true
            (Discord_agents.Resource.contains_substring ~haystack:detail
               ~needle:("missing_tool:" ^ tool))
        | Ok () -> Alcotest.failf "%s: expected Setup_failed" label
        | Error f -> Alcotest.failf "%s: got %s" label (failure_name f)
      in
      check_missing "no flock" empty "flock";
      check_missing "no setsid" only_flock "setsid"))

(* A stop that lands before the command started is a lost connection
   (we killed it), not a remote setup failure. *)
let test_stream_stop_before_start () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock:_ ->
      check_error "killed at spawn" "Transport_lost"
        (stream ~sw ~mgr ~ssh ~on_spawn:(fun p -> Unix.kill p Sys.sigkill)
           ["sleep"; "300"])))

let test_stream_empty_lines_survive_lost_connection () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let lines = ref [] and local_pid = ref None in
      Eio.Fiber.both
        (fun () ->
          check_error "lost" "Transport_lost"
            (stream ~sw ~mgr ~ssh ~on_spawn:(fun p -> local_pid := Some p)
               ~on_line:(fun l -> lines := l :: !lines)
               ["sh"; "-c"; "printf 'a\\n\\n\\n'; sleep 300"]))
        (fun () ->
          wait_until ~clock "output seen" (fun () -> !lines <> []);
          Eio.Time.sleep clock 0.3;
          Unix.kill (Option.get !local_pid) Sys.sigterm);
      Alcotest.(check (list string)) "trailing blanks kept" ["a"; ""; ""]
        (List.rev !lines)))

let test_stream_on_spawn_exception () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock:_ ->
      match stream ~sw ~mgr ~ssh ~on_spawn:(fun _ -> failwith "spawn hook")
              ["sleep"; "300"] with
      | _ -> Alcotest.fail "expected the hook's exception"
      | exception Failure msg ->
        Alcotest.(check string) "propagated" "spawn hook" msg))

(* Cancelling the fiber must stop the remote command, not just abandon it. *)
let test_stream_cancellation_stops_remote () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let remote_pid = ref None in
      Eio.Fiber.first
        (fun () ->
          ignore (stream ~sw ~mgr ~ssh
            ~on_remote_pid:(fun p -> remote_pid := Some p) ["sleep"; "300"]))
        (fun () -> wait_until ~clock "started" (fun () -> Option.is_some !remote_pid));
      let pid = Option.get !remote_pid in
      wait_until ~clock "remote command killed" (fun () -> not (alive pid))))

(* sshd runs the bootstrap with the user's login shell, which may print
   before anything else. *)
let test_bash_login_shell_with_banner () =
  with_env (fun ~base:_ ~ssh ->
    with_putenv ["FAKE_SSH_LOGIN_SHELL", "bash"; "FAKE_SSH_BANNER", "1"] (fun () ->
      in_eio (fun () ->
        check_ok "capture" "it's $x"
          (Host_exec.run_capture ~ssh remote ["printf"; "%s"; "it's $x"]));
      with_eio (fun ~sw ~mgr ~clock:_ ->
        let lines = ref [] in
        check_ok "stream" "" (Result.map (fun () -> "")
          (stream ~sw ~mgr ~ssh ~on_line:(fun l -> lines := l :: !lines)
             ["echo"; "hello"]));
        Alcotest.(check (list string)) "lines" ["hello"] (List.rev !lines))))

(* The lock path is fixed before the exports, so a run whose env sets
   HOME still contends with the first. *)
let test_env_home_does_not_move_lock () =
  with_env (fun ~base ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let started = ref false in
      Eio.Fiber.both
        (fun () ->
          ignore (stream ~sw ~mgr ~ssh ~lock_key:"same"
                    ~on_remote_pid:(fun _ -> started := true) ["sleep"; "2"]))
        (fun () ->
          wait_until ~clock "first run started" (fun () -> !started);
          check_error "still busy" "Busy"
            (stream ~sw ~mgr ~ssh ~lock_key:"same"
               ~env:["HOME", Filename.concat base "elsewhere"] ["true"]))))

(* After a stop, a retry of the same session must not be refused as busy
   for a fixed grace period when the group died at TERM straight away. *)
let test_lock_released_promptly_after_stop () =
  with_env (fun ~base:_ ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let local_pid = ref None and started = ref false in
      Eio.Fiber.both
        (fun () ->
          ignore (stream ~sw ~mgr ~ssh ~lock_key:"again"
                    ~on_spawn:(fun p -> local_pid := Some p)
                    ~on_remote_pid:(fun _ -> started := true) ["sleep"; "300"]))
        (fun () ->
          wait_until ~clock "started" (fun () -> !started);
          Unix.kill (Option.get !local_pid) Sys.sigterm);
      let t0 = Unix.gettimeofday () in
      let rec retry () =
        match stream ~sw ~mgr ~ssh ~lock_key:"again" ["true"] with
        | Ok () -> Unix.gettimeofday () -. t0
        | Error Host_exec.Busy when Unix.gettimeofday () -. t0 < 5.0 ->
          Eio.Time.sleep clock 0.1; retry ()
        | Error f -> Alcotest.failf "retry: %s" (failure_name f)
      in
      let waited = retry () in
      Alcotest.(check bool)
        (Printf.sprintf "lock free after %.2fs, well before the 3 s grace" waited)
        true (waited < 1.5);
      (* The stop's disarm file is swept by the next run of the key. *)
      let run_dir = Filename.concat (Sys.getenv "HOME") ".cache/discord-agents/run" in
      let leftovers = Sys.readdir run_dir |> Array.to_list
                      |> List.filter (fun f -> Filename.check_suffix f ".done") in
      Alcotest.(check (list string)) "no done-files left" [] leftovers))

(* A normal exit disarms the watchdog: a properly detached background
   process outlives the turn, as it would locally. *)
let test_normal_exit_leaves_detached_process () =
  with_env (fun ~base ~ssh ->
    with_eio (fun ~sw ~mgr ~clock ->
      let pidfile = Filename.concat base "daemon.pid" in
      check_ok "turn" "" (Result.map (fun () -> "")
        (stream ~sw ~mgr ~ssh
           ["sh"; "-c"; Printf.sprintf "sleep 30 >/dev/null 2>&1 & echo $! > %s"
                          (Filename.quote pidfile)]));
      Eio.Time.sleep clock 1.0;
      let daemon = read_pid pidfile in
      Alcotest.(check bool) "detached process still running" true (alive daemon);
      (try Unix.kill daemon Sys.sigkill with Unix.Unix_error _ -> ())))

let () =
  Alcotest.run "host_exec" [
    "remote_script", [
      Alcotest.test_case "quote roundtrip" `Quick test_quote_roundtrip;
      Alcotest.test_case "script rejects" `Quick test_script_rejects;
      Alcotest.test_case "parse marker" `Quick test_parse_marker;
      Alcotest.test_case "classify capture" `Quick test_classify_capture;
    ];
    "capture", [
      Alcotest.test_case "arguments roundtrip" `Quick test_capture_roundtrips_arguments;
      Alcotest.test_case "large payload" `Quick test_capture_large_payload;
      Alcotest.test_case "failures" `Quick test_capture_failures;
      Alcotest.test_case "local unchanged" `Quick test_local_capture_unchanged;
    ];
    "streaming", [
      Alcotest.test_case "lines and status" `Quick test_stream_lines_and_status;
      Alcotest.test_case "setup failures" `Quick test_stream_setup_failures;
      Alcotest.test_case "busy" `Quick test_stream_busy;
      Alcotest.test_case "stop kills remote group" `Quick
        test_stream_stop_kills_remote_group;
      Alcotest.test_case "callback exception stops remote" `Quick
        test_stream_callback_exception_stops_remote;
      Alcotest.test_case "stop kills TERM-ignoring descendant" `Quick
        test_stream_stop_kills_term_ignoring_descendant;
      Alcotest.test_case "missing tools" `Quick test_stream_missing_tools;
      Alcotest.test_case "stop before start" `Quick test_stream_stop_before_start;
      Alcotest.test_case "empty lines survive lost connection" `Quick
        test_stream_empty_lines_survive_lost_connection;
      Alcotest.test_case "on_spawn exception" `Quick test_stream_on_spawn_exception;
      Alcotest.test_case "cancellation stops remote" `Quick
        test_stream_cancellation_stops_remote;
      Alcotest.test_case "bash login shell with banner" `Quick
        test_bash_login_shell_with_banner;
      Alcotest.test_case "env HOME does not move lock" `Quick
        test_env_home_does_not_move_lock;
      Alcotest.test_case "lock released promptly after stop" `Quick
        test_lock_released_promptly_after_stop;
      Alcotest.test_case "normal exit leaves detached process" `Quick
        test_normal_exit_leaves_detached_process;
    ];
  ]
