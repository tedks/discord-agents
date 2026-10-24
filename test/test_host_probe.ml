module Probe = Discord_agents.Host_probe
module Process = Discord_agents.Probe_process
module Host = Discord_agents.Host
module Exec = Discord_agents.Host_exec

let local = Host.local_host "local"
let remote = { local with Host.name = "remote"; transport = Host.Ssh "remote" }

let with_clock f = Eio_main.run (fun env -> f (Eio.Stdenv.mono_clock env))

let with_dir f =
  let dir = Filename.temp_dir "discord_agents_probe_" "" in
  Fun.protect ~finally:(fun () ->
    (* Only fixtures created by this test live in this private directory. *)
    Sys.readdir dir |> Array.iter (fun name -> Unix.unlink (Filename.concat dir name));
    Unix.rmdir dir) (fun () -> f dir)

let fake_ssh dir =
  let path = Filename.concat dir "ssh" in
  let oc = open_out path in
  Printf.fprintf oc "#!/bin/sh\nexec python3 -I %s \"$@\"\n"
    (Filename.quote (Filename.concat (Sys.getcwd ()) "fake_ssh.py"));
  close_out oc;
  Unix.chmod path 0o755;
  path

let failure = function
  | Probe.Disabled -> "disabled"
  | Process Timed_out -> "timeout"
  | Process Output_limit_exceeded -> "output limit"
  | Process (Setup_failed message) -> "setup: " ^ message
  | Process (Io_failed message) -> "I/O: " ^ message
  | Host error -> Exec.describe_failure ~host:"test" error

let ok expected = function
  | Ok actual -> Alcotest.(check string) "bytes" expected actual
  | Error error -> Alcotest.fail (failure error)

let test_roundtrip () =
  with_dir (fun dir ->
    let ssh = fake_ssh dir in
    with_clock (fun clock ->
      List.iter (fun host ->
        let run ?(max_output_bytes = 4096) args =
          Probe.run ~clock ~ssh ~timeout_s:2 ~max_output_bytes host args in
        List.iter (fun bytes ->
          ok bytes (run ~max_output_bytes:(String.length bytes) ["printf"; "%s"; bytes]))
          [""; "no newline"; "a\nb\n"; "it's $HOME `id` $(id)"; "é✓"];
        ok "\000\255" (run ~max_output_bytes:2 ["printf"; "\\000\\377"]);
        ok "out\nerr\n" (run ["sh"; "-c"; "echo out; echo err >&2"]);
        (match run ["sh"; "-c"; "printf problem; exit 255"] with
         | Error (Host (Exited (255, "problem"))) -> ()
         | result -> ok "expected command failure" result);
        ok (dir ^ "\n")
          (Probe.run ~clock ~ssh ~timeout_s:2 ~max_output_bytes:4096 ~cwd:dir host ["pwd"]))
        [local; remote]))

let test_refusals () =
  with_dir (fun dir ->
    let ssh = fake_ssh dir in
    with_clock (fun clock ->
      let run ?(timeout_s = 2) ?(max_output_bytes = 64) ?(cwd = ".") host args =
        Probe.run ~clock ~ssh ~timeout_s ~max_output_bytes ~cwd host args in
      Alcotest.(check bool) "disabled host does not spawn" true
        (run { remote with enabled = false } ["touch"; Filename.concat dir "must-not-exist"]
         = Error Probe.Disabled);
      Alcotest.(check bool) "no side effect" false (Sys.file_exists (Filename.concat dir "must-not-exist"));
      List.iter (fun host ->
        List.iter (fun result ->
          Alcotest.(check bool) "refused before spawn" true
            (match result with Error (Probe.Process (Setup_failed _)) -> true | _ -> false))
          [run ~timeout_s:0 host ["true"]; run ~max_output_bytes:(-1) host ["true"];
           run ~max_output_bytes:(1024 * 1024 + 1) host ["true"];
           run host []; run host ["bad\000arg"]; run ~cwd:"relative" host ["true"]];
        Alcotest.(check bool) "missing cwd" true
          (match run ~cwd:(dir ^ "/missing") host ["true"] with
           | Error (Host (Missing_cwd _)) -> true | _ -> false)) [local; remote];
      Alcotest.(check bool) "unreachable remains distinct" true
        (match run { remote with transport = Host.Ssh "unreachable" } ["true"] with
         | Error (Host (Unreachable _)) -> true | _ -> false)))

let running pid =
  match In_channel.with_open_bin (Printf.sprintf "/proc/%d/stat" pid) In_channel.input_all with
  | text ->
    let state = text.[String.rindex text ')' + 2] in
    state <> 'Z' && state <> 'X'
  | exception Sys_error _ -> false

let await_stopped clock path =
  let pid = In_channel.with_open_bin path (fun ic -> int_of_string (input_line ic)) in
  let rec loop remaining =
    if running pid && remaining > 0 then begin
      Eio.Time.Mono.sleep clock 0.02;
      loop (remaining - 1)
    end
  in
  loop 150;
  Alcotest.(check bool) "owned child stopped" false (running pid)

let test_limits () =
  with_dir (fun dir ->
    let ssh = fake_ssh dir in
    with_clock (fun clock ->
      List.iteri (fun i host ->
        let run = Probe.run ~clock ~ssh ~timeout_s:1 ~max_output_bytes:32 host in
        (* A large single line and stderr both count. Never accept a
           truncated success, even when a valid exit trailer arrives. *)
        List.iter (fun args ->
          Alcotest.(check bool) "output over cap" true
            (run args = Error (Probe.Process Output_limit_exceeded)))
          [["printf"; "%s"; String.make 33 'x'];
           ["sh"; "-c"; "printf '%10000s' x >&2"]];
        let pidfile = Filename.concat dir (Printf.sprintf "pid-%d" i) in
        let script = Printf.sprintf
          "sleep 20 & echo $! > %s; wait" (Filename.quote pidfile) in
        Alcotest.(check bool) "hung probe times out" true
          (run ["sh"; "-c"; script] = Error (Probe.Process Timed_out));
        await_stopped clock pidfile;
        (* Root exits successfully, but its child retains the pipe. *)
        let script = Printf.sprintf
          "sleep 20 & echo $! > %s; exit 0" (Filename.quote pidfile) in
        Alcotest.(check bool) "inherited pipe is still bounded" true
          (run ["sh"; "-c"; script] = Error (Probe.Process Timed_out));
        await_stopped clock pidfile) [local; remote]))

let test_process_cleanup () =
  with_dir (fun dir -> with_clock (fun clock ->
    let run = Process.run ~clock ~timeout_s:2. ~max_output_bytes:16 in
    let command pidfile tail = ["sh"; "-c"; Printf.sprintf
      "sleep 20 >/dev/null 2>&1 & echo $! > %s; %s" (Filename.quote pidfile) tail] in
    let pidfile = Filename.concat dir "pid-success" in
    (match run (command pidfile "printf ok") with
     | Ok output ->
       Alcotest.(check string) "success output" "ok" output.text;
       Alcotest.(check bool) "success status" true (output.status = Unix.WEXITED 0)
     | Error _ -> Alcotest.fail "successful probe failed");
    await_stopped clock pidfile;
    let pidfile = Filename.concat dir "pid-overflow" in
    Alcotest.(check bool) "overflow stops the group" true
      (run (command pidfile "while :; do printf 01234567890123456789; done")
       = Error Process.Output_limit_exceeded);
    await_stopped clock pidfile;
    let pidfile = Filename.concat dir "pid-closed" in
    Alcotest.(check bool) "closed output does not evade timeout" true
      (Process.run ~clock ~timeout_s:1. ~max_output_bytes:16
         (command pidfile "exec 1>&- 2>&-; wait") = Error Process.Timed_out);
    await_stopped clock pidfile;
    (* Cancellation propagates, with the same ownership cleanup. *)
    let pidfile = Filename.concat dir "pid-cancel" in
    let rec wait_started () =
      let ready = try
        In_channel.with_open_bin pidfile (fun ic -> Option.is_some (int_of_string_opt (input_line ic)))
        with Sys_error _ | End_of_file -> false in
      if not ready then (Eio.Time.Mono.sleep clock 0.01; wait_started ())
    in
    let result = Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 2.) (fun () ->
      Eio.Fiber.first
        (fun () -> ignore (Process.run ~clock ~timeout_s:10. ~max_output_bytes:16
                     (command pidfile "wait")); `Completed)
        (fun () -> wait_started (); `Cancel)) in
    Alcotest.(check bool) "caller cancelled after child started" true (result = `Cancel);
    await_stopped clock pidfile))

let test_duplex_and_fds () =
  with_clock (fun clock ->
    let input = String.make 200_000 'x' in
    let baseline = Array.length (Sys.readdir "/proc/self/fd") in
    for _ = 1 to 10 do
      let result = Process.run ~clock ~timeout_s:1. ~max_output_bytes:200_010 ~input
        ["sh"; "-c"; "printf before; cat"] in
      match result with
      | Ok output -> Alcotest.(check string) "duplex" ("before" ^ input) output.text
      | Error _ -> Alcotest.fail "duplex capture failed"
    done;
    Alcotest.(check int) "no fd growth" baseline (Array.length (Sys.readdir "/proc/self/fd")))

let test_remote_supervision () =
  with_dir (fun dir ->
    let ssh = fake_ssh dir in
    with_clock (fun clock ->
      let pidfile = Filename.concat dir "remote-pid" in
      let run = Probe.run ~clock ~ssh ~timeout_s:1 ~max_output_bytes:8 remote in
      let command tail = ["sh"; "-c"; Printf.sprintf
        "sleep 20 >/dev/null 2>&1 & echo $! > %s; %s" (Filename.quote pidfile) tail] in
      ok "ok" (run (command "printf ok"));
      await_stopped clock pidfile;
      Alcotest.(check bool) "output overflow disconnects ssh" true
        (run (command "while :; do printf '%10000s' x; done")
         = Error (Probe.Process Output_limit_exceeded));
      await_stopped clock pidfile))

let test_high_fds () =
  let module Limits = Discord_agents.Runtime_limits in
  let before = Limits.get_nofile_limit () in
  if before.hard < 1200L then Alcotest.skip ();
  Fun.protect ~finally:(fun () -> Limits.set_nofile_soft_limit_raw before.soft) (fun () ->
  Limits.set_nofile_soft_limit_raw (max before.soft 1200L);
  with_clock (fun clock ->
    let fds = ref [] in
    Fun.protect ~finally:(fun () -> List.iter Unix.close !fds) (fun () ->
      for _ = 1 to 1100 do
        fds := Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 :: !fds
      done;
      match Process.run ~clock ~timeout_s:1. ~max_output_bytes:3 ["printf"; "yes"] with
      | Ok output -> Alcotest.(check string) "capture above FD_SETSIZE" "yes" output.text
      | Error _ -> Alcotest.fail "capture with high fds failed")))

let test_missing_supervision_tools () =
  with_dir (fun dir ->
    let executable name = Option.get (Discord_agents.Resource.executable_in_path name) in
    let python = executable "python3" in
    List.iter (fun name -> Unix.symlink (executable name) (Filename.concat dir name))
      ["sh"; "base64"; "cat"];
    let ssh = Filename.concat dir "ssh" in
    let oc = open_out ssh in
    Printf.fprintf oc "#!/bin/sh\nPATH=%s; export PATH\nexec %s -I %s \"$@\"\n"
      (Filename.quote dir) (Filename.quote python)
      (Filename.quote (Filename.concat (Sys.getcwd ()) "fake_ssh.py"));
    close_out oc;
    Unix.chmod ssh 0o755;
    with_clock (fun clock ->
      let marker = Filename.concat dir "must-not-run" in
      let args = ["sh"; "-c"; "echo ran > " ^ Filename.quote marker] in
      Alcotest.(check bool) "missing executor timeout refused" true
        (match Probe.run ~clock ~ssh ~timeout_s:1 ~max_output_bytes:100 remote args with
         | Error (Host (Setup_failed message)) ->
           String.starts_with ~prefix:"probe supervision requires timeout" (String.trim message)
         | _ -> false);
      let oc = open_out (Filename.concat dir "timeout") in
      output_string oc "#!/bin/sh\nif [ \"$1\" = --version ]; then echo 'BusyBox timeout'; exit 0; fi\nexit 99\n";
      close_out oc;
      Unix.chmod (Filename.concat dir "timeout") 0o755;
      Alcotest.(check bool) "non-GNU timeout refused" true
        (match Probe.run ~clock ~ssh ~timeout_s:1 ~max_output_bytes:100 remote args with
         | Error (Host (Setup_failed message)) ->
           String.starts_with ~prefix:"probe supervision requires GNU coreutils timeout" (String.trim message)
         | _ -> false);
      let oc = open_out (Filename.concat dir "timeout") in
      output_string oc "#!/bin/sh\nexit 1\n";
      close_out oc;
      Alcotest.(check bool) "failed version check explains requirement" true
        (match Probe.run ~clock ~ssh ~timeout_s:1 ~max_output_bytes:100 remote args with
         | Error (Host (Setup_failed message)) ->
           String.starts_with ~prefix:"probe supervision requires GNU coreutils timeout" (String.trim message)
         | _ -> false);
      let path = Sys.getenv "PATH" in
      Fun.protect ~finally:(fun () -> Unix.putenv "PATH" path) (fun () ->
        Unix.putenv "PATH" dir;
        Alcotest.(check bool) "missing local setsid refused" true
          (match Probe.run ~clock ~timeout_s:1 ~max_output_bytes:100 local args with
           | Error (Process (Setup_failed _)) -> true | _ -> false));
      Alcotest.(check bool) "probe never launched" false (Sys.file_exists marker)))

let test_capture_without_home () =
  with_clock (fun clock ->
    let script = Discord_agents.Remote_script.script ~nonce:"test" ~mode:Capture
      ~cwd:"." ~env:[] ~argv:["printf"; "ok"] |> Result.get_ok in
    match Process.run ~clock ~timeout_s:2. ~max_output_bytes:4096
        ["env"; "-u"; "HOME"; "sh"; "-c"; script] with
    | Ok output ->
      Alcotest.(check bool) "capture needs no HOME" true
        (Exec.classify_capture ~nonce:"test" ~cwd:"."
           ~ssh_status:(Exec.exit_code output.status) output.text = Ok "ok")
    | Error _ -> Alcotest.fail "capture without HOME failed")

let test_auto_reaping_refused () =
  with_clock (fun clock ->
    let previous = Sys.signal Sys.sigchld Sys.Signal_ignore in
    Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigchld previous) (fun () ->
      Alcotest.(check bool) "auto-reaping refused before spawn" true
        (match Process.run ~clock ~timeout_s:1. ~max_output_bytes:10 ["true"] with
         | Error (Process.Setup_failed _) -> true | _ -> false)))

let () = Alcotest.run "host_probe" [
  "transport", [
    Alcotest.test_case "exact bytes local and ssh" `Quick test_roundtrip;
    Alcotest.test_case "refusals and failure stages" `Quick test_refusals;
    Alcotest.test_case "time and memory limits" `Quick test_limits;
    Alcotest.test_case "executor cleans up after success and disconnect" `Quick test_remote_supervision;
    Alcotest.test_case "missing supervision tools refuse launch" `Quick test_missing_supervision_tools;
    Alcotest.test_case "capture without HOME" `Quick test_capture_without_home;
  ];
  "supervision", [
    Alcotest.test_case "cleanup on exit overflow timeout cancellation" `Quick test_process_cleanup;
    Alcotest.test_case "duplex input and descriptor ownership" `Quick test_duplex_and_fds;
    Alcotest.test_case "high file descriptors" `Quick test_high_fds;
    Alcotest.test_case "auto-reaping disposition refused" `Quick test_auto_reaping_refused;
  ];
]
