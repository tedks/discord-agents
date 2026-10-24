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
    let pidfile = Filename.concat dir "pid" in
    let run = Process.run ~clock ~timeout_s:0.15 ~max_output_bytes:16 in
    let command tail = ["sh"; "-c"; Printf.sprintf
      "sleep 20 >/dev/null 2>&1 & echo $! > %s; %s" (Filename.quote pidfile) tail] in
    (match run (command "printf ok") with
     | Ok output ->
       Alcotest.(check string) "success output" "ok" output.text;
       Alcotest.(check bool) "success status" true (output.status = Unix.WEXITED 0)
     | Error _ -> Alcotest.fail "successful probe failed");
    await_stopped clock pidfile;
    Alcotest.(check bool) "overflow stops the group" true
      (run (command "while :; do printf 01234567890123456789; done")
       = Error Process.Output_limit_exceeded);
    await_stopped clock pidfile;
    Alcotest.(check bool) "closed output does not evade timeout" true
      (run (command "exec 1>&- 2>&-; wait") = Error Process.Timed_out);
    await_stopped clock pidfile;
    (* Cancellation propagates, with the same ownership cleanup. *)
    let result = Eio.Time.Timeout.run (Eio.Time.Timeout.seconds clock 0.05)
      (fun () -> Result.map_error (fun _ -> `Unexpected)
        (Process.run ~clock ~timeout_s:10. ~max_output_bytes:16 (command "wait"))) in
    Alcotest.(check bool) "caller cancellation" true (result = Error `Timeout);
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
  with_clock (fun clock ->
    let fds = ref [] in
    Fun.protect ~finally:(fun () -> List.iter Unix.close !fds) (fun () ->
      for _ = 1 to 1100 do
        fds := Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 :: !fds
      done;
      match Process.run ~clock ~timeout_s:1. ~max_output_bytes:3 ["printf"; "yes"] with
      | Ok output -> Alcotest.(check string) "capture above FD_SETSIZE" "yes" output.text
      | Error _ -> Alcotest.fail "capture with high fds failed"))

let test_missing_supervision_tools () =
  with_dir (fun dir ->
    let executable name = Option.get (Discord_agents.Resource.executable_in_path name) in
    let python = executable "python3" in
    List.iter (fun name -> Unix.symlink (executable name) (Filename.concat dir name))
      ["sh"; "base64"];
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
         | Error (Host (Setup_failed _)) -> true | _ -> false);
      let path = Sys.getenv "PATH" in
      Fun.protect ~finally:(fun () -> Unix.putenv "PATH" path) (fun () ->
        Unix.putenv "PATH" dir;
        Alcotest.(check bool) "missing local setsid refused" true
          (match Probe.run ~clock ~timeout_s:1 ~max_output_bytes:100 local args with
           | Error (Process (Setup_failed _)) -> true | _ -> false));
      Alcotest.(check bool) "probe never launched" false (Sys.file_exists marker)))

let () = Alcotest.run "host_probe" [
  "transport", [
    Alcotest.test_case "exact bytes local and ssh" `Quick test_roundtrip;
    Alcotest.test_case "refusals and failure stages" `Quick test_refusals;
    Alcotest.test_case "time and memory limits" `Quick test_limits;
    Alcotest.test_case "executor cleans up after success and disconnect" `Quick test_remote_supervision;
    Alcotest.test_case "missing supervision tools refuse launch" `Quick test_missing_supervision_tools;
  ];
  "supervision", [
    Alcotest.test_case "cleanup on exit overflow timeout cancellation" `Quick test_process_cleanup;
    Alcotest.test_case "duplex input and descriptor ownership" `Quick test_duplex_and_fds;
    Alcotest.test_case "high file descriptors" `Quick test_high_fds;
  ];
]
