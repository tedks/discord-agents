module C = Discord_agents.Host_capability
module H = Discord_agents.Host_callback
module Health = Discord_agents.Mcp_health
module Host = Discord_agents.Host
module Client = Discord_agents.Control_client

let host = Host.local_host "writer"
let context = { H.host; cwd = "/project"; writer = "writer";
  mcp_path = Some "/bot/mcp"; socket_path = "/config/control.sock" }
let check label expected actual = Alcotest.(check bool) label true (expected = actual)
let now () = 100.

let test_request () =
  List.iter (fun (response, expected) ->
    let client = Client.make ~request:(fun request ->
      check "only health" "health" request.Client.method_name;
      check "no session params" None request.params;
      check "socket deadline" 2 request.timeout_s;
      response) in
    check "response" expected (Health.check client)) [
      Ok (`Assoc ["ok", `Bool true; "private", `String "SECRET"]), true;
      Ok (`Assoc ["ok", `Bool true; "ok", `Bool true]), false;
      Ok (`Assoc ["ok", `Bool true; "error", `String "SECRET"]), false;
      Ok (`Assoc ["ok", `Bool false]), false;
      Ok (`Assoc []), false; Ok `Null, false; Error "SECRET", false;
    ]

let test_observations () =
  let calls = ref 0 in
  let run result ~timeout_s ~max_output_bytes ~cwd target argv =
    incr calls;
    check "deadline" 8 timeout_s; check "output cap" 1024 max_output_bytes;
    check "host" host target; check "cwd" context.cwd cwd;
    check "local executable and socket" ["/bot/mcp"; "--check-control"; "/config/control.sock"] argv;
    result in
  let snapshot = H.observe ~run:(run (Ok H.reply)) ~now context in
  check "control reachable" (C.Known ()) snapshot.control;
  check "fresh" snapshot (H.current ~now:101. ~max_age:10. ~context snapshot);
  List.iter (fun (now, max_age) -> check "stale" (C.Unknown Stale)
    (H.current ~now ~max_age ~context snapshot).control)
    [111.,10.; 99.,10.; nan,10.; 100.,infinity];
  check "changed callback path" (C.Unknown Context_changed)
    (H.current ~now:100. ~max_age:10. ~context:{ context with socket_path = "/other" } snapshot).control;
  check "noise is not success" (C.Unknown Malformed)
    (H.observe ~run:(run (Ok ("banner\n" ^ H.reply))) ~now context).control;
  check "timeout unknown" (C.Unknown Timed_out)
    (H.observe ~run:(run (Error (Discord_agents.Host_probe.Process Timed_out))) ~now context).control;
  calls := 0;
  check "disabled" (C.Unknown Disabled)
    (H.observe ~run:(run (Ok H.reply)) ~now { context with host = { host with enabled = false } }).control;
  check "missing executable" (C.Unknown Missing_executable)
    (H.observe ~run:(run (Ok H.reply)) ~now { context with mcp_path = None }).control;
  check "invalid socket" (C.Unknown Invalid_context)
    (H.observe ~run:(run (Ok H.reply)) ~now { context with socket_path = "relative" }).control;
  check "invalid reverse destination" (C.Unknown Invalid_context)
    (H.observe ~run:(run (Ok H.reply)) ~now { context with
      host = { host with transport = Ssh "executor"; writer_ssh = Some "bad destination" } }).control;
  check "no refused probe spawned" 0 !calls

let test_reverse_command () =
  let remote = { host with Host.transport = Ssh "executor"; writer_ssh = Some "writer-alias" } in
  let context = { context with host = remote; mcp_path = Some "/bot's/mcp" } in
  match H.command context with
  | Ok argv ->
    check "same SSH options as agent callback"
      (["ssh"] @ Discord_agents.Host_exec.ssh_options @
       ["--"; "writer-alias"; {|'/bot'\''s/mcp' '--check-control' '/config/control.sock'|}]) argv
  | Error _ -> Alcotest.fail "valid remote callback refused"

let test_transport () =
  let dir = Filename.temp_dir "callback ' " "" in
  let app = Filename.concat dir "discord-agents" in
  Unix.mkdir app 0o700;
  let socket_path = Filename.concat app "control.sock" in
  let old_path = Sys.getenv "PATH" in
  let ssh = Filename.concat dir "ssh" in
  let oc = open_out ssh in
  Printf.fprintf oc "#!/bin/sh\nexec python3 -I %s \"$@\"\n"
    (Filename.quote (Filename.concat (Sys.getcwd ()) "fake_ssh.py"));
  close_out oc; Unix.chmod ssh 0o755;
  let mcp_path = Some (Filename.concat (Filename.dirname (Sys.getcwd ())) "bin/mcp_server.exe") in
  Fun.protect ~finally:(fun () ->
    Unix.putenv "PATH" old_path;
    if Sys.file_exists socket_path then Unix.unlink socket_path;
    Unix.unlink ssh; Unix.rmdir app; Unix.rmdir dir) (fun () ->
    Unix.putenv "PATH" (dir ^ ":" ^ old_path);
    List.iter (fun target ->
      List.iter (fun (response, expected) ->
        let server = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Unix.bind server (Unix.ADDR_UNIX socket_path); Unix.listen server 1;
        let pid = Unix.fork () in
        if pid = 0 then begin
          ignore (Unix.alarm 4);
          let client, _ = Unix.accept server in
          let line = input_line (Unix.in_channel_of_descr client) in
          if Yojson.Safe.from_string line <> `Assoc ["method", `String "health"] then Unix._exit 20;
          let bytes = response ^ "\n" in
          ignore (Unix.write_substring client bytes 0 (String.length bytes));
          Unix.close client; Unix.close server; Unix._exit 0
        end;
        Unix.close server;
        let reaped = ref false in
        Fun.protect ~finally:(fun () ->
          if not !reaped then begin
            let waited, _ = Unix.waitpid [Unix.WNOHANG] pid in
            if waited = 0 then (Unix.kill pid Sys.sigkill; ignore (Unix.waitpid [] pid))
          end;
          Unix.unlink socket_path) (fun () ->
          Eio_main.run (fun env ->
            let snapshot = H.collect ~clock:(Eio.Stdenv.mono_clock env) ~now ~ssh
              { context with host = target; cwd = dir; mcp_path; socket_path } in
            check "end-to-end route" expected snapshot.control;
            Alcotest.(check bool) "health details not retained" false
              (Discord_agents.Resource.contains_substring ~haystack:(Marshal.to_string snapshot []) ~needle:"SECRET"));
          let _, status = Unix.waitpid [] pid in
          reaped := true;
          check "served health request" (Unix.WEXITED 0) status))
        [ {|{"ok":true,"private":"SECRET"}|}, C.Known ();
          {|{"error":"SECRET"}|}, C.Unknown Command_failed ])
      [host; { host with transport = Host.Ssh "executor" }];
    Eio_main.run (fun env ->
      let snapshot = H.collect ~clock:(Eio.Stdenv.mono_clock env) ~now ~ssh
        { context with cwd = dir; mcp_path; socket_path } in
      check "missing socket is not reachable" (C.Unknown Command_failed) snapshot.control;
      let reverse = H.collect ~clock:(Eio.Stdenv.mono_clock env) ~now ~ssh
        { context with host = { host with transport = Ssh "executor"; writer_ssh = Some "unreachable" };
          cwd = dir; mcp_path; socket_path } in
      check "broken reverse SSH is not reachable" (C.Unknown Command_failed) reverse.control;
      match Discord_agents.Probe_process.run ~clock:(Eio.Stdenv.mono_clock env)
        ~timeout_s:2. ~max_output_bytes:4096
        ~input:"{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}\n" [Option.get mcp_path] with
      | Ok output ->
        check "stdio mode exit" (Unix.WEXITED 0) output.status;
        let json = Yojson.Safe.from_string output.text in
        check "stdio mode still serves MCP"
          (`Assoc ["jsonrpc", `String "2.0"; "id", `Int 2; "result", `Assoc []]) json
      | Error _ -> Alcotest.fail "ordinary MCP mode failed"))

let () = Alcotest.run "host_callback" ["callback", [
  Alcotest.test_case "private health response validation" `Quick test_request;
  Alcotest.test_case "bounded dated observations" `Quick test_observations;
  Alcotest.test_case "quoted reverse callback command" `Quick test_reverse_command;
  Alcotest.test_case "real MCP local and double fake-SSH route" `Quick test_transport;
]]
