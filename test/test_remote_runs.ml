(** Remote agent turns: target resolution, MCP callback wiring, and a
    full turn through the SSH path.

    The SSH path runs through test/fake_ssh.py (see test_host_exec.ml),
    and the "agent" is a fake claude script on PATH that records its
    argv and prints stream-json, so a whole remote turn is exercised
    without network or a real CLI. *)

module Agent_process = Discord_agents.Agent_process
module Agent_runner = Discord_agents.Agent_runner
module Host = Discord_agents.Host

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } ->
    Sys.readdir path |> Array.iter (fun n -> rm_rf (Filename.concat path n));
    Unix.rmdir path
  | _ -> Unix.unlink path

let write_exe path body =
  Out_channel.with_open_text path (fun oc -> output_string oc body);
  Unix.chmod path 0o755

let read path = In_channel.with_open_bin path In_channel.input_all

(* A private HOME, a fake ssh, and a fake claude first on PATH. The fake
   claude writes its argv (one per line) to $HOME/claude.argv and prints
   a two-event stream-json turn. *)
let with_env f =
  let base = Filename.temp_dir "discord_agents_remote_runs_" "" in
  let saved = List.map (fun k -> k, Sys.getenv_opt k)
      ["HOME"; "PATH"; "DISCORD_AGENTS_MCP_COMMAND"] in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved;
      rm_rf base)
    (fun () ->
      Unix.putenv "HOME" base;
      let bin = Filename.concat base "bin" in
      Unix.mkdir bin 0o755;
      let relay = Filename.concat (Sys.getcwd ()) "fake_ssh.py" in
      let ssh = Filename.concat bin "fake-ssh" in
      write_exe ssh (Printf.sprintf "#!/bin/sh\nexec python3 -I %s \"$@\"\n"
                       (Filename.quote relay));
      write_exe (Filename.concat bin "claude") {|#!/bin/sh
for a in "$@"; do printf '%s\n' "$a"; done > "$HOME/claude.argv"
pwd > "$HOME/claude.cwd"
echo '{"type":"assistant","message":{"content":[{"type":"text","text":"hello from remote"}]}}'
echo '{"type":"result","result":"hello from remote","session_id":"s-1"}'
|};
      (* A real executable for mcp_command_opt to resolve; never run. *)
      let mcp = Filename.concat bin "discord agents-mcp" in
      write_exe mcp "#!/bin/sh\nexit 0\n";
      Unix.putenv "DISCORD_AGENTS_MCP_COMMAND" mcp;
      Unix.putenv "PATH"
        (bin ^ ":" ^ Option.value (List.assoc "PATH" saved) ~default:"/usr/bin:/bin");
      let work = Filename.concat base "work" in
      Unix.mkdir work 0o755;
      f ~base ~ssh ~mcp ~work)

let remote = Agent_process.Remote {
  host = "framework0"; destination = "remote"; writer_ssh = "tower0";
}

(* ── Pure ────────────────────────────────────────────────────────── *)

let test_remote_mcp_command () =
  let command, args =
    Agent_process.remote_mcp_command ~writer_ssh:"tower0"
      ~thread_id:"123" ~mcp_path:"/srv/my build/mcp_server.exe" in
  Alcotest.(check string) "ssh" "ssh" command;
  (match List.rev args with
   | remote_cmd :: dest :: "--" :: _ ->
     Alcotest.(check string) "writer destination" "tower0" dest;
     Alcotest.(check string) "quoted remote command"
       "env DISCORD_AGENTS_THREAD_ID='123' '/srv/my build/mcp_server.exe'"
       remote_cmd
   | _ -> Alcotest.fail "expected ... -- dest cmd");
  Alcotest.(check bool) "batch mode" true (List.mem "BatchMode=yes" args);
  (* The writer's login shell parses the command: a hostile thread id
     must stay one word. *)
  let _, args =
    Agent_process.remote_mcp_command ~writer_ssh:"w"
      ~thread_id:"1'; touch /tmp/x; '" ~mcp_path:"/m" in
  Alcotest.(check string) "thread id quoted"
    {|env DISCORD_AGENTS_THREAD_ID='1'\''; touch /tmp/x; '\''' '/m'|}
    (List.nth args (List.length args - 1))

let test_codex_overrides_for_remote () =
  let o = Agent_process.codex_mcp_overrides_for ~command:"ssh"
      ~args:["-T"; "say \"hi\""] () in
  Alcotest.(check (list string)) "toml overrides"
    ["-c"; {|mcp_servers.discord_agents.command="ssh"|};
     "-c"; {|mcp_servers.discord_agents.args=["-T", "say \"hi\""]|}] o;
  Alcotest.(check (list string)) "local form unchanged"
    ["-c"; {|mcp_servers.discord_agents.command="/x"|};
     "-c"; {|mcp_servers.discord_agents.args=[]|}]
    (Agent_process.codex_mcp_overrides_for ~command:"/x" ())

let test_remote_failure_message () =
  let msg = Agent_process.remote_failure_message ~host:"framework0" in
  (* Claude session recovery matches the agent's stderr inside this. *)
  Alcotest.(check string) "exited keeps the local shape"
    "agent exited with code 1 on framework0: No conversation found with session ID: x"
    (msg (Discord_agents.Host_exec.Exited
            (1, "No conversation found with session ID: x")));
  Alcotest.(check string) "unreachable names the host"
    "framework0 is unreachable: no route"
    (msg (Discord_agents.Host_exec.Unreachable "no route"))

(* ── A full remote turn ──────────────────────────────────────────── *)

let run_turn ~ssh ~work ?(on_pid = ignore) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let r = Agent_process.run_streaming ~sw ~env ~target:remote ~ssh
      ~working_dir:work ~kind:Discord_agents.Config.Claude
      ~session_id:"s-1" ~session_id_confirmed:true ~thread_id:"4242"
      ~message_count:1 ~prompt:"hi there"
      ~on_event:(fun e -> events := e :: !events) ~on_pid () in
  r, List.rev !events

let test_claude_turn_over_ssh () =
  with_env (fun ~base ~ssh ~mcp ~work ->
    let pid = ref None in
    let r, events = run_turn ~ssh ~work ~on_pid:(fun p -> pid := Some p) () in
    (match r with
     | Ok () -> ()
     | Error e -> Alcotest.failf "turn failed: %s" e);
    Alcotest.(check bool) "local ssh pid reported" true (Option.is_some !pid);
    let texts = List.filter_map (function
      | Agent_process.Result { text; _ } -> Some text
      | _ -> None) events in
    Alcotest.(check (list string)) "result event" ["hello from remote"] texts;
    Alcotest.(check string) "ran in the session's working dir"
      work (String.trim (read (Filename.concat base "claude.cwd")));
    let argv = String.split_on_char '\n' (read (Filename.concat base "claude.argv")) in
    (* Inline MCP config naming ssh back to the writer, with the thread id,
       and no local setsid in the remote argv. *)
    let rec after_flag = function
      | "--mcp-config" :: json :: _ -> Some json
      | _ :: rest -> after_flag rest
      | [] -> None
    in
    Alcotest.(check bool) "no setsid in remote argv" false
      (List.exists (fun a -> Filename.basename a = "setsid") argv);
    match after_flag argv with
    | None -> Alcotest.fail "no --mcp-config"
    | Some json ->
      let open Yojson.Safe.Util in
      let entry = Yojson.Safe.from_string json
                  |> member "mcpServers" |> member "discord-agents" in
      Alcotest.(check string) "command" "ssh" (entry |> member "command" |> to_string);
      let args = entry |> member "args" |> to_list |> List.map to_string in
      Alcotest.(check string) "remote command"
        (Printf.sprintf "env DISCORD_AGENTS_THREAD_ID='4242' %s"
           (Discord_agents.Remote_script.quote mcp))
        (List.nth args (List.length args - 1)))

let test_unreachable_turn () =
  with_env (fun ~base:_ ~ssh ~mcp:_ ~work ->
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let target = Agent_process.Remote {
      host = "framework0"; destination = "unreachable"; writer_ssh = "w" } in
    match Agent_process.run_streaming ~sw ~env ~target ~ssh
            ~working_dir:work ~kind:Discord_agents.Config.Codex
            ~session_id:"x" ~thread_id:"1" ~message_count:0 ~prompt:"p"
            ~on_event:ignore () with
    | Ok () -> Alcotest.fail "expected failure"
    | Error e ->
      Alcotest.(check bool) ("names the host: " ^ e) true
        (Discord_agents.Resource.contains_substring ~haystack:e
           ~needle:"framework0 is unreachable"))

let test_remote_attachment_saved_in_worktree () =
  with_env (fun ~base:_ ~ssh ~mcp:_ ~work ->
    Eio_main.run @@ fun _env ->
    let data = String.init 300_000 (fun i -> Char.chr (i land 0xff)) in
    match Agent_runner.save_attachment ~ssh ~target:remote ~working_dir:work
            ~name:"123_it's a file.bin" data with
    | Error e -> Alcotest.failf "save failed: %s" e
    | Ok path ->
      Alcotest.(check string) "path in remote worktree"
        (Filename.concat work ".discord-attachments/123_it's a file.bin") path;
      Alcotest.(check bool) "bytes intact" true (read path = data))

let test_remote_gemini_settings () =
  with_env (fun ~base:_ ~ssh ~mcp:_ ~work ->
    Eio_main.run @@ fun _env ->
    ignore (Sys.command (Printf.sprintf "git -C %s init -q" (Filename.quote work)));
    Unix.mkdir (Filename.concat work ".gemini") 0o755;
    Out_channel.with_open_text (Filename.concat work ".gemini/settings.json")
      (fun oc -> output_string oc
          {|{"theme":"dark","mcpServers":{"other":{"command":"x"}}}|});
    let host = { (Host.local_host "framework0") with
                 Host.transport = Host.Ssh "remote" } in
    Agent_process.setup_gemini_mcp_remote ~ssh ~host ~working_dir:work
      ~command:"ssh" ~args:["--"; "tower0"; "env X=1 /m"] ();
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_file (Filename.concat work ".gemini/settings.json") in
    Alcotest.(check string) "user key kept" "dark" (json |> member "theme" |> to_string);
    Alcotest.(check string) "other server kept" "x"
      (json |> member "mcpServers" |> member "other" |> member "command" |> to_string);
    Alcotest.(check string) "our server" "ssh"
      (json |> member "mcpServers" |> member "discord-agents" |> member "command"
       |> to_string);
    let exclude = read (Filename.concat work ".git/info/exclude") in
    Alcotest.(check bool) "excluded from git" true
      (Agent_process.exclude_already_lists_gemini exclude))

let () =
  Alcotest.run "remote_runs" [
    "pure", [
      Alcotest.test_case "remote mcp command" `Quick test_remote_mcp_command;
      Alcotest.test_case "codex overrides" `Quick test_codex_overrides_for_remote;
      Alcotest.test_case "failure message" `Quick test_remote_failure_message;
    ];
    "over ssh", [
      Alcotest.test_case "claude turn" `Quick test_claude_turn_over_ssh;
      Alcotest.test_case "unreachable" `Quick test_unreachable_turn;
      Alcotest.test_case "attachment" `Quick test_remote_attachment_saved_in_worktree;
      Alcotest.test_case "gemini settings" `Quick test_remote_gemini_settings;
    ];
  ]
