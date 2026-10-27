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
      (* FAKE_CLAUDE_RECOVER=1: a --resume of an unknown session fails
         the way Claude does, so the caller's recovery retry can be seen. *)
      write_exe (Filename.concat bin "claude") {|#!/bin/sh
for a in "$@"; do printf '%s\n' "$a"; done > "$HOME/claude.argv"
pwd > "$HOME/claude.cwd"
printf '%s' "${DISCORD_AGENTS_THREAD_ID:-unset}" > "$HOME/claude.thread"
echo x >> "$HOME/claude.calls"
if [ "${FAKE_CLAUDE_RECOVER:-}" = 1 ]; then
  for a in "$@"; do
    if [ "$a" = --resume ]; then
      echo "No conversation found with session ID: s-1" >&2; exit 1
    fi
  done
fi
echo '{"type":"assistant","message":{"content":[{"type":"text","text":"hello from remote"}]}}'
echo '{"type":"result","result":"hello from remote","session_id":"s-1"}'
|};
      write_exe (Filename.concat bin "codex") {|#!/bin/sh
for a in "$@"; do printf '%s\n' "$a"; done > "$HOME/codex.argv"
echo '{"type":"thread.started","thread_id":"t-9"}'
echo '{"type":"item.completed","item":{"type":"agent_message","text":"codex says hi"}}'
echo '{"type":"turn.completed","usage":{}}'
|};
      (* A fake agy: records argv and the MCP variable, prints a turn. *)
      write_exe (Filename.concat bin "agy") {|#!/bin/sh
for a in "$@"; do printf '%s\n' "$a"; done > "$HOME/agy.argv"
printf '%s' "${DISCORD_AGENTS_MCP_FOR_AGY:-unset}" > "$HOME/agy.mcp"
echo '{"event":"init","conversation_id":"s-1","init":{}}'
echo '{"event":"step_update","step_update":{"step_index":2,"state":"DONE","step_type":"agent_response","text_delta":"agy says hi"}}'
echo '{"event":"result","result":{"conversation_id":"s-1","status":"SUCCESS","response":"agy says hi"}}'
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

(* Run the MCP command as the agent would, with an ssh stand-in that
   prints its argv, to see exactly what reaches the writer. *)
let run_mcp_command ~thread_id =
  let dir = Filename.temp_dir "discord_agents_mcp_cmd_" "" in
  Fun.protect ~finally:(fun () -> rm_rf dir) @@ fun () ->
  write_exe (Filename.concat dir "ssh")
    "#!/bin/sh\nfor a in \"$@\"; do printf '%s\\n' \"$a\"; done\n";
  let command, args =
    Agent_process.remote_mcp_command ~writer_ssh:"tower0"
      ~mcp_path:"/srv/my build/mcp_server.exe" ~config_home:"/home/u/.config" in
  let env = Array.append [| "PATH=" ^ dir ^ ":/usr/bin:/bin" |]
      (match thread_id with
       | Some id -> [| "DISCORD_AGENTS_THREAD_ID=" ^ id |]
       | None -> [||]) in
  let ((out, _, _) as proc) =
    Unix.open_process_args_full "/bin/sh"
      (Array.of_list ((if command = "sh" then "/bin/sh" else command) :: args)) env in
  let output = In_channel.input_all out in
  let status = Unix.close_process_full proc in
  output, status

let test_remote_mcp_command () =
  let output, status = run_mcp_command ~thread_id:(Some "4242") in
  Alcotest.(check bool) "ran" true (status = Unix.WEXITED 0);
  (match List.rev (String.split_on_char '\n' (String.trim output)) with
   | remote_cmd :: dest :: "--" :: _ ->
     Alcotest.(check string) "writer destination" "tower0" dest;
     (* The thread comes from the agent's environment at run time, so
        the config itself is the same for every thread. *)
     Alcotest.(check string) "remote command"
       "env DISCORD_AGENTS_THREAD_ID=4242 XDG_CONFIG_HOME='/home/u/.config' '/srv/my build/mcp_server.exe'"
       remote_cmd
   | _ -> Alcotest.failf "unexpected ssh argv: %s" output);
  (* Anything but a Discord id is refused before ssh runs, so it can
     never reach the writer's shell unquoted. *)
  List.iter (fun bad ->
    let output, status = run_mcp_command ~thread_id:bad in
    Alcotest.(check bool) "refused" true (status <> Unix.WEXITED 0);
    Alcotest.(check string) "ssh not run" "" output)
    [None; Some ""; Some "1; touch /tmp/x"; Some "12a"]

let test_codex_overrides_for_remote () =
  let o = Agent_process.codex_mcp_overrides_for ~command:"ssh"
      ~args:["-T"; "say \"hi\""] () in
  Alcotest.(check (list string)) "toml overrides"
    ["-c"; {|mcp_servers.discord_agents.command="ssh"|};
     "-c"; {|mcp_servers.discord_agents.args=["-T", "say \"hi\""]|}] o;
  Alcotest.(check (list string)) "local form unchanged"
    ["-c"; {|mcp_servers.discord_agents.command="/x"|};
     "-c"; {|mcp_servers.discord_agents.args=[]|}]
    (Agent_process.codex_mcp_overrides_for ~command:"/x" ());
  Alcotest.(check (list string)) "backslash and control characters"
    ["-c"; {|mcp_servers.discord_agents.command="sh"|};
     "-c"; {|mcp_servers.discord_agents.args=["a\\b", "tab\there", "nl\n", "bell\u0007"]|}]
    (Agent_process.codex_mcp_overrides_for ~command:"sh"
       ~args:["a\\b"; "tab\there"; "nl\n"; "bell\007"] ())

(* The local path passes the thread id the same way (Codex gives MCP
   servers an allowlisted environment, so it must be in the env table). *)
let test_codex_overrides_carry_thread_id () =
  Alcotest.(check (list string)) "env table entry"
    ["-c"; {|mcp_servers.discord_agents.command="/x"|};
     "-c"; {|mcp_servers.discord_agents.args=[]|};
     "-c"; {|mcp_servers.discord_agents.env.DISCORD_AGENTS_THREAD_ID="123"|}]
    (Agent_process.codex_mcp_overrides_for ~thread_id:"123" ~command:"/x" ())

let test_attachment_storage_name () =
  let name = Agent_runner.attachment_storage_name in
  Alcotest.(check string) "plain" "123_report.pdf" (name ~id:"123" ~filename:"report.pdf");
  Alcotest.(check string) "separators and leading dots" "123__.._etc_passwd"
    (name ~id:"123" ~filename:"../../etc/passwd");
  Alcotest.(check string) "leading dots dropped" "123_bashrc"
    (name ~id:"123" ~filename:"..bashrc");
  Alcotest.(check string) "control and quotes" "123_a_b_c"
    (name ~id:"123" ~filename:"a\nb'c");
  Alcotest.(check string) "empty" "123_file" (name ~id:"123" ~filename:"");
  Alcotest.(check int) "bounded" (4 + 120)
    (String.length (name ~id:"123" ~filename:(String.make 500 'x')))

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

let run_turn ~ssh ~work ?(on_pid = ignore) ?(kind = Discord_agents.Config.Claude)
    ?(message_count = 1) ?system_prompt () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let events = ref [] in
  let r = Agent_process.run_streaming ~sw ~env ~target:remote ~ssh
      ~working_dir:work ~kind
      ~session_id:"s-1" ~session_id_confirmed:true ~thread_id:"4242"
      ~message_count ?system_prompt ~prompt:"hi there"
      ~on_event:(fun e -> events := e :: !events) ~on_pid () in
  r, List.rev !events

let test_claude_turn_over_ssh () =
  with_env (fun ~base ~ssh ~mcp ~work ->
    let pid = ref None in
    let r, events = run_turn ~ssh ~work ~on_pid:(fun p -> pid := Some p)
        ~system_prompt:"you are a test" () in
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
    (* As locally, the agent carries the caller's thread id. *)
    Alcotest.(check string) "thread id in the agent's environment"
      "4242" (read (Filename.concat base "claude.thread"));
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
    Alcotest.(check bool) "system prompt passed" true
      (List.mem "you are a test" argv);
    match after_flag argv with
    | None -> Alcotest.fail "no --mcp-config"
    | Some json ->
      let open Yojson.Safe.Util in
      let entry = Yojson.Safe.from_string json
                  |> member "mcpServers" |> member "discord-agents" in
      Alcotest.(check string) "command" "sh" (entry |> member "command" |> to_string);
      let args = entry |> member "args" |> to_list |> List.map to_string in
      Alcotest.(check bool) "server path passed, quoted" true
        (List.mem (Discord_agents.Remote_script.quote mcp) args);
      Alcotest.(check bool) "no thread id baked in" false
        (List.exists (fun a -> Discord_agents.Resource.contains_substring
                                 ~haystack:a ~needle:"4242") args))

(* Claude session recovery matches the agent's stderr inside the failure
   message; it must still trigger when that stderr came over ssh. *)
let test_claude_recovery_over_ssh () =
  with_env (fun ~base ~ssh ~mcp:_ ~work ->
    Unix.putenv "FAKE_CLAUDE_RECOVER" "1";
    Fun.protect ~finally:(fun () -> Unix.putenv "FAKE_CLAUDE_RECOVER" "") @@ fun () ->
    let r, _ = run_turn ~ssh ~work ~message_count:0 () in
    (match r with Ok () -> () | Error e -> Alcotest.failf "not recovered: %s" e);
    Alcotest.(check int) "retried once" 2
      (List.length (String.split_on_char '\n'
                      (String.trim (read (Filename.concat base "claude.calls"))))))

let test_codex_turn_over_ssh () =
  with_env (fun ~base ~ssh ~mcp:_ ~work ->
    let r, events = run_turn ~ssh ~work ~kind:Discord_agents.Config.Codex () in
    (match r with Ok () -> () | Error e -> Alcotest.failf "turn failed: %s" e);
    Alcotest.(check bool) "agent text arrived" true
      (List.exists (function
         | Agent_process.Text_delta t ->
           Discord_agents.Resource.contains_substring ~haystack:t ~needle:"codex says hi"
         | Agent_process.Result { text; _ } ->
           Discord_agents.Resource.contains_substring ~haystack:text ~needle:"codex says hi"
         | _ -> false) events);
    let argv = read (Filename.concat base "codex.argv") in
    Alcotest.(check bool) "mcp command override" true
      (Discord_agents.Resource.contains_substring ~haystack:argv
         ~needle:{|mcp_servers.discord_agents.command="sh"|});
    (* Codex gives MCP servers an allowlisted environment, so the thread
       id must be in the server's env table, not only the agent's env. *)
    Alcotest.(check bool) "thread id in the MCP server env table" true
      (Discord_agents.Resource.contains_substring ~haystack:argv
         ~needle:{|mcp_servers.discord_agents.env.DISCORD_AGENTS_THREAD_ID="4242"|}))

(* ── agy over ssh: the executor's global MCP config ── *)

let agy_config base = Filename.concat base ".gemini/config/mcp_config.json"

let mkdir_p path = ignore (Sys.command (Filename.quote_command "mkdir" ["-p"; path]))

(* Run the value agy's entry would eval, with an ssh stand-in that prints
   its argv, to see what reaches the writer. *)
let eval_agy_value value =
  let dir = Filename.temp_dir "discord_agents_agy_value_" "" in
  Fun.protect ~finally:(fun () -> rm_rf dir) @@ fun () ->
  write_exe (Filename.concat dir "ssh")
    "#!/bin/sh\nfor a in \"$@\"; do printf '%s\\n' \"$a\"; done\n";
  let args = match Agent_process.agy_mcp_entry with
    | `Assoc f -> (match List.assoc "args" f with
        | `List l -> List.map (function `String x -> x | _ -> "") l | _ -> [])
    | _ -> [] in
  let env = [| "PATH=" ^ dir ^ ":/usr/bin:/bin"; "DISCORD_AGENTS_THREAD_ID=4242";
               Agent_process.agy_mcp_env_var ^ "=" ^ value |] in
  let ((out, _, _) as proc) =
    Unix.open_process_args_full "/bin/sh" (Array.of_list ("/bin/sh" :: args)) env in
  let output = In_channel.input_all out in
  ignore (Unix.close_process_full proc);
  output

let test_agy_turn_over_ssh () =
  with_env (fun ~base ~ssh ~mcp ~work ->
    mkdir_p (Filename.concat base ".gemini/config");
    let cfg = agy_config base in
    Out_channel.with_open_text cfg (fun oc ->
      output_string oc {|{"theme":"dark","mcpServers":{"mine":{"command":"x"}}}|});
    Unix.chmod cfg 0o644;
    let r, events = run_turn ~ssh ~work ~kind:Discord_agents.Config.Agy () in
    (match r with Ok () -> () | Error e -> Alcotest.failf "turn failed: %s" e);
    Alcotest.(check bool) "agy text arrived" true
      (List.exists (function Agent_process.Text_delta "agy says hi" -> true | _ -> false) events);
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_file cfg in
    Alcotest.(check string) "user key kept" "dark" (json |> member "theme" |> to_string);
    Alcotest.(check string) "user server kept" "x"
      (json |> member "mcpServers" |> member "mine" |> member "command" |> to_string);
    Alcotest.(check bool) "our entry installed on the executor" true
      (json |> member "mcpServers" |> member "discord_agents" = Agent_process.agy_mcp_entry);
    Alcotest.(check int) "mode kept" 0o644 ((Unix.stat cfg).Unix.st_perm);
    Alcotest.(check bool) "no temp left" true
      (Array.for_all (fun f -> not (String.starts_with ~prefix:".discord-agents." f))
         (Sys.readdir (Filename.dirname cfg)));
    Alcotest.(check bool) "agy ran in the worktree" true
      (Discord_agents.Resource.contains_substring
         ~haystack:(read (Filename.concat base "agy.argv")) ~needle:("--add-dir\n" ^ work));
    let value = read (Filename.concat base "agy.mcp") in
    Alcotest.(check bool) "agy got the MCP variable" true (value <> "unset");
    (* Evaluated as the entry does: ssh back to the writer with the
       thread id and this bot's server. *)
    let reached = eval_agy_value value in
    Alcotest.(check bool) "reaches the writer" true
      (Discord_agents.Resource.contains_substring ~haystack:reached ~needle:"tower0");
    Alcotest.(check bool) "carries the thread id" true
      (Discord_agents.Resource.contains_substring ~haystack:reached
         ~needle:"DISCORD_AGENTS_THREAD_ID=4242");
    Alcotest.(check bool) "runs this bot's server" true
      (Discord_agents.Resource.contains_substring ~haystack:reached
         ~needle:(Filename.basename mcp)))

let test_agy_remote_declines () =
  with_env (fun ~base ~ssh ~mcp:_ ~work ->
    (* No ~/.gemini/config on the executor: nothing created, no variable. *)
    let r, _ = run_turn ~ssh ~work ~kind:Discord_agents.Config.Agy () in
    (match r with Ok () -> () | Error e -> Alcotest.failf "turn failed: %s" e);
    Alcotest.(check bool) "config dir not created" false
      (Sys.file_exists (Filename.concat base ".gemini"));
    Alcotest.(check string) "no MCP variable" "unset" (read (Filename.concat base "agy.mcp"));
    (* A symlinked config is left alone. *)
    mkdir_p (Filename.concat base ".gemini/config");
    let target = Filename.concat base "managed.json" in
    Out_channel.with_open_text target (fun oc -> output_string oc "{}");
    Unix.symlink target (agy_config base);
    let r, _ = run_turn ~ssh ~work ~kind:Discord_agents.Config.Agy () in
    (match r with Ok () -> () | Error e -> Alcotest.failf "turn failed: %s" e);
    Alcotest.(check bool) "symlink kept" true
      ((Unix.lstat (agy_config base)).Unix.st_kind = Unix.S_LNK);
    Alcotest.(check string) "target untouched" "{}" (read target);
    Alcotest.(check string) "still no MCP variable" "unset" (read (Filename.concat base "agy.mcp")))

let test_agy_remote_new_file_and_old_entry () =
  with_env (fun ~base ~ssh ~mcp:_ ~work ->
    mkdir_p (Filename.concat base ".gemini/config");
    let cfg = agy_config base in
    let r, _ = run_turn ~ssh ~work ~kind:Discord_agents.Config.Agy () in
    (match r with Ok () -> () | Error e -> Alcotest.failf "turn failed: %s" e);
    Alcotest.(check int) "a new config is private" 0o600 ((Unix.stat cfg).Unix.st_perm);
    (* The entry #164 wrote is recognised and updated, extra keys kept. *)
    let old_args = match Agent_process.agy_mcp_previous_args_json with a :: _ -> a | [] -> `Null in
    Yojson.Safe.to_file cfg (`Assoc [("mcpServers", `Assoc [("discord_agents", `Assoc [
      ("command", `String "/bin/sh"); ("args", old_args); ("env", `Assoc [("K", `String "V")])])])]);
    let r, _ = run_turn ~ssh ~work ~kind:Discord_agents.Config.Agy () in
    (match r with Ok () -> () | Error e -> Alcotest.failf "turn failed: %s" e);
    let open Yojson.Safe.Util in
    let entry = Yojson.Safe.from_file cfg |> member "mcpServers" |> member "discord_agents" in
    Alcotest.(check bool) "updated to the current args" true
      (entry |> member "args" = (match Agent_process.agy_mcp_entry with
         | `Assoc f -> List.assoc "args" f | _ -> `Null));
    Alcotest.(check string) "extra key kept" "V" (entry |> member "env" |> member "K" |> to_string);
    Alcotest.(check bool) "MCP variable set" true (read (Filename.concat base "agy.mcp") <> "unset"))

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
      ~mcp:(Some ("sh", ["-c"; "exec ssh tower0"])) ();
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_file (Filename.concat work ".gemini/settings.json") in
    Alcotest.(check string) "user key kept" "dark" (json |> member "theme" |> to_string);
    Alcotest.(check string) "other server kept" "x"
      (json |> member "mcpServers" |> member "other" |> member "command" |> to_string);
    Alcotest.(check string) "our server" "sh"
      (json |> member "mcpServers" |> member "discord-agents" |> member "command"
       |> to_string);
    let exclude = read (Filename.concat work ".git/info/exclude") in
    Alcotest.(check bool) "excluded from git" true
      (Agent_process.exclude_already_lists_gemini exclude))

(* With no MCP server to register, withdraw a stale entry, as locally. *)
let test_remote_gemini_withdraws_entry_without_mcp () =
  with_env (fun ~base:_ ~ssh ~mcp:_ ~work ->
    Eio_main.run @@ fun _env ->
    Unix.mkdir (Filename.concat work ".gemini") 0o755;
    Out_channel.with_open_text (Filename.concat work ".gemini/settings.json")
      (fun oc -> output_string oc
          {|{"theme":"dark","mcpServers":{"discord-agents":{"command":"old"}}}|});
    let host = { (Host.local_host "framework0") with
                 Host.transport = Host.Ssh "remote" } in
    Agent_process.setup_gemini_mcp_remote ~ssh ~host ~working_dir:work ~mcp:None ();
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_file (Filename.concat work ".gemini/settings.json") in
    Alcotest.(check string) "user key kept" "dark" (json |> member "theme" |> to_string);
    Alcotest.(check bool) "our entry gone" true
      (json |> member "mcpServers" |> member "discord-agents" = `Null))

(* A requested time limit must never silently become no limit. *)
let test_timeout_refused_without_coreutils () =
  with_env (fun ~base:_ ~ssh ~mcp:_ ~work ->
    let saved = Sys.getenv_opt "PATH" in
    Unix.putenv "PATH" (Filename.dirname ssh);
    Fun.protect ~finally:(fun () -> Unix.putenv "PATH" (Option.value saved ~default:""))
    @@ fun () ->
    let host = { (Host.local_host "framework0") with Host.transport = Host.Ssh "remote" } in
    match Discord_agents.Host_exec.run_capture ~ssh ~timeout_s:5 ~cwd:work host ["true"] with
    | Error (Discord_agents.Host_exec.Setup_failed msg) ->
      Alcotest.(check bool) msg true
        (Discord_agents.Resource.contains_substring ~haystack:msg ~needle:"timeout")
    | Ok _ -> Alcotest.fail "ran without its time limit"
    | Error f -> Alcotest.failf "unexpected: %s" (Discord_agents.Host_exec.failure_detail f))

let () =
  Alcotest.run "remote_runs" [
    "pure", [
      Alcotest.test_case "remote mcp command" `Quick test_remote_mcp_command;
      Alcotest.test_case "codex overrides" `Quick test_codex_overrides_for_remote;
      Alcotest.test_case "failure message" `Quick test_remote_failure_message;
      Alcotest.test_case "attachment storage name" `Quick test_attachment_storage_name;
      Alcotest.test_case "codex overrides carry thread id" `Quick
        test_codex_overrides_carry_thread_id;
    ];
    "over ssh", [
      Alcotest.test_case "claude turn" `Quick test_claude_turn_over_ssh;
      Alcotest.test_case "unreachable" `Quick test_unreachable_turn;
      Alcotest.test_case "attachment" `Quick test_remote_attachment_saved_in_worktree;
      Alcotest.test_case "gemini settings" `Quick test_remote_gemini_settings;
      Alcotest.test_case "claude recovery" `Quick test_claude_recovery_over_ssh;
      Alcotest.test_case "codex turn" `Quick test_codex_turn_over_ssh;
      Alcotest.test_case "gemini withdraws entry without mcp" `Quick
        test_remote_gemini_withdraws_entry_without_mcp;
      Alcotest.test_case "timeout refused without coreutils" `Quick
        test_timeout_refused_without_coreutils;
      Alcotest.test_case "agy turn installs executor MCP entry" `Quick test_agy_turn_over_ssh;
      Alcotest.test_case "agy declines without config dir, on symlink" `Quick
        test_agy_remote_declines;
      Alcotest.test_case "agy new file private, old entry updated" `Quick
        test_agy_remote_new_file_and_old_entry;
    ];
  ]
