let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_rf (Filename.concat path name));
    Unix.rmdir path
  | _ ->
    Unix.unlink path

let make_tmp_dir prefix =
  let base = Filename.temp_file prefix "" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  base

let restore_env name = function
  | Some value -> Unix.putenv name value
  | None -> Unix.putenv name ""

let with_tmp_home f =
  let base = make_tmp_dir "discord_agents_sessions_" in
  let old_home = Sys.getenv_opt "HOME" in
  let old_xdg_config_home = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Fun.protect
    ~finally:(fun () ->
      restore_env "HOME" old_home;
      restore_env "XDG_CONFIG_HOME" old_xdg_config_home;
      rm_rf base)
    (fun () ->
      Unix.putenv "HOME" base;
      Unix.putenv "XDG_CONFIG_HOME" "";
      f base)

let sessions_path home =
  Filename.concat home ".config/discord-agents/sessions.json"

let backup_path home = sessions_path home ^ ".bak"

let make_session () =
  Discord_agents.Session_store.make_session
    ~project_name:"control"
    ~working_dir:"/tmp/project"
    ~agent_kind:Discord_agents.Config.Claude
    ~session_id:"session-1"
    ~thread_id:"control"
    ~system_prompt:(Some "prompt")
    ~initial_prompt:None
    ()

let find_control_session store =
  match Discord_agents.Session_store.find_opt store ~thread_id:"control" with
  | Some session -> session
  | None -> Alcotest.fail "expected persisted control session"

let test_save_updates_backup () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    let primary = sessions_path home in
    let backup = backup_path home in
    Alcotest.(check bool) "primary exists" true (Sys.file_exists primary);
    Alcotest.(check bool) "backup exists" true (Sys.file_exists backup);
    Alcotest.(check string) "backup mirrors primary"
      (Discord_agents.Resource.read_file primary)
      (Discord_agents.Resource.read_file backup))

let test_load_uses_backup_when_primary_corrupt () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    (match Discord_agents.Session_store.set_stop_requested store session true with
     | Ok () -> ()
     | Error err -> Alcotest.failf "set_stop_requested failed: %s" err);
    let primary = sessions_path home in
    let oc = open_out primary in
    output_string oc "{ definitely not json";
    close_out oc;
    let reloaded = Discord_agents.Session_store.create () in
    let recovered = find_control_session reloaded in
    Alcotest.(check bool) "stop_requested recovered from backup"
      true recovered.Discord_agents.Session_store.stop_requested)

let test_load_uses_backup_when_primary_missing () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    Sys.remove (sessions_path home);
    let reloaded = Discord_agents.Session_store.create () in
    let recovered = find_control_session reloaded in
    Alcotest.(check string) "project recovered from backup"
      "control" recovered.Discord_agents.Session_store.project_name)

let () =
  Alcotest.run "session_store" [
    ("persistence", [
      Alcotest.test_case "save updates backup" `Quick
        test_save_updates_backup;
      Alcotest.test_case "load uses backup when primary corrupt" `Quick
        test_load_uses_backup_when_primary_corrupt;
      Alcotest.test_case "load uses backup when primary missing" `Quick
        test_load_uses_backup_when_primary_missing;
    ]);
  ]
