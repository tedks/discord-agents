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
  Filename.temp_dir prefix ""

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

let test_load_uses_backup_when_primary_has_wrong_shape () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    (match Discord_agents.Session_store.set_stop_requested store session true with
     | Ok () -> ()
     | Error err -> Alcotest.failf "set_stop_requested failed: %s" err);
    let primary = sessions_path home in
    let oc = open_out primary in
    output_string oc "{}";
    close_out oc;
    let reloaded = Discord_agents.Session_store.create () in
    let recovered = find_control_session reloaded in
    Alcotest.(check bool) "structural error recovered from backup"
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

let test_save_with_visible_but_unconfirmed_primary_updates_backup () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    session.stop_requested <- true;
    let write_file path content =
      if String.equal path (sessions_path home) then
        Discord_agents.Resource.write_file_atomic
          ~fsync_parent:(fun dir ->
            raise (Unix.Unix_error (Unix.EINVAL, "fsync", dir)))
          path content
      else
        Discord_agents.Resource.write_file_atomic path content
    in
    Discord_agents.Session_store.save_with ~write_file store;
    Alcotest.(check string) "backup mirrors primary after warning"
      (Discord_agents.Resource.read_file (sessions_path home))
      (Discord_agents.Resource.read_file (backup_path home));
    Sys.remove (sessions_path home);
    let reloaded = Discord_agents.Session_store.create () in
    let recovered = find_control_session reloaded in
    Alcotest.(check bool) "backup captured updated stop_requested"
      true recovered.Discord_agents.Session_store.stop_requested)

let test_save_refuses_read_only_preflight () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    store.Discord_agents.Session_store.sessions <-
      Discord_agents.Session_store.SessionMap.add
        "control" session store.sessions;
    let write_attempted = ref false in
    let write_file _path _content =
      write_attempted := true;
      failwith "write should not be attempted"
    in
    (match
       try
         Discord_agents.Session_store.save_with
           ~preflight_write:(fun _ -> Error "disk is read-only")
           ~write_file store;
         None
       with Failure msg -> Some msg
     with
     | Some err ->
       Alcotest.(check string) "preflight error" "disk is read-only" err
     | None ->
       Alcotest.fail "save_with unexpectedly succeeded");
    Alcotest.(check bool) "write skipped" false !write_attempted;
    Alcotest.(check bool) "primary absent"
      false (Sys.file_exists (sessions_path home)))

let () =
  Alcotest.run "session_store" [
    ("persistence", [
      Alcotest.test_case "save updates backup" `Quick
        test_save_updates_backup;
      Alcotest.test_case "load uses backup when primary corrupt" `Quick
        test_load_uses_backup_when_primary_corrupt;
      Alcotest.test_case "load uses backup when primary has wrong shape" `Quick
        test_load_uses_backup_when_primary_has_wrong_shape;
      Alcotest.test_case "load uses backup when primary missing" `Quick
        test_load_uses_backup_when_primary_missing;
      Alcotest.test_case "visible but unconfirmed primary still updates backup" `Quick
        test_save_with_visible_but_unconfirmed_primary_updates_backup;
      Alcotest.test_case "save refuses read-only preflight" `Quick
        test_save_refuses_read_only_preflight;
    ]);
  ]
