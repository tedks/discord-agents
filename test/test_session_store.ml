module Agent_checkpoint = Discord_agents.Agent_checkpoint

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

let write_primary_sessions home content =
  let path = sessions_path home in
  Discord_agents.Resource.ensure_parent_dir path;
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

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

let expect_active_run session ~message_id ~pid ~start_ticks =
  match session.Discord_agents.Session_store.active_run with
  | Some active_run ->
    Alcotest.(check string) "active message id" message_id
      (Agent_checkpoint.message_id_any active_run);
    (match Agent_checkpoint.child_process_any active_run with
     | Some child ->
       Alcotest.(check int) "child pid" pid child.pid;
       Alcotest.(check int64) "child start ticks" start_ticks child.start_ticks
     | None -> Alcotest.fail "expected persisted child process identity")
  | None ->
    Alcotest.fail "expected persisted active run"

let expect_active_run_without_child session ~message_id =
  match session.Discord_agents.Session_store.active_run with
  | Some active_run ->
    Alcotest.(check string) "active message id" message_id
      (Agent_checkpoint.message_id_any active_run);
    Alcotest.(check bool) "no child identity"
      true
      (Option.is_none
         (Agent_checkpoint.child_process_any active_run))
  | None ->
    Alcotest.fail "expected persisted active run"

let test_agent_checkpoint_tracks_child_by_construction () =
  let open Agent_checkpoint in
  let checkpoint = create ~message_id:"message-1" in
  Alcotest.(check string) "untracked message id"
    "message-1" (message_id checkpoint);
  Alcotest.(check bool) "untracked has no child"
    true (Option.is_none (child_process_any (erase checkpoint)));
  let child = child_process_identity ~pid:1234 ~start_ticks:5678L in
  let tracked = track_child checkpoint child in
  let tracked_child = child_process tracked in
  Alcotest.(check int) "tracked child pid" 1234 tracked_child.pid;
  Alcotest.(check int64) "tracked child start ticks"
    5678L tracked_child.start_ticks;
  (match child_process_any (erase tracked) with
   | Some erased_child ->
     Alcotest.(check int) "erased child pid" 1234 erased_child.pid;
     Alcotest.(check int64) "erased child start ticks"
       5678L erased_child.start_ticks
   | None ->
     Alcotest.fail "expected erased tracked checkpoint to expose child")

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
    let backup_stat = Unix.stat (backup_path home) in
    Unix.utimes
      primary
      backup_stat.Unix.st_atime
      backup_stat.Unix.st_mtime;
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

let test_save_marks_primary_newer_when_backup_update_fails () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    session.stop_requested <- true;
    let write_file path content =
      if String.equal path (backup_path home) then
        failwith "backup write failed"
      else
        Discord_agents.Resource.write_file_atomic path content
    in
    Discord_agents.Session_store.save_with ~write_file store;
    let primary_stat = Unix.stat (sessions_path home) in
    let backup_stat = Unix.stat (backup_path home) in
    Alcotest.(check bool) "primary stamped newer than stale backup"
      true (primary_stat.Unix.st_mtime > backup_stat.Unix.st_mtime))

let test_persisted_active_run_without_child_loads () =
  with_tmp_home (fun home ->
    (* This is the real crash window after the active-run checkpoint
       is saved and before the child process identity is captured. *)
    write_primary_sessions home {|
[
  {
    "project_name": "control",
    "working_dir": "/tmp/project",
    "agent_kind": "claude",
    "session_id": "session-1",
    "thread_id": "control",
    "message_count": 0,
    "active_message_id": "message-legacy"
  }
]
|};
    let store = Discord_agents.Session_store.create () in
    let recovered = find_control_session store in
    expect_active_run_without_child recovered ~message_id:"message-legacy")

let test_persisted_active_run_with_child_loads () =
  with_tmp_home (fun home ->
    write_primary_sessions home {|
[
  {
    "project_name": "control",
    "working_dir": "/tmp/project",
    "agent_kind": "claude",
    "session_id": "session-1",
    "thread_id": "control",
    "message_count": 0,
    "active_message_id": "message-legacy",
    "active_child_pid": 4242,
    "active_child_start_ticks": "123456789"
  }
]
|};
    let store = Discord_agents.Session_store.create () in
    let recovered = find_control_session store in
    expect_active_run recovered
      ~message_id:"message-legacy"
      ~pid:4242
      ~start_ticks:123456789L)

let test_load_ignores_stale_backup_when_primary_is_newer_and_corrupt () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    session.stop_requested <- true;
    let write_file path content =
      if String.equal path (backup_path home) then
        failwith "backup write failed"
      else
        Discord_agents.Resource.write_file_atomic path content
    in
    Discord_agents.Session_store.save_with ~write_file store;
    let oc = open_out (sessions_path home) in
    output_string oc "{ definitely not json";
    close_out oc;
    let backup_stat = Unix.stat (backup_path home) in
    Unix.utimes
      (sessions_path home)
      backup_stat.Unix.st_atime
      (backup_stat.Unix.st_mtime +. 10.0);
    let reloaded = Discord_agents.Session_store.create () in
    Alcotest.(check bool) "stale backup ignored"
      true
      (Option.is_none
         (Discord_agents.Session_store.find_opt reloaded ~thread_id:"control")))

let test_active_run_roundtrips_through_backup () =
  with_tmp_home (fun home ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session () in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    let child =
      Agent_checkpoint.child_process_identity
        ~pid:4242 ~start_ticks:123456789L
    in
    let active_run =
      let checkpoint =
        Agent_checkpoint.create ~message_id:"message-42"
      in
      Some (Agent_checkpoint.erase
        (Agent_checkpoint.track_child checkpoint child))
    in
    (match Discord_agents.Session_store.set_active_run store session active_run with
     | Ok () -> ()
     | Error err -> Alcotest.failf "set_active_run failed: %s" err);
    Sys.remove (sessions_path home);
    let reloaded = Discord_agents.Session_store.create () in
    let recovered = find_control_session reloaded in
    expect_active_run recovered
      ~message_id:"message-42"
      ~pid:4242
      ~start_ticks:123456789L)

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
      Alcotest.test_case "agent checkpoint tracks child by construction" `Quick
        test_agent_checkpoint_tracks_child_by_construction;
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
      Alcotest.test_case "failed backup leaves primary newer" `Quick
        test_save_marks_primary_newer_when_backup_update_fails;
      Alcotest.test_case "persisted active run without child loads" `Quick
        test_persisted_active_run_without_child_loads;
      Alcotest.test_case "persisted active run with child loads" `Quick
        test_persisted_active_run_with_child_loads;
      Alcotest.test_case "load ignores stale backup when primary is newer and corrupt" `Quick
        test_load_ignores_stale_backup_when_primary_is_newer_and_corrupt;
      Alcotest.test_case "active run roundtrips through backup" `Quick
        test_active_run_roundtrips_through_backup;
      Alcotest.test_case "save refuses read-only preflight" `Quick
        test_save_refuses_read_only_preflight;
    ]);
  ]
