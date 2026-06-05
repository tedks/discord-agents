let contains_substring text needle =
  let text_len = String.length text in
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > text_len then false
    else if String.sub text i needle_len = needle then true
    else loop (i + 1)
  in
  needle_len = 0 || loop 0

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_rf (Filename.concat path name));
    Unix.rmdir path
  | _ ->
    Unix.unlink path

let with_tmp_dir f =
  let dir = Filename.temp_dir "discord_agents_disk_health_" "" in
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let test_mode_of_available_bytes () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  Alcotest.(check string) "healthy"
    "healthy"
    (string_of_mode (mode_of_available_bytes (mib 256)));
  Alcotest.(check string) "warning"
    "warning"
    (string_of_mode (mode_of_available_bytes (mib 96)));
  Alcotest.(check string) "read_only"
    "read_only"
    (string_of_mode (mode_of_available_bytes (mib 32)))

let test_preflight_available_bytes_updates_snapshot () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  reset ();
  match update_from_available_bytes ~path:"/tmp/state" (mib 48) with
  | Ok () -> Alcotest.fail "expected read-only preflight failure"
  | Error err ->
    Alcotest.(check bool) "mentions read-only" true
      (contains_substring err "read-only");
    let snap = snapshot () in
    Alcotest.(check string) "mode"
      "read_only" (string_of_mode snap.mode);
    Alcotest.(check (option string)) "path"
      (Some "/tmp/state") snap.checked_path

let test_healthy_probe_does_not_clear_unrelated_read_only_path () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  reset ();
  ignore (update_from_available_bytes ~path:"/project" (mib 32));
  ignore (update_from_available_bytes ~path:"/config" (mib 512));
  let snap = snapshot () in
  Alcotest.(check string) "mode remains read-only"
    "read_only" (string_of_mode snap.mode);
  Alcotest.(check (option string)) "path remains project"
    (Some "/project") snap.checked_path;
  ignore (update_from_available_bytes ~path:"/project" (mib 512));
  let snap = snapshot () in
  Alcotest.(check string) "path recovers independently"
    "healthy" (string_of_mode snap.mode)

let test_write_failure_survives_healthy_probe_until_write_success () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  with_tmp_dir (fun dir ->
    reset ();
    note_write_failure dir
      (Unix.Unix_error (Unix.EUNKNOWNERR 122, "write", dir));
    ignore (preflight_path_with
      ~available_bytes_of_path:(fun _ -> mib 512)
      dir);
    let snap = snapshot () in
    Alcotest.(check string) "healthy probe does not clear quota"
      "read_only" (string_of_mode snap.mode);
    let quota_reason =
      match snap.last_error with
      | Some err -> contains_substring err "EDQUOT"
      | None -> false
    in
    Alcotest.(check bool) "quota reason remains"
      true quota_reason;
    note_write_success dir;
    let snap = snapshot () in
    Alcotest.(check string) "successful write clears sticky failure"
      "healthy" (string_of_mode snap.mode))

let test_write_failure_survives_warning_probe_until_write_success () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  with_tmp_dir (fun dir ->
    reset ();
    note_write_failure dir
      (Unix.Unix_error (Unix.EUNKNOWNERR 122, "write", dir));
    ignore (preflight_path_with
      ~available_bytes_of_path:(fun _ -> mib 96)
      dir);
    let snap = snapshot () in
    Alcotest.(check string) "warning probe does not downgrade quota"
      "read_only" (string_of_mode snap.mode);
    let quota_reason =
      match snap.last_error with
      | Some err -> contains_substring err "EDQUOT"
      | None -> false
    in
    Alcotest.(check bool) "quota reason remains"
      true quota_reason)

let test_first_write_failure_clears_after_file_is_created () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  with_tmp_dir (fun dir ->
    let file_path = Filename.concat dir "new-file.json" in
    reset ();
    note_write_failure file_path
      (Unix.Unix_error (Unix.EUNKNOWNERR 122, "write", file_path));
    ignore (preflight_path_with
      ~available_bytes_of_path:(fun _ -> mib 512)
      file_path);
    let snap = snapshot () in
    Alcotest.(check string) "first-write quota sticks"
      "read_only" (string_of_mode snap.mode);
    let oc = open_out file_path in
    output_string oc "{}\n";
    close_out oc;
    note_write_success file_path;
    let snap = snapshot () in
    Alcotest.(check string) "creating file then writing clears sticky failure"
      "healthy" (string_of_mode snap.mode))

let test_deleted_pressure_path_is_evicted_on_refresh () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  with_tmp_dir (fun dir ->
    let child = Filename.concat dir "deleted-child" in
    Unix.mkdir child 0o755;
    reset ();
    ignore (update_from_available_bytes ~path:child (mib 32));
    Unix.rmdir child;
    ignore (preflight_path_with
      ~available_bytes_of_path:(fun path ->
        Alcotest.(check string) "probe falls back to parent" dir path;
        mib 512)
      child);
    let snap = snapshot () in
    Alcotest.(check string) "deleted pressure path evicted"
      "healthy" (string_of_mode snap.mode);
    Alcotest.(check (option string)) "parent path recorded"
      (Some dir) snap.checked_path)

let test_probe_error_clears_after_successful_probe () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  with_tmp_dir (fun dir ->
    reset ();
    ignore (preflight_path_with
      ~available_bytes_of_path:(fun _ -> failwith "probe exploded")
      dir);
    let snap = snapshot () in
    Alcotest.(check bool) "probe error recorded"
      true (Option.is_some snap.last_error);
    ignore (preflight_path_with
      ~available_bytes_of_path:(fun _ -> mib 512)
      dir);
    let snap = snapshot () in
    Alcotest.(check (option string)) "probe error cleared"
      None snap.last_error)

let test_classify_write_failure_variants () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  let code exn =
    match classify_write_failure exn with
    | Some failure -> failure.code
    | None -> Alcotest.fail "expected classified disk failure"
  in
  Alcotest.(check string) "enospc"
    "ENOSPC"
    (code (Unix.Unix_error (Unix.ENOSPC, "write", "/tmp/x")));
  Alcotest.(check string) "edquot"
    "EDQUOT"
    (code (Sys_error "Disk quota exceeded"));
  Alcotest.(check string) "edquot unix unknown errno"
    "EDQUOT"
    (code (Unix.Unix_error (Unix.EUNKNOWNERR 122, "write", "/tmp/x")));
  Alcotest.(check string) "erofs"
    "EROFS"
    (code (Unix.Unix_error (Unix.EROFS, "write", "/tmp/x")));
  Alcotest.(check string) "partial write"
    "PARTIAL_WRITE"
    (code (Failure "resource: short write"))

let test_note_write_failure_marks_read_only () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  reset ();
  note_write_failure "/tmp/state"
    (Unix.Unix_error (Unix.ENOSPC, "write", "/tmp/state"));
  let snap = snapshot () in
  Alcotest.(check string) "mode"
    "read_only" (string_of_mode snap.mode);
  Alcotest.(check bool) "reason recorded"
    true (Option.is_some snap.last_error)

let test_new_session_block_message_uses_preflight () =
  let open Discord_agents.Disk_health in
  let open For_testing in
  reset ();
  let blocked =
    new_session_block_message
      ~preflight:(fun () -> Error "disk probe says read-only")
      ()
  in
  Alcotest.(check (option string)) "preflight error surfaces"
    (Some "disk probe says read-only") blocked

let () =
  Alcotest.run "disk_health" [
    ("health", [
      Alcotest.test_case "mode of available bytes" `Quick
        test_mode_of_available_bytes;
      Alcotest.test_case "preflight updates snapshot" `Quick
        test_preflight_available_bytes_updates_snapshot;
      Alcotest.test_case "healthy probe preserves other read-only path" `Quick
        test_healthy_probe_does_not_clear_unrelated_read_only_path;
      Alcotest.test_case "quota write failure survives healthy probe" `Quick
        test_write_failure_survives_healthy_probe_until_write_success;
      Alcotest.test_case "quota write failure survives warning probe" `Quick
        test_write_failure_survives_warning_probe_until_write_success;
      Alcotest.test_case "first write failure clears after file creation" `Quick
        test_first_write_failure_clears_after_file_is_created;
      Alcotest.test_case "deleted pressure path is evicted" `Quick
        test_deleted_pressure_path_is_evicted_on_refresh;
      Alcotest.test_case "probe error clears after success" `Quick
        test_probe_error_clears_after_successful_probe;
      Alcotest.test_case "classify write failure variants" `Quick
        test_classify_write_failure_variants;
      Alcotest.test_case "write failure marks read only" `Quick
        test_note_write_failure_marks_read_only;
      Alcotest.test_case "new session block message re-probes" `Quick
        test_new_session_block_message_uses_preflight;
    ]);
  ]
