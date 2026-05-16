let test_mode_of_available_bytes () =
  let open Discord_agents.Disk_health in
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
  reset_for_tests ();
  match update_from_available_bytes ~path:"/tmp/state" (mib 48) with
  | Ok () -> Alcotest.fail "expected read-only preflight failure"
  | Error err ->
    Alcotest.(check bool) "mentions read-only" true
      (String.contains err 'r');
    let snap = snapshot () in
    Alcotest.(check string) "mode"
      "read_only" (string_of_mode snap.mode);
    Alcotest.(check (option string)) "path"
      (Some "/tmp/state") snap.checked_path

let test_classify_write_failure_variants () =
  let open Discord_agents.Disk_health in
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
  Alcotest.(check string) "erofs"
    "EROFS"
    (code (Unix.Unix_error (Unix.EROFS, "write", "/tmp/x")));
  Alcotest.(check string) "partial write"
    "PARTIAL_WRITE"
    (code (Failure "resource: short write"))

let test_note_write_failure_marks_read_only () =
  let open Discord_agents.Disk_health in
  reset_for_tests ();
  note_write_failure "/tmp/state"
    (Unix.Unix_error (Unix.ENOSPC, "write", "/tmp/state"));
  let snap = snapshot () in
  Alcotest.(check string) "mode"
    "read_only" (string_of_mode snap.mode);
  Alcotest.(check bool) "reason recorded"
    true (Option.is_some snap.last_error)

let test_new_session_block_message_uses_preflight () =
  let open Discord_agents.Disk_health in
  reset_for_tests ();
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
      Alcotest.test_case "classify write failure variants" `Quick
        test_classify_write_failure_variants;
      Alcotest.test_case "write failure marks read only" `Quick
        test_note_write_failure_marks_read_only;
      Alcotest.test_case "new session block message re-probes" `Quick
        test_new_session_block_message_uses_preflight;
    ]);
  ]
