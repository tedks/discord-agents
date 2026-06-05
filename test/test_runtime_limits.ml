let limit soft hard =
  Discord_agents.Runtime_limits.{ soft; hard }

let test_nofile_target_raises_to_desired () =
  Alcotest.(check (option int64)) "target"
    (Some 65_536L)
    (Discord_agents.Runtime_limits.nofile_target
       ~desired:65_536L (limit 1_024L 1_048_576L))

let test_nofile_target_caps_at_hard_limit () =
  Alcotest.(check (option int64)) "target"
    (Some 4_096L)
    (Discord_agents.Runtime_limits.nofile_target
       ~desired:65_536L (limit 1_024L 4_096L))

let test_nofile_target_does_not_lower_existing_soft_limit () =
  Alcotest.(check (option int64)) "target"
    None
    (Discord_agents.Runtime_limits.nofile_target
       ~desired:65_536L (limit 131_072L 1_048_576L))

let test_nofile_target_noop_when_hard_not_above_soft () =
  Alcotest.(check (option int64)) "target"
    None
    (Discord_agents.Runtime_limits.nofile_target
       ~desired:65_536L (limit 1_024L 512L))

let () =
  Alcotest.run "runtime_limits" [
    ("nofile", [
      Alcotest.test_case "raises to desired" `Quick
        test_nofile_target_raises_to_desired;
      Alcotest.test_case "caps at hard limit" `Quick
        test_nofile_target_caps_at_hard_limit;
      Alcotest.test_case "does not lower existing soft limit" `Quick
        test_nofile_target_does_not_lower_existing_soft_limit;
      Alcotest.test_case "noop when hard not above soft" `Quick
        test_nofile_target_noop_when_hard_not_above_soft;
    ]);
  ]
