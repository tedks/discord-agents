let test_create_message_body_enforces_nonce () =
  let json =
    Discord_agents.Discord_rest.create_message_body
      ~content:"hello"
      ~reply_to:"123"
      ~nonce:"abcdef0123456789abcdef01"
      ()
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "content"
    "hello" (json |> member "content" |> to_string);
  Alcotest.(check string) "nonce"
    "abcdef0123456789abcdef01" (json |> member "nonce" |> to_string);
  Alcotest.(check bool) "enforce_nonce"
    true (json |> member "enforce_nonce" |> to_bool);
  Alcotest.(check string) "reply_to"
    "123"
    (json |> member "message_reference" |> member "message_id" |> to_string)

let test_transport_error_classification () =
  let classify msg =
    Discord_agents.Discord_rest.classify_transport_error_message msg
    |> Discord_agents.Discord_rest.string_of_transport_error_kind
  in
  Alcotest.(check string) "hostname resolution"
    "hostname_resolution"
    (classify "Failure(\"failed to resolve hostname\")");
  Alcotest.(check string) "timeout"
    "timeout"
    (classify "Unix.Unix_error(ETIMEDOUT, \"connect\", \"\")");
  Alcotest.(check string) "connection"
    "connection"
    (classify "Failure(\"connection reset by peer\")");
  Alcotest.(check string) "other"
    "other_transport"
    (classify "Failure(\"unexpected library wording\")")

let test_transport_backoff_caps () =
  let check expected failures =
    Alcotest.(check (float 1e-9))
      (Printf.sprintf "failures=%d" failures)
      expected
      (Discord_agents.Discord_rest.transport_backoff_seconds failures)
  in
  check 0.5 1;
  check 1.0 2;
  check 2.0 3;
  check 8.0 10

let test_decode_json_body_reports_parse_error () =
  match Discord_agents.Discord_rest.decode_json_body "{not json" with
  | Ok _ -> Alcotest.fail "expected parse error"
  | Error err ->
    Alcotest.(check bool) "non-empty error"
      true (String.length err > 0)

let test_next_backoff_until_updates_deadline () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let ts_s t = Int64.to_float (Mtime.to_uint64_ns t) /. 1e9 in
  Alcotest.(check (float 1e-9)) "active window extends when needed"
    13.0
    (Discord_agents.Discord_rest.next_backoff_until
      ~now:(ts 9.0) ~current_until:(Some (ts 10.0)) ~delay:4.0
     |> ts_s);
  Alcotest.(check (float 1e-9)) "active longer window preserved"
    10.0
    (Discord_agents.Discord_rest.next_backoff_until
      ~now:(ts 9.0) ~current_until:(Some (ts 10.0)) ~delay:0.5
     |> ts_s);
  Alcotest.(check (float 1e-9)) "expired window advances"
    15.0
    (Discord_agents.Discord_rest.next_backoff_until
      ~now:(ts 11.0) ~current_until:(Some (ts 10.0)) ~delay:4.0
     |> ts_s);
  Alcotest.(check (float 1e-9)) "missing window starts fresh"
    15.0
    (Discord_agents.Discord_rest.next_backoff_until
      ~now:(ts 11.0) ~current_until:None ~delay:4.0
     |> ts_s)

let test_retry_after_seconds_from_body () =
  Alcotest.(check (option (float 1e-9))) "float retry_after"
    (Some 2.5)
    (Discord_agents.Discord_rest.retry_after_seconds_from_body
      {|{"retry_after":2.5}|});
  Alcotest.(check (option (float 1e-9))) "int retry_after"
    (Some 3.0)
    (Discord_agents.Discord_rest.retry_after_seconds_from_body
      {|{"retry_after":3}|});
  Alcotest.(check (option (float 1e-9))) "missing retry_after"
    None
    (Discord_agents.Discord_rest.retry_after_seconds_from_body
      {|{"message":"nope"}|});
  Alcotest.(check (option (float 1e-9))) "retry_after preserves zero"
    (Some 0.0)
    (Discord_agents.Discord_rest.retry_after_seconds_from_body
      {|{"retry_after":0}|});
  Alcotest.(check (option (float 1e-9))) "retry_after preserves large values"
    (Some 9999.0)
    (Discord_agents.Discord_rest.retry_after_seconds_from_body
      {|{"retry_after":9999}|})

let test_retry_after_seconds_prefers_headers () =
  let headers = Http.Header.of_list [("retry-after", "4.5")] in
  Alcotest.(check (option (float 1e-9))) "header preferred"
    (Some 4.5)
    (Discord_agents.Discord_rest.retry_after_seconds
      ~headers {|{"retry_after":2.5}|});
  Alcotest.(check (option (float 1e-9))) "header fallback to body"
    (Some 2.5)
    (Discord_agents.Discord_rest.retry_after_seconds
      ~headers:(Http.Header.of_list [("retry-after", "not-a-number")])
      {|{"retry_after":2.5}|})

let test_health_state_failure_and_recovery () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Discord_agents.Discord_rest.create_health_state () in
  let delay1 =
    Discord_agents.Discord_rest.note_failure_state state
      ~now:(ts 10.0) ~summary:"first" ~delay:0.5
  in
  Alcotest.(check (float 1e-9)) "first delay" 0.5 delay1;
  Alcotest.(check bool) "degraded after failure"
    true (Discord_agents.Discord_rest.health_degraded state);
  Alcotest.(check (option string)) "error recorded"
    (Some "first") (Discord_agents.Discord_rest.health_last_error state);
  Alcotest.(check int) "failures incremented"
    1 (Discord_agents.Discord_rest.health_consecutive_failures state);
  Alcotest.(check (float 1e-9)) "backoff set"
    0.5
    (Discord_agents.Discord_rest.health_retry_delay_s ~now:(ts 10.0) state);
  let cleared =
    Discord_agents.Discord_rest.note_recovery_state state
  in
  Alcotest.(check int) "recovery reports prior failures"
    1 cleared;
  Alcotest.(check bool) "recovery clears degraded"
    false (Discord_agents.Discord_rest.health_degraded state);
  Alcotest.(check (option string)) "recovery clears error"
    None (Discord_agents.Discord_rest.health_last_error state)

let test_health_state_returns_effective_shared_delay () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Discord_agents.Discord_rest.create_health_state () in
  let _ =
    Discord_agents.Discord_rest.note_failure_state state
      ~now:(ts 10.0) ~summary:"first" ~delay:5.0
  in
  let delay =
    Discord_agents.Discord_rest.note_failure_state state
      ~now:(ts 11.0) ~summary:"second" ~delay:1.0
  in
  Alcotest.(check (float 1e-9)) "effective delay respects longer active window"
    4.0 delay

let test_rate_limit_state_preserves_backoff_window () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Discord_agents.Discord_rest.create_health_state () in
  let _ =
    Discord_agents.Discord_rest.note_rate_limit_state state
      ~now:(ts 10.0) ~summary:"429" ~delay:3.0
  in
  Alcotest.(check int) "rate limit marks degraded"
    1 (Discord_agents.Discord_rest.health_consecutive_failures state);
  let _ =
    Discord_agents.Discord_rest.note_rate_limit_state state
      ~now:(ts 11.0) ~summary:"429-again" ~delay:1.0
  in
  Alcotest.(check int) "repeated rate limits increment failures"
    2 (Discord_agents.Discord_rest.health_consecutive_failures state);
  Alcotest.(check (float 1e-9)) "active window preserved"
    2.0
    (Discord_agents.Discord_rest.health_retry_delay_s ~now:(ts 11.0) state);
  let _ =
    Discord_agents.Discord_rest.note_rate_limit_state state
      ~now:(ts 11.0) ~summary:"429-extend" ~delay:5.0
  in
  Alcotest.(check (float 1e-9)) "longer retry_after extends window"
    5.0
    (Discord_agents.Discord_rest.health_retry_delay_s ~now:(ts 11.0) state);
  let _ =
    Discord_agents.Discord_rest.note_rate_limit_state state
      ~now:(ts 15.0) ~summary:"429-later" ~delay:2.0
  in
  Alcotest.(check (float 1e-9)) "later candidate extends active window"
    2.0
    (Discord_agents.Discord_rest.health_retry_delay_s ~now:(ts 15.0) state)

let test_response_clears_rest_health () =
  Alcotest.(check bool) "200 clears"
    true (Discord_agents.Discord_rest.response_clears_rest_health 200);
  Alcotest.(check bool) "204 clears"
    true (Discord_agents.Discord_rest.response_clears_rest_health 204);
  Alcotest.(check bool) "400 does not clear"
    false (Discord_agents.Discord_rest.response_clears_rest_health 400);
  Alcotest.(check bool) "404 does not clear"
    false (Discord_agents.Discord_rest.response_clears_rest_health 404);
  Alcotest.(check bool) "500 does not clear"
    false (Discord_agents.Discord_rest.response_clears_rest_health 500)

let () =
  Alcotest.run "discord_rest" [
    ("discord_rest", [
      Alcotest.test_case "create_message_body enforces nonce" `Quick
        test_create_message_body_enforces_nonce;
      Alcotest.test_case "transport error classification" `Quick
        test_transport_error_classification;
      Alcotest.test_case "transport backoff caps" `Quick
        test_transport_backoff_caps;
      Alcotest.test_case "decode_json_body reports parse error" `Quick
        test_decode_json_body_reports_parse_error;
      Alcotest.test_case "next_backoff_until updates deadlines" `Quick
        test_next_backoff_until_updates_deadline;
      Alcotest.test_case "retry_after_seconds_from_body parses int/float" `Quick
        test_retry_after_seconds_from_body;
      Alcotest.test_case "retry_after_seconds prefers headers" `Quick
        test_retry_after_seconds_prefers_headers;
      Alcotest.test_case "health state failure and recovery" `Quick
        test_health_state_failure_and_recovery;
      Alcotest.test_case "health state returns effective shared delay" `Quick
        test_health_state_returns_effective_shared_delay;
      Alcotest.test_case "rate limit state preserves backoff window" `Quick
        test_rate_limit_state_preserves_backoff_window;
      Alcotest.test_case "response_clears_rest_health only for 2xx" `Quick
        test_response_clears_rest_health;
    ]);
  ]
