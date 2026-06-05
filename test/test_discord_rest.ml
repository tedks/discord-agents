module Rest = Discord_agents.Discord_rest

let rng_initialized = ref false

let ensure_rng () =
  if not !rng_initialized then begin
    Mirage_crypto_rng_unix.use_default ();
    rng_initialized := true
  end

let response ?(headers = Http.Header.of_list []) code body =
  (Http.Response.make ~status:(Http.Status.of_int code) ~headers (),
   Cohttp_eio.Body.of_string body)

let with_fake_rest call f =
  ensure_rng ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let t : Rest.t = {
    token = "test-token";
    call;
    sw;
    clock = Eio.Stdenv.mono_clock env;
    rest_state = Rest.create_health_state ();
  } in
  f t

let body_json body =
  match body with
  | None -> None
  | Some body -> Some (Rest.read_body body)

let test_create_message_body_enforces_nonce () =
  let json =
    Rest.create_message_body
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
    Rest.classify_transport_error_message msg
    |> Rest.string_of_transport_error_kind
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
      (Rest.transport_backoff_seconds failures)
  in
  check 0.5 1;
  check 1.0 2;
  check 2.0 3;
  check 8.0 10

let test_decode_json_body_reports_parse_error () =
  match Rest.decode_json_body "{not json" with
  | Ok _ -> Alcotest.fail "expected parse error"
  | Error err ->
    Alcotest.(check bool) "non-empty error"
      true (String.length err > 0)

let test_next_backoff_until_updates_deadline () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let ts_s t = Int64.to_float (Mtime.to_uint64_ns t) /. 1e9 in
  Alcotest.(check (float 1e-9)) "active window extends when needed"
    13.0
    (Rest.next_backoff_until
      ~now:(ts 9.0) ~current_until:(Some (ts 10.0)) ~delay:4.0
     |> ts_s);
  Alcotest.(check (float 1e-9)) "active longer window preserved"
    10.0
    (Rest.next_backoff_until
      ~now:(ts 9.0) ~current_until:(Some (ts 10.0)) ~delay:0.5
     |> ts_s);
  Alcotest.(check (float 1e-9)) "expired window advances"
    15.0
    (Rest.next_backoff_until
      ~now:(ts 11.0) ~current_until:(Some (ts 10.0)) ~delay:4.0
     |> ts_s);
  Alcotest.(check (float 1e-9)) "missing window starts fresh"
    15.0
    (Rest.next_backoff_until
      ~now:(ts 11.0) ~current_until:None ~delay:4.0
     |> ts_s)

let test_retry_after_seconds_from_body () =
  Alcotest.(check (option (float 1e-9))) "float retry_after"
    (Some 2.5)
    (Rest.retry_after_seconds_from_body
      {|{"retry_after":2.5}|});
  Alcotest.(check (option (float 1e-9))) "int retry_after"
    (Some 3.0)
    (Rest.retry_after_seconds_from_body
      {|{"retry_after":3}|});
  Alcotest.(check (option (float 1e-9))) "missing retry_after"
    None
    (Rest.retry_after_seconds_from_body
      {|{"message":"nope"}|});
  Alcotest.(check (option (float 1e-9))) "retry_after preserves zero"
    (Some 0.0)
    (Rest.retry_after_seconds_from_body
      {|{"retry_after":0}|});
  Alcotest.(check (option (float 1e-9))) "retry_after caps large values"
    (Some Rest.max_retry_after_s)
    (Rest.retry_after_seconds_from_body
      {|{"retry_after":9999}|})

let test_retry_after_seconds_prefers_headers () =
  let headers = Http.Header.of_list [("retry-after", "4.5")] in
  Alcotest.(check (option (float 1e-9))) "header preferred"
    (Some 4.5)
    (Rest.retry_after_seconds
      ~headers {|{"retry_after":2.5}|});
  Alcotest.(check (option (float 1e-9))) "header fallback to body"
    (Some 2.5)
    (Rest.retry_after_seconds
      ~headers:(Http.Header.of_list [("retry-after", "not-a-number")])
      {|{"retry_after":2.5}|})

let test_retry_after_rejects_non_finite_values () =
  let check_none label raw =
    Alcotest.(check (option (float 1e-9))) label
      None (Rest.parse_retry_after_seconds raw)
  in
  check_none "nan rejected" "nan";
  check_none "infinity rejected" "inf";
  Alcotest.(check (option (float 1e-9))) "huge exponent rejected"
    None (Rest.parse_retry_after_seconds "1e9999")

let test_body_for_health_summary_preserves_utf8_boundary () =
  let body = String.make 499 'x' ^ "\xC3\xA9tail" in
  Alcotest.(check string) "drops incomplete final codepoint"
    (String.make 499 'x' ^ "... (truncated)")
    (Rest.body_for_health_summary body)

let test_health_state_failure_and_recovery () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let delay1 =
    Rest.note_failure_state state
      ~now:(ts 10.0) ~summary:"first" ~delay:0.5
  in
  Alcotest.(check (float 1e-9)) "first delay" 0.5 delay1;
  Alcotest.(check bool) "degraded after failure"
    true (Rest.health_degraded state);
  Alcotest.(check (option string)) "error recorded"
    (Some "first") (Rest.health_last_error state);
  Alcotest.(check int) "failures incremented"
    1 (Rest.health_consecutive_failures state);
  Alcotest.(check (float 1e-9)) "backoff set"
    0.5
    (Rest.health_retry_delay_s ~now:(ts 10.0) state);
  let cleared =
    Rest.note_recovery_state state ~now:(ts 11.0)
  in
  Alcotest.(check int) "recovery reports prior failures"
    1 cleared;
  Alcotest.(check bool) "recovery clears degraded"
    false (Rest.health_degraded state);
  Alcotest.(check (option string)) "recovery clears error"
    None (Rest.health_last_error state)

let test_health_state_returns_effective_shared_delay () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let _ =
    Rest.note_failure_state state
      ~now:(ts 10.0) ~summary:"first" ~delay:5.0
  in
  let delay =
    Rest.note_failure_state state
      ~now:(ts 11.0) ~summary:"second" ~delay:1.0
  in
  Alcotest.(check (float 1e-9)) "effective delay respects longer active window"
    4.0 delay

let test_rate_limit_state_preserves_backoff_window () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let _ =
    Rest.note_rate_limit_state state
      ~now:(ts 10.0) ~summary:"429" ~delay:3.0
  in
  Alcotest.(check int) "rate limit marks degraded"
    1 (Rest.health_consecutive_failures state);
  let _ =
    Rest.note_rate_limit_state state
      ~now:(ts 11.0) ~summary:"429-again" ~delay:1.0
  in
  Alcotest.(check int) "repeated rate limits increment failures"
    2 (Rest.health_consecutive_failures state);
  Alcotest.(check (float 1e-9)) "active window preserved"
    2.0
    (Rest.health_retry_delay_s ~now:(ts 11.0) state);
  let _ =
    Rest.note_rate_limit_state state
      ~now:(ts 11.0) ~summary:"429-extend" ~delay:5.0
  in
  Alcotest.(check (float 1e-9)) "longer retry_after extends window"
    5.0
    (Rest.health_retry_delay_s ~now:(ts 11.0) state);
  let _ =
    Rest.note_rate_limit_state state
      ~now:(ts 15.0) ~summary:"429-later" ~delay:2.0
  in
  Alcotest.(check (float 1e-9)) "later candidate extends active window"
    2.0
    (Rest.health_retry_delay_s ~now:(ts 15.0) state)

let test_rest_backoff_survives_unrelated_recovery () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let _ =
    Rest.note_rate_limit_state state
      ~now:(ts 10.0) ~summary:"429" ~delay:5.0
  in
  let failures = Rest.note_recovery_state state ~now:(ts 11.0) in
  Alcotest.(check int) "reports active rest failures" 1 failures;
  Alcotest.(check bool) "active rest backoff keeps degraded"
    true (Rest.health_degraded state);
  Alcotest.(check (option string)) "active rest error preserved"
    (Some "429") (Rest.health_last_error state);
  Alcotest.(check (float 1e-9)) "active deadline preserved"
    4.0 (Rest.health_retry_delay_s ~now:(ts 11.0) state);
  let _ = Rest.note_recovery_state state ~now:(ts 16.0) in
  Alcotest.(check bool) "expired rest backoff can recover"
    false (Rest.health_degraded state);
  Alcotest.(check (option string)) "expired rest error cleared"
    None (Rest.health_last_error state);
  Alcotest.(check (float 1e-9)) "expired deadline cleared"
    0.0 (Rest.health_retry_delay_s ~now:(ts 16.0) state)

let test_rest_backoff_survives_transport_shadowing () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let _ =
    Rest.note_rate_limit_state state
      ~now:(ts 10.0) ~summary:"429" ~delay:5.0
  in
  let delay =
    Rest.note_transport_failure_state state
      ~now:(ts 11.0) ~summary:"timeout" ~delay:8.0
  in
  Alcotest.(check (float 1e-9)) "transport extends combined deadline"
    8.0 delay;
  let transport_failures = Rest.note_transport_response_state state in
  Alcotest.(check int) "transport response reports failure"
    1 transport_failures;
  Alcotest.(check bool) "transport cleared"
    false (Rest.health_transport_degraded state);
  Alcotest.(check bool) "rest backoff still degraded"
    true (Rest.health_degraded state);
  Alcotest.(check (option string)) "rest error preserved"
    (Some "429") (Rest.health_last_error state);
  Alcotest.(check (float 1e-9)) "transport clear exposes rest deadline"
    4.0 (Rest.health_retry_delay_s ~now:(ts 11.0) state)

let test_rest_backoff_preserves_error_on_nonretryable_transport () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let _ =
    Rest.note_rate_limit_state state
      ~now:(ts 10.0) ~summary:"429" ~delay:5.0
  in
  let delay =
    Rest.note_transport_failure_without_backoff_state state
      ~now:(ts 11.0) ~summary:"unexpected"
  in
  Alcotest.(check (float 1e-9)) "rest deadline remains active"
    4.0 delay;
  Alcotest.(check (option string)) "rest error preserved"
    (Some "429") (Rest.health_last_error state);
  Alcotest.(check (option string)) "transport error recorded"
    (Some "unexpected") (Rest.health_last_transport_error state)

let test_transport_backoff_survives_rest_shadowing () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let _ =
    Rest.note_transport_failure_state state
      ~now:(ts 10.0) ~summary:"timeout" ~delay:8.0
  in
  let delay =
    Rest.note_failure_state state
      ~now:(ts 11.0) ~summary:"500" ~delay:4.0
  in
  Alcotest.(check (float 1e-9)) "transport remains combined deadline"
    7.0 delay;
  let transport_failures = Rest.note_transport_response_state state in
  Alcotest.(check int) "transport response reports failure"
    1 transport_failures;
  Alcotest.(check bool) "transport cleared"
    false (Rest.health_transport_degraded state);
  Alcotest.(check bool) "rest backoff remains degraded"
    true (Rest.health_degraded state);
  Alcotest.(check (option string)) "rest error preserved"
    (Some "500") (Rest.health_last_error state);
  Alcotest.(check (float 1e-9)) "rest deadline remains after transport clear"
    4.0 (Rest.health_retry_delay_s ~now:(ts 11.0) state)

let test_transport_health_clears_separately () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let _ =
    Rest.note_transport_failure_state state
      ~now:(ts 10.0) ~summary:"transport" ~delay:0.5
  in
  Alcotest.(check bool) "rest degraded"
    true (Rest.health_degraded state);
  Alcotest.(check bool) "transport degraded"
    true (Rest.health_transport_degraded state);
  Alcotest.(check int) "transport failures"
    1 (Rest.health_consecutive_transport_failures state);
  let cleared = Rest.note_transport_response_state state in
  Alcotest.(check int) "transport response reports failures" 1 cleared;
  Alcotest.(check bool) "rest remains degraded"
    true (Rest.health_degraded state);
  Alcotest.(check (float 1e-9)) "transport backoff cleared"
    0.0 (Rest.health_retry_delay_s ~now:(ts 10.0) state);
  Alcotest.(check bool) "transport cleared"
    false (Rest.health_transport_degraded state);
  Alcotest.(check (option string)) "transport error cleared"
    None (Rest.health_last_transport_error state)

let test_client_failure_without_backoff_state () =
  let ts seconds = Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)) in
  let state = Rest.create_health_state () in
  let delay =
    Rest.note_failure_without_backoff_state state
      ~now:(ts 10.0) ~summary:"host=discord.com kind=http_401 error=bad auth"
  in
  Alcotest.(check (float 1e-9)) "no backoff delay" 0.0 delay;
  Alcotest.(check int) "failure recorded"
    1 (Rest.health_consecutive_failures state);
  Alcotest.(check (float 1e-9)) "no retry delay"
    0.0 (Rest.health_retry_delay_s ~now:(ts 10.0) state)

let test_response_clears_rest_health () =
  Alcotest.(check bool) "200 clears"
    true (Rest.response_clears_rest_health 200);
  Alcotest.(check bool) "204 clears"
    true (Rest.response_clears_rest_health 204);
  Alcotest.(check bool) "400 does not clear"
    false (Rest.response_clears_rest_health 400);
  Alcotest.(check bool) "404 clears expected missing resources"
    true (Rest.response_clears_rest_health 404);
  Alcotest.(check bool) "500 does not clear"
    false (Rest.response_clears_rest_health 500);
  Alcotest.(check bool) "401 marks rest failure"
    true (Rest.response_marks_rest_failure 401);
  Alcotest.(check bool) "404 does not mark failure"
    false (Rest.response_marks_rest_failure 404);
  Alcotest.(check bool) "429 handled separately"
    false (Rest.response_marks_rest_failure 429)

let test_read_body_reports_truncation () =
  let chunk =
    String.make (Rest.max_body_size + 8192) 'x'
  in
  let body = Cohttp_eio.Body.of_string chunk in
  let (body_str, truncated) =
    Rest.read_body_with_truncation body
  in
  Alcotest.(check bool) "truncated" true truncated;
  Alcotest.(check bool) "not full body"
    true (String.length body_str < String.length chunk)

let test_create_message_reuses_nonce_across_5xx_retry () =
  let attempts = ref [] in
  let call ~sw:_ ~headers:_ ?body meth uri =
    attempts := (meth, Uri.path uri, body_json body) :: !attempts;
    match List.length !attempts with
    | 1 ->
      response
        ~headers:(Http.Header.of_list [("retry-after", "0")])
        500 {|{"message":"server unavailable"}|}
    | _ ->
      response 200
        {|{"id":"m1","channel_id":"c1","author":{"id":"u1","username":"bot"},"content":"hello","timestamp":"2026-06-05T00:00:00.000000+00:00"}|}
  in
  with_fake_rest call @@ fun t ->
  match Rest.create_message t ~channel_id:"c1" ~content:"hello" () with
  | Error err -> Alcotest.fail ("expected retry success: " ^ err)
  | Ok _ ->
    let attempts = List.rev !attempts in
    Alcotest.(check int) "two attempts" 2 (List.length attempts);
    match attempts with
    | [(`POST, _, Some body1); (`POST, _, Some body2)] ->
      Alcotest.(check string) "serialized body reused" body1 body2;
      let open Yojson.Safe.Util in
      let json = Yojson.Safe.from_string body1 in
      Alcotest.(check string) "nonce stable"
        (json |> member "nonce" |> to_string)
        (Yojson.Safe.from_string body2 |> member "nonce" |> to_string);
      Alcotest.(check bool) "enforce nonce"
        true (json |> member "enforce_nonce" |> to_bool)
    | _ -> Alcotest.fail "unexpected attempt shape"

let test_create_channel_does_not_retry_5xx () =
  let attempts = ref 0 in
  let call ~sw:_ ~headers:_ ?body:_ _meth _uri =
    incr attempts;
    response 500 {|{"message":"server unavailable"}|}
  in
  with_fake_rest call @@ fun t ->
  match Rest.create_channel t ~guild_id:"g1" ~name:"new-channel" () with
  | Ok _ -> Alcotest.fail "expected create_channel failure"
  | Error _ ->
    Alcotest.(check int) "single non-idempotent attempt" 1 !attempts

let test_401_marks_rest_health_without_transport_backoff () =
  let call ~sw:_ ~headers:_ ?body:_ _meth _uri =
    response 401 {|{"message":"bad auth"}|}
  in
  with_fake_rest call @@ fun t ->
  match Rest.request t ~meth:`GET ~path:"/gateway/bot" () with
  | Ok _ -> Alcotest.fail "expected 401 failure"
  | Error _ ->
    Alcotest.(check bool) "rest degraded" true (Rest.rest_degraded t);
    Alcotest.(check bool) "transport not degraded"
      false (Rest.transport_degraded t);
    Alcotest.(check (float 1e-9)) "no shared backoff"
      0.0 (Rest.rest_retry_delay_s t);
    Alcotest.(check int) "rest failure recorded"
      1 (Rest.consecutive_rest_failures t);
    Alcotest.(check int) "transport failure not recorded"
      0 (Rest.consecutive_transport_failures t)

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
      Alcotest.test_case "retry_after rejects non-finite values" `Quick
        test_retry_after_rejects_non_finite_values;
      Alcotest.test_case "health summary truncates on UTF-8 boundary" `Quick
        test_body_for_health_summary_preserves_utf8_boundary;
      Alcotest.test_case "health state failure and recovery" `Quick
        test_health_state_failure_and_recovery;
      Alcotest.test_case "health state returns effective shared delay" `Quick
        test_health_state_returns_effective_shared_delay;
      Alcotest.test_case "rate limit state preserves backoff window" `Quick
        test_rate_limit_state_preserves_backoff_window;
      Alcotest.test_case "rest backoff survives unrelated recovery" `Quick
        test_rest_backoff_survives_unrelated_recovery;
      Alcotest.test_case "rest backoff survives transport shadowing" `Quick
        test_rest_backoff_survives_transport_shadowing;
      Alcotest.test_case "rest backoff preserves nonretryable transport error" `Quick
        test_rest_backoff_preserves_error_on_nonretryable_transport;
      Alcotest.test_case "transport backoff survives rest shadowing" `Quick
        test_transport_backoff_survives_rest_shadowing;
      Alcotest.test_case "transport health clears separately" `Quick
        test_transport_health_clears_separately;
      Alcotest.test_case "client failure records without backoff" `Quick
        test_client_failure_without_backoff_state;
      Alcotest.test_case "response_clears_rest_health semantics" `Quick
        test_response_clears_rest_health;
      Alcotest.test_case "read_body reports truncation" `Quick
        test_read_body_reports_truncation;
      Alcotest.test_case "create_message reuses nonce across 5xx retry" `Quick
        test_create_message_reuses_nonce_across_5xx_retry;
      Alcotest.test_case "create_channel does not retry 5xx" `Quick
        test_create_channel_does_not_retry_5xx;
      Alcotest.test_case "401 marks rest health without transport backoff" `Quick
        test_401_marks_rest_health_without_transport_backoff;
    ]);
  ]
