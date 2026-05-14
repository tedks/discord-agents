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
    (classify "Failure(\"connection reset by peer\")")

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

let () =
  Alcotest.run "discord_rest" [
    ("discord_rest", [
      Alcotest.test_case "create_message_body enforces nonce" `Quick
        test_create_message_body_enforces_nonce;
      Alcotest.test_case "transport error classification" `Quick
        test_transport_error_classification;
      Alcotest.test_case "transport backoff caps" `Quick
        test_transport_backoff_caps;
    ]);
  ]
