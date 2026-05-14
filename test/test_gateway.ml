let test_payload_diagnostics_valid () =
  let payload = {|{"op":0,"t":"MESSAGE_CREATE","s":123,"d":{}}|} in
  let expected =
    Printf.sprintf "bytes=%d op=Dispatch event=MESSAGE_CREATE seq=123"
      (String.length payload)
  in
  Alcotest.(check string) "valid payload summary"
    expected (Discord_agents.Discord_gateway.payload_diagnostics payload)

let test_payload_diagnostics_invalid_json () =
  let payload = "{not json" in
  let expected =
    Printf.sprintf "bytes=%d invalid_json" (String.length payload)
  in
  Alcotest.(check string) "invalid payload summary"
    expected (Discord_agents.Discord_gateway.payload_diagnostics payload)

let test_websocket_reader_limit_tracks_payload_limit () =
  Alcotest.(check int) "reader limit matches payload cap"
    Discord_agents.Websocket.max_payload_size
    Discord_agents.Websocket.reader_max_size

let () =
  Alcotest.run "gateway" [
    ("gateway", [
      Alcotest.test_case "payload diagnostics valid" `Quick
        test_payload_diagnostics_valid;
      Alcotest.test_case "payload diagnostics invalid json" `Quick
        test_payload_diagnostics_invalid_json;
      Alcotest.test_case "reader limit tracks payload cap" `Quick
        test_websocket_reader_limit_tracks_payload_limit;
    ]);
  ]
