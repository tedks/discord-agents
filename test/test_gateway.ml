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

let test_payload_diagnostics_wrong_types () =
  let payload = {|{"op":"x","t":42,"s":"oops"}|} in
  let expected =
    Printf.sprintf "bytes=%d op=? event=? seq=?"
      (String.length payload)
  in
  Alcotest.(check string) "wrong-typed payload summary"
    expected (Discord_agents.Discord_gateway.payload_diagnostics payload)

let test_websocket_reader_limit_tracks_payload_limit () =
  Alcotest.(check int) "reader limit matches payload cap"
    Discord_agents.Websocket.max_payload_size
    Discord_agents.Websocket.reader_max_size

let websocket_frame_header ~fin ~opcode payload_len =
  let b0 = (if fin then 0x80 else 0x00) lor opcode in
  let buf = Buffer.create 10 in
  Buffer.add_char buf (Char.chr b0);
  if payload_len < 126 then
    Buffer.add_char buf (Char.chr payload_len)
  else if payload_len < 65536 then begin
    Buffer.add_char buf (Char.chr 126);
    Buffer.add_char buf (Char.chr (payload_len lsr 8));
    Buffer.add_char buf (Char.chr (payload_len land 0xff))
  end else begin
    Buffer.add_char buf (Char.chr 127);
    for i = 7 downto 0 do
      Buffer.add_char buf (Char.chr ((payload_len lsr (i * 8)) land 0xff))
    done
  end;
  Buffer.contents buf

let websocket_frame ~fin ~opcode payload =
  websocket_frame_header ~fin ~opcode (String.length payload) ^ payload

let websocket_of_input input =
  let flow = Eio_mock.Flow.make "websocket-test" in
  {
    Discord_agents.Websocket.flow =
      (flow :> Discord_agents.Websocket.closable_flow Eio.Resource.t);
    reader = Eio.Buf_read.of_string input;
    closed = false;
  }

let check_failure_contains name needle f =
  match f () with
  | exception Failure msg ->
    Alcotest.(check bool) name true
      (String.starts_with ~prefix:needle msg)
  | exception exn ->
    Alcotest.failf "%s: unexpected exception %s" name (Printexc.to_string exn)
  | _ ->
    Alcotest.failf "%s: expected Failure" name

let test_websocket_rejects_oversized_frame_header () =
  let oversized_len = Discord_agents.Websocket.max_payload_size + 1 in
  let ws =
    websocket_of_input
      (websocket_frame_header ~fin:true ~opcode:1 oversized_len)
  in
  check_failure_contains "oversized frame" "websocket: payload too large"
    (fun () -> ignore (Discord_agents.Websocket.recv_frame ws))

let test_websocket_rejects_fragment_accumulation_overflow () =
  let first_payload =
    String.make Discord_agents.Websocket.max_payload_size 'a'
  in
  let input =
    websocket_frame ~fin:false ~opcode:1 first_payload
    ^ websocket_frame ~fin:true ~opcode:0 "b"
  in
  let ws = websocket_of_input input in
  check_failure_contains "fragment overflow"
    "websocket: accumulated fragments too large"
    (fun () -> ignore (Discord_agents.Websocket.recv_frame ws))

let () =
  Alcotest.run "gateway" [
    ("gateway", [
      Alcotest.test_case "payload diagnostics valid" `Quick
        test_payload_diagnostics_valid;
      Alcotest.test_case "payload diagnostics invalid json" `Quick
        test_payload_diagnostics_invalid_json;
      Alcotest.test_case "payload diagnostics wrong types" `Quick
        test_payload_diagnostics_wrong_types;
      Alcotest.test_case "reader limit tracks payload cap" `Quick
        test_websocket_reader_limit_tracks_payload_limit;
      Alcotest.test_case "websocket rejects oversized frame header" `Quick
        test_websocket_rejects_oversized_frame_header;
      Alcotest.test_case "websocket rejects fragment accumulation overflow" `Quick
        test_websocket_rejects_fragment_accumulation_overflow;
    ]);
  ]
