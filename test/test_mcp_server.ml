module Mcp_server = Discord_agents.Mcp_server
module Mcp_tool = Discord_agents.Mcp_tool

let failf fmt = Format.kasprintf (fun message -> Alcotest.fail message) fmt

let rec canonical_json = function
  | `Assoc fields ->
    fields
    |> List.map (fun (key, value) -> key, canonical_json value)
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> fun fields -> `Assoc fields
  | `List values -> `List (List.map canonical_json values)
  | other -> other

let json_string json =
  Yojson.Safe.to_string (canonical_json json)

let check_json label expected actual =
  Alcotest.(check string) label (json_string expected) (json_string actual)

let response ~id ~result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ]

let text_result ?(is_error=false) text =
  let fields = [
    ("content", `List [
      `Assoc [
        ("type", `String "text");
        ("text", `String text);
      ];
    ]);
  ] in
  `Assoc (if is_error then fields @ [("isError", `Bool true)] else fields)

let handler_calls = ref []

let recording_handler result call =
  handler_calls := call :: !handler_calls;
  result

let handle_line ?(handler=recording_handler (Ok "handled")) line =
  Mcp_server.handle_line ~handle_tool_call:handler line

let expect_response label ?handler line expected =
  match handle_line ?handler line with
  | None -> failf "%s: expected a response" label
  | Some actual -> check_json label expected actual

let expect_no_response label line =
  match handle_line line with
  | None -> ()
  | Some actual ->
    failf "%s: expected no response, got %s"
      label (Yojson.Safe.to_string actual)

let test_initialize () =
  expect_response "initialize"
    {|{"jsonrpc":"2.0","id":1,"method":"initialize"}|}
    (response ~id:(`Int 1) ~result:Mcp_server.initialize_result)

let test_notifications_initialized_is_ignored () =
  expect_no_response "initialized notification"
    {|{"jsonrpc":"2.0","method":"notifications/initialized"}|}

let test_tools_list_uses_typed_descriptors () =
  expect_response "tools/list"
    {|{"jsonrpc":"2.0","id":"tools","method":"tools/list"}|}
    (response ~id:(`String "tools")
       ~result:(`Assoc [("tools", Mcp_tool.tool_definitions_json)]))

let test_ping () =
  expect_response "ping"
    {|{"jsonrpc":"2.0","id":2,"method":"ping"}|}
    (response ~id:(`Int 2) ~result:(`Assoc []))

let test_tools_call_invokes_handler () =
  handler_calls := [];
  let handler =
    recording_handler (Ok "sent")
  in
  expect_response "tools/call" ~handler
    {|{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"send_message","arguments":{"thread_id":"123","message":"hello"}}}|}
    (response ~id:(`Int 3) ~result:(text_result "sent"));
  match !handler_calls with
  | [{ Mcp_server.name; arguments }] ->
    Alcotest.(check string) "tool name" "send_message" name;
    check_json "arguments"
      (`Assoc [
        ("thread_id", `String "123");
        ("message", `String "hello");
      ])
      arguments
  | calls ->
    failf "expected one handler call, got %d" (List.length calls)

let test_tools_call_defaults_arguments () =
  handler_calls := [];
  let handler =
    recording_handler (Ok "listed")
  in
  expect_response "tools/call default args" ~handler
    {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"list_projects"}}|}
    (response ~id:(`Int 4) ~result:(text_result "listed"));
  match !handler_calls with
  | [{ Mcp_server.name; arguments }] ->
    Alcotest.(check string) "tool name" "list_projects" name;
    check_json "arguments" (`Assoc []) arguments
  | calls ->
    failf "expected one handler call, got %d" (List.length calls)

let test_tools_call_error_result () =
  let handler _call = Error "not yet" in
  expect_response "tools/call error" ~handler
    {|{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"x"}}|}
    (response ~id:(`Int 5) ~result:(text_result ~is_error:true "not yet"))

let test_tools_call_exception_becomes_tool_error () =
  let handler _call = failwith "boom" in
  expect_response "tools/call exception" ~handler
    {|{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"x"}}|}
    (response ~id:(`Int 6)
       ~result:(text_result ~is_error:true "Error: Failure(\"boom\")"))

let test_unknown_request_returns_jsonrpc_error () =
  expect_response "unknown request"
    {|{"jsonrpc":"2.0","id":7,"method":"missing"}|}
    (`Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int 7);
      ("error", `Assoc [
        ("code", `Int (-32601));
        ("message", `String "Unknown method: missing");
      ]);
    ])

let test_unknown_notification_is_ignored () =
  expect_no_response "unknown notification"
    {|{"jsonrpc":"2.0","method":"missing"}|}

let test_invalid_json_is_ignored () =
  expect_no_response "invalid json" "{"

let () =
  Alcotest.run "mcp_server" [
    ("protocol", [
      Alcotest.test_case "initialize" `Quick test_initialize;
      Alcotest.test_case "initialized notification" `Quick
        test_notifications_initialized_is_ignored;
      Alcotest.test_case "tools/list descriptors" `Quick
        test_tools_list_uses_typed_descriptors;
      Alcotest.test_case "ping" `Quick test_ping;
      Alcotest.test_case "tools/call invokes handler" `Quick
        test_tools_call_invokes_handler;
      Alcotest.test_case "tools/call defaults arguments" `Quick
        test_tools_call_defaults_arguments;
      Alcotest.test_case "tools/call error result" `Quick
        test_tools_call_error_result;
      Alcotest.test_case "tools/call exception" `Quick
        test_tools_call_exception_becomes_tool_error;
      Alcotest.test_case "unknown request" `Quick
        test_unknown_request_returns_jsonrpc_error;
      Alcotest.test_case "unknown notification" `Quick
        test_unknown_notification_is_ignored;
      Alcotest.test_case "invalid json" `Quick test_invalid_json_is_ignored;
    ]);
  ]
