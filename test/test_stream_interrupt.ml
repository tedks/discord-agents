(** Deterministic JSONL interrupt tests.

    These fixtures are captured line shapes from the three agent stream
    modes. The simulated interrupt happens after complete JSONL records
    have been emitted and while the next record is only partially written.
    Production reads line-oriented JSON, so the incomplete tail must not be
    handed to a line parser; already parsed events must remain intact. *)

type expected = {
  text : string;
  session_id : string option;
  tool_name : string option;
}

let parse_complete_jsonl ~parse chunk =
  let len = String.length chunk in
  let lines = String.split_on_char '\n' chunk in
  let complete_lines =
    if len > 0 && chunk.[len - 1] <> '\n' then
      match List.rev lines with
      | _partial :: rev_complete -> List.rev rev_complete
      | [] -> []
    else
      lines
  in
  complete_lines
  |> List.filter (fun line -> line <> "")
  |> List.concat_map parse

let has_text expected =
  List.exists (function
    | Discord_agents.Agent_process.Text_delta text -> text = expected
    | _ -> false)

let has_session_id expected =
  List.exists (function
    | Discord_agents.Agent_process.Result { session_id = Some sid; _ } ->
      sid = expected
    | _ -> false)

let has_tool expected =
  List.exists (function
    | Discord_agents.Agent_process.Tool_use { tool_name; _ } ->
      tool_name = expected
    | _ -> false)

let no_raw_tail tail =
  List.for_all (function
    | Discord_agents.Agent_process.Other raw -> raw <> tail
    | _ -> true)

let assert_interrupt_preserves_complete_events
    ~name ~parse ~complete_lines ~truncated_tail expected =
  let stream =
    String.concat "\n" complete_lines ^ "\n" ^ truncated_tail
  in
  let events = parse_complete_jsonl ~parse stream in
  Alcotest.(check bool) (name ^ " text survives interrupt")
    true (has_text expected.text events);
  (match expected.session_id with
   | Some sid ->
     Alcotest.(check bool) (name ^ " session id survives interrupt")
       true (has_session_id sid events)
   | None -> ());
  (match expected.tool_name with
   | Some tool_name ->
     Alcotest.(check bool) (name ^ " tool event survives interrupt")
       true (has_tool tool_name events)
   | None -> ());
  Alcotest.(check bool) (name ^ " truncated tail is not parsed")
    true (no_raw_tail truncated_tail events)

let test_claude_interrupt_preserves_events () =
  assert_interrupt_preserves_complete_events
    ~name:"claude"
    ~parse:Discord_agents.Agent_process.parse_stream_json_line
    ~complete_lines:[
      {|{"type":"assistant","message":{"content":[{"type":"text","text":"planning complete\n"}]}}|};
      {|{"type":"assistant","message":{"content":[{"type":"tool_use","id":"toolu_01A","name":"Bash","input":{"command":"sleep 30","description":"simulate long work"}}]}}|};
    ]
    ~truncated_tail:{|{"type":"assistant","message":{"content":[{"type":"text","text":"partial|}
    { text = "planning complete\n"; session_id = None; tool_name = Some "Bash" }

let test_codex_interrupt_preserves_events () =
  assert_interrupt_preserves_complete_events
    ~name:"codex"
    ~parse:Discord_agents.Agent_process.parse_codex_json_line
    ~complete_lines:[
      {|{"type":"thread.started","thread_id":"codex-thread-123"}|};
      {|{"type":"item.completed","item":{"id":"msg_0","type":"agent_message","text":"ready before interrupt"}}|};
      {|{"type":"item.started","item":{"id":"cmd_0","type":"command_execution","command":"sleep 30","status":"in_progress"}}|};
    ]
    ~truncated_tail:{|{"type":"item.completed","item":{"id":"cmd_0","type":"command_execution","aggregated_output":"|}
    { text = "ready before interrupt\n\n";
      session_id = Some "codex-thread-123";
      tool_name = Some "Bash" }

let test_gemini_interrupt_preserves_events () =
  assert_interrupt_preserves_complete_events
    ~name:"gemini"
    ~parse:Discord_agents.Agent_process.parse_gemini_stream_json_line
    ~complete_lines:[
      {|{"type":"init","session_id":"gemini-session-123","model":"gemini-2.5-pro"}|};
      {|{"type":"message","role":"assistant","content":"ready before interrupt","delta":true}|};
      {|{"type":"tool_use","tool_name":"run_shell_command","parameters":{"command":"sleep 30","description":"simulate long work"}}|};
    ]
    ~truncated_tail:{|{"type":"tool_result","status":"success","output":"|}
    { text = "ready before interrupt";
      session_id = Some "gemini-session-123";
      tool_name = Some "Bash" }

let () =
  Alcotest.run "stream_interrupt" [
    "jsonl truncation", [
      Alcotest.test_case "claude complete events survive partial tail" `Quick
        test_claude_interrupt_preserves_events;
      Alcotest.test_case "codex complete events survive partial tail" `Quick
        test_codex_interrupt_preserves_events;
      Alcotest.test_case "gemini complete events survive partial tail" `Quick
        test_gemini_interrupt_preserves_events;
    ];
  ]
