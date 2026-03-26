(** Tests for Discord message formatting: reformat_tables, find_trailing_table_start,
    split_message, and scan_fences. *)

(* ── reformat_tables ────────────────────────────────────────────── *)

let test_reformat_plain_text () =
  let input = "Hello world.\nThis is normal text.\n\nAnother paragraph." in
  Alcotest.(check string) "plain text unchanged"
    input (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_simple_table () =
  let input = "| Name | Value |\n|------|-------|\n| foo  | bar   |" in
  let expected = "```\n| Name | Value |\n|------|-------|\n| foo  | bar   |\n```" in
  Alcotest.(check string) "table wrapped in code block"
    expected (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_table_with_surrounding_text () =
  let input =
    "Here is a table:\n\n| A | B |\n|---|---|\n| 1 | 2 |\n\nText after the table." in
  let expected =
    "Here is a table:\n\n```\n| A | B |\n|---|---|\n| 1 | 2 |\n```\n\nText after the table." in
  Alcotest.(check string) "text before and after table is normal"
    expected (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_table_inside_code_block () =
  let input = "```\n| A | B |\n| 1 | 2 |\n```" in
  Alcotest.(check string) "table inside code block unchanged"
    input (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_table_inside_lang_code_block () =
  let input = "```markdown\n| A | B |\n| 1 | 2 |\n```" in
  Alcotest.(check string) "table inside language code block unchanged"
    input (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_multiple_tables () =
  let input =
    "First:\n| A | B |\n| 1 | 2 |\n\nSecond:\n| C | D |\n| 3 | 4 |" in
  let expected =
    "First:\n```\n| A | B |\n| 1 | 2 |\n```\n\nSecond:\n```\n| C | D |\n| 3 | 4 |\n```" in
  Alcotest.(check string) "multiple tables each wrapped"
    expected (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_empty_string () =
  Alcotest.(check string) "empty string unchanged"
    "" (Discord_agents.Agent_process.reformat_tables "")

let test_reformat_table_then_code_block () =
  let input =
    "| A | B |\n| 1 | 2 |\n\n```ocaml\nlet x = 1\n```\n\nDone." in
  let expected =
    "```\n| A | B |\n| 1 | 2 |\n```\n\n```ocaml\nlet x = 1\n```\n\nDone." in
  Alcotest.(check string) "table then code block both correct"
    expected (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_pipe_in_code_not_table () =
  (* Pipes inside code blocks should not be treated as tables *)
  let input = "```bash\necho hello | grep h\n```" in
  Alcotest.(check string) "pipe in code block not treated as table"
    input (Discord_agents.Agent_process.reformat_tables input)

let test_reformat_single_table_row () =
  let input = "| just one row |" in
  let expected = "```\n| just one row |\n```" in
  Alcotest.(check string) "single row wrapped"
    expected (Discord_agents.Agent_process.reformat_tables input)

let reformat_tables_tests = [
  Alcotest.test_case "plain text" `Quick test_reformat_plain_text;
  Alcotest.test_case "simple table" `Quick test_reformat_simple_table;
  Alcotest.test_case "table with surrounding text" `Quick
    test_reformat_table_with_surrounding_text;
  Alcotest.test_case "table inside code block" `Quick
    test_reformat_table_inside_code_block;
  Alcotest.test_case "table inside lang code block" `Quick
    test_reformat_table_inside_lang_code_block;
  Alcotest.test_case "multiple tables" `Quick test_reformat_multiple_tables;
  Alcotest.test_case "empty string" `Quick test_reformat_empty_string;
  Alcotest.test_case "table then code block" `Quick
    test_reformat_table_then_code_block;
  Alcotest.test_case "pipe in code block" `Quick
    test_reformat_pipe_in_code_not_table;
  Alcotest.test_case "single row" `Quick test_reformat_single_table_row;
]

(* ── find_trailing_table_start ──────────────────────────────────── *)

let test_trailing_table_basic () =
  let input = "some text\n| A | B |\n| 1 | 2 |" in
  let expected = String.length "some text\n" in
  Alcotest.(check (option int)) "finds table start"
    (Some expected) (Discord_agents.Agent_process.find_trailing_table_start input)

let test_trailing_no_table () =
  let input = "just some text\nno tables here" in
  Alcotest.(check (option int)) "no trailing table"
    None (Discord_agents.Agent_process.find_trailing_table_start input)

let test_trailing_entire_table () =
  let input = "| A | B |\n| 1 | 2 |\n| 3 | 4 |" in
  Alcotest.(check (option int)) "entire text is table"
    None (Discord_agents.Agent_process.find_trailing_table_start input)

let test_trailing_table_in_code_block () =
  let input = "text\n```\n| A | B |\n| 1 | 2 |\n```" in
  Alcotest.(check (option int)) "table in code block not trailing"
    None (Discord_agents.Agent_process.find_trailing_table_start input)

let test_trailing_table_with_blank_line () =
  let input = "some text\n\n| A | B |\n| 1 | 2 |" in
  (* The blank line before the table is skipped; offset is at the table row *)
  match Discord_agents.Agent_process.find_trailing_table_start input with
  | None -> Alcotest.fail "expected Some"
  | Some pos ->
    let after = String.sub input pos (String.length input - pos) in
    Alcotest.(check bool) "table content starts with |"
      true (String.length after > 0 && after.[0] = '|')

let test_trailing_empty () =
  Alcotest.(check (option int)) "empty string"
    None (Discord_agents.Agent_process.find_trailing_table_start "")

let trailing_table_tests = [
  Alcotest.test_case "basic" `Quick test_trailing_table_basic;
  Alcotest.test_case "no table" `Quick test_trailing_no_table;
  Alcotest.test_case "entire table" `Quick test_trailing_entire_table;
  Alcotest.test_case "table in code block" `Quick
    test_trailing_table_in_code_block;
  Alcotest.test_case "with blank line" `Quick
    test_trailing_table_with_blank_line;
  Alcotest.test_case "empty" `Quick test_trailing_empty;
]

(* ── split_message ──────────────────────────────────────────────── *)

let test_split_short () =
  let input = "short message" in
  Alcotest.(check (list string)) "short message not split"
    [input] (Discord_agents.Agent_process.split_message input)

let test_split_long_at_paragraph () =
  (* Build text with two paragraphs, total > 1900 chars *)
  let para1 = String.make 1000 'a' in
  let para2 = String.make 1000 'b' in
  let input = para1 ^ "\n\n" ^ para2 in
  let chunks = Discord_agents.Agent_process.split_message input in
  Alcotest.(check bool) "splits into multiple chunks"
    true (List.length chunks >= 2);
  List.iter (fun chunk ->
    Alcotest.(check bool) "each chunk under limit"
      true (String.length chunk <= 1900)
  ) chunks

let test_split_preserves_code_blocks () =
  (* A code block that spans the split boundary *)
  let before = String.make 1800 'x' in
  let input = before ^ "\n```ocaml\nlet x = 1\n```" in
  let chunks = Discord_agents.Agent_process.split_message input in
  (* Every chunk should have balanced ``` fences *)
  List.iter (fun chunk ->
    let (in_code, _) = Discord_agents.Agent_process.scan_fences chunk in
    Alcotest.(check bool) "code blocks balanced in chunk"
      false in_code
  ) chunks

let test_split_all_chunks_under_limit () =
  let input = String.make 5000 'z' in
  let chunks = Discord_agents.Agent_process.split_message input in
  List.iter (fun chunk ->
    Alcotest.(check bool) "chunk under 1900"
      true (String.length chunk <= 1900)
  ) chunks;
  (* All content preserved *)
  let total = List.fold_left (fun acc c -> acc + String.length c) 0 chunks in
  Alcotest.(check bool) "total content preserved"
    true (total >= 5000)

let split_message_tests = [
  Alcotest.test_case "short message" `Quick test_split_short;
  Alcotest.test_case "split at paragraph" `Quick test_split_long_at_paragraph;
  Alcotest.test_case "preserves code blocks" `Quick
    test_split_preserves_code_blocks;
  Alcotest.test_case "all chunks under limit" `Quick
    test_split_all_chunks_under_limit;
]

(* ── scan_fences ────────────────────────────────────────────────── *)

let test_scan_no_fences () =
  let (in_code, lang) = Discord_agents.Agent_process.scan_fences "plain text" in
  Alcotest.(check bool) "not in code" false in_code;
  Alcotest.(check string) "no lang" "" lang

let test_scan_open_fence () =
  let (in_code, lang) =
    Discord_agents.Agent_process.scan_fences "text\n```ocaml\nlet x = 1" in
  Alcotest.(check bool) "in code" true in_code;
  Alcotest.(check string) "lang is ocaml" "ocaml" lang

let test_scan_closed_fence () =
  let (in_code, _) =
    Discord_agents.Agent_process.scan_fences "```\ncode\n```" in
  Alcotest.(check bool) "not in code" false in_code

let test_scan_bare_fence () =
  let (in_code, lang) =
    Discord_agents.Agent_process.scan_fences "text\n```\ncode" in
  Alcotest.(check bool) "in code" true in_code;
  Alcotest.(check string) "no lang" "" lang

let scan_fences_tests = [
  Alcotest.test_case "no fences" `Quick test_scan_no_fences;
  Alcotest.test_case "open fence" `Quick test_scan_open_fence;
  Alcotest.test_case "closed fence" `Quick test_scan_closed_fence;
  Alcotest.test_case "bare fence" `Quick test_scan_bare_fence;
]

(* ── runner ──────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "discord_formatting" [
    "reformat_tables", reformat_tables_tests;
    "find_trailing_table_start", trailing_table_tests;
    "split_message", split_message_tests;
    "scan_fences", scan_fences_tests;
  ]
