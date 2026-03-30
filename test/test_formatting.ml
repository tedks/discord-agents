(** Tests for Discord message formatting: reformat_tables, find_trailing_table_start,
    split_message, and scan_fences. *)

let reformat = Discord_agents.Agent_process.reformat_tables

(* ── reformat_tables ────────────────────────────────────────────── *)

let test_reformat_plain_text () =
  let input = "Hello world.\nThis is normal text.\n\nAnother paragraph." in
  Alcotest.(check string) "plain text unchanged"
    input (reformat input)

let test_reformat_simple_table () =
  let input = "| Name | Value |\n|------|-------|\n| foo  | bar   |" in
  let expected = String.concat "\n" [
    "```";
    "| Name | Value |";
    "| ---- | ----- |";
    "| foo  | bar   |";
    "```" ] in
  Alcotest.(check string) "table wrapped and padded"
    expected (reformat input)

let test_reformat_table_with_surrounding_text () =
  let input =
    "Here is a table:\n\n| A | B |\n|---|---|\n| 1 | 2 |\n\nText after the table." in
  let result = reformat input in
  (* Text before table is unchanged *)
  Alcotest.(check bool) "starts with intro text"
    true (String.length result > 16
          && String.sub result 0 16 = "Here is a table:");
  (* Text after table is unchanged *)
  Alcotest.(check bool) "ends with outro text"
    true (let suffix = "Text after the table." in
          let rlen = String.length result in
          let slen = String.length suffix in
          rlen >= slen && String.sub result (rlen - slen) slen = suffix);
  (* Table is inside code block *)
  Alcotest.(check bool) "contains code block"
    true (let has s = try ignore (Str.search_forward (Str.regexp_string s) result 0); true
          with Not_found -> false in
          has "```\n|" && has "|\n```")

let test_reformat_table_inside_code_block () =
  let input = "```\n| A | B |\n| 1 | 2 |\n```" in
  Alcotest.(check string) "table inside code block unchanged"
    input (reformat input)

let test_reformat_table_inside_lang_code_block () =
  let input = "```markdown\n| A | B |\n| 1 | 2 |\n```" in
  Alcotest.(check string) "table inside language code block unchanged"
    input (reformat input)

let test_reformat_multiple_tables () =
  let input =
    "First:\n| A | B |\n| 1 | 2 |\n\nSecond:\n| C | D |\n| 3 | 4 |" in
  let result = reformat input in
  (* Count code block pairs *)
  let count_occurrences _sub s =
    let rec aux pos acc =
      match String.index_from_opt s pos '`' with
      | None -> acc
      | Some i ->
        if i + 2 < String.length s && s.[i+1] = '`' && s.[i+2] = '`' then
          aux (i + 3) (acc + 1)
        else aux (i + 1) acc
    in aux 0 0
  in
  (* 2 tables × 2 fences each = 4 total ``` *)
  Alcotest.(check int) "four fence markers"
    4 (count_occurrences "```" result);
  Alcotest.(check bool) "starts with First:"
    true (String.length result >= 6 && String.sub result 0 6 = "First:")

let test_reformat_empty_string () =
  Alcotest.(check string) "empty string unchanged"
    "" (reformat "")

let test_reformat_table_then_code_block () =
  let input =
    "| A | B |\n| 1 | 2 |\n\n```ocaml\nlet x = 1\n```\n\nDone." in
  let result = reformat input in
  Alcotest.(check bool) "ends with Done."
    true (let s = "Done." in let rlen = String.length result in
          rlen >= 5 && String.sub result (rlen - String.length s) (String.length s) = s);
  Alcotest.(check bool) "contains ocaml code block"
    true (try ignore (Str.search_forward (Str.regexp_string "```ocaml") result 0); true
          with Not_found -> false)

let test_reformat_pipe_in_code_not_table () =
  let input = "```bash\necho hello | grep h\n```" in
  Alcotest.(check string) "pipe in code block not treated as table"
    input (reformat input)

let test_reformat_single_table_row () =
  let input = "| just one row |" in
  let result = reformat input in
  Alcotest.(check bool) "wrapped in code block"
    true (String.length result >= 3 && String.sub result 0 3 = "```");
  Alcotest.(check bool) "contains the row"
    true (try ignore (Str.search_forward (Str.regexp_string "just one row") result 0); true
          with Not_found -> false)

let test_reformat_padding_alignment () =
  (* Columns with uneven widths get padded *)
  let input = "| x | longer column |\n| ab | c |" in
  let result = reformat input in
  let lines = String.split_on_char '\n' result in
  (* Skip ``` lines, check data rows have same length *)
  let data_lines = List.filter (fun l ->
    String.length l > 0 && l.[0] = '|') lines in
  (match data_lines with
   | a :: b :: _ ->
     Alcotest.(check int) "rows have equal length"
       (String.length a) (String.length b)
   | _ -> Alcotest.fail "expected at least 2 data lines")

let test_reformat_separator_regenerated () =
  (* Separator row should be regenerated with dashes matching column widths *)
  let input = "| Name | Value |\n|---|---|\n| foo | bar |" in
  let result = reformat input in
  let lines = String.split_on_char '\n' result in
  let sep_lines = List.filter (fun l ->
    String.length l > 0 && l.[0] = '|' &&
    String.to_seq l |> Seq.exists (fun c -> c = '-')
  ) lines in
  (match sep_lines with
   | sep :: _ ->
     (* Separator should have dashes padded to column width *)
     Alcotest.(check bool) "separator has padded dashes"
       true (String.length sep > 10)
   | [] -> Alcotest.fail "no separator row found")

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
  Alcotest.test_case "padding alignment" `Quick test_reformat_padding_alignment;
  Alcotest.test_case "separator regenerated" `Quick
    test_reformat_separator_regenerated;
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

(* ── wrap_line ──────────────────────────────────────────────────── *)

let test_wrap_short_line () =
  let result = Discord_agents.Agent_process.wrap_line ~max_width:40 "short line" in
  Alcotest.(check (list string)) "short line unchanged"
    ["short line"] result

let test_wrap_long_line () =
  let input = "this is a line that is longer than twenty characters" in
  let result = Discord_agents.Agent_process.wrap_line ~max_width:20 input in
  Alcotest.(check bool) "produces multiple lines"
    true (List.length result >= 2);
  List.iter (fun line ->
    Alcotest.(check bool) "each line within limit"
      true (String.length line <= 20)
  ) result;
  (* All words preserved *)
  let rejoined = String.concat " " result in
  Alcotest.(check string) "words preserved" input rejoined

let test_wrap_single_long_word () =
  let word = String.make 30 'x' in
  let result = Discord_agents.Agent_process.wrap_line ~max_width:20 word in
  (* Single word exceeding limit is kept intact *)
  Alcotest.(check (list string)) "long word kept intact"
    [word] result

let wrap_line_tests = [
  Alcotest.test_case "short line" `Quick test_wrap_short_line;
  Alcotest.test_case "long line" `Quick test_wrap_long_line;
  Alcotest.test_case "single long word" `Quick test_wrap_single_long_word;
]

(* ── wrap_text ──────────────────────────────────────────────────── *)

let test_wrap_text_plain () =
  let input = "this is a line that is longer than twenty characters" in
  let result = Discord_agents.Agent_process.wrap_text ~max_width:20 input in
  let lines = String.split_on_char '\n' result in
  List.iter (fun line ->
    Alcotest.(check bool) "each line within limit"
      true (String.length line <= 20)
  ) lines

let test_wrap_text_preserves_code_blocks () =
  let long_code = String.make 50 'x' in
  let input = "```\n" ^ long_code ^ "\n```" in
  let result = Discord_agents.Agent_process.wrap_text ~max_width:20 input in
  Alcotest.(check string) "code block unchanged"
    input result

let test_wrap_text_mixed () =
  let long_text = "this is some text that should be wrapped at the limit" in
  let long_code = String.make 80 'y' in
  let input = long_text ^ "\n```\n" ^ long_code ^ "\n```" in
  let result = Discord_agents.Agent_process.wrap_text ~max_width:20 input in
  (* Code line should be preserved exactly *)
  Alcotest.(check bool) "code line preserved"
    true (try ignore (Str.search_forward (Str.regexp_string long_code) result 0);
              true
          with Not_found -> false)

let wrap_text_tests = [
  Alcotest.test_case "plain text" `Quick test_wrap_text_plain;
  Alcotest.test_case "preserves code blocks" `Quick
    test_wrap_text_preserves_code_blocks;
  Alcotest.test_case "mixed content" `Quick test_wrap_text_mixed;
]

(* ── table width constraining ──────────────────────────────────── *)

let test_reformat_table_narrow_width () =
  let input = "| Name | Description | Status |\n|------|-------------|--------|\n| foo  | a long description here | active |" in
  let result = Discord_agents.Agent_process.reformat_tables ~max_width:40 input in
  let lines = String.split_on_char '\n' result in
  (* All lines (including table rows) should be <= 40 chars *)
  let data_lines = List.filter (fun l ->
    String.length l > 0 && l.[0] = '|') lines in
  List.iter (fun line ->
    Alcotest.(check bool)
      (Printf.sprintf "line within limit: %d <= 40" (String.length line))
      true (String.length line <= 40)
  ) data_lines

let test_reformat_table_default_width_unchanged () =
  (* A small table should be unaffected by width constraint *)
  let input = "| A | B |\n|---|---|\n| 1 | 2 |" in
  let narrow = Discord_agents.Agent_process.reformat_tables ~max_width:120 input in
  let default = Discord_agents.Agent_process.reformat_tables input in
  Alcotest.(check string) "same output at default width"
    default narrow

let table_width_tests = [
  Alcotest.test_case "narrow width constrains table" `Quick
    test_reformat_table_narrow_width;
  Alcotest.test_case "default width unchanged" `Quick
    test_reformat_table_default_width_unchanged;
]

(* ── command parsing ──────────────────────────────────────────────── *)

let cmd_testable =
  let pp fmt cmd =
    let s = match cmd with
      | Discord_agents.Command.List_projects -> "List_projects"
      | List_sessions -> "List_sessions"
      | List_claude_sessions -> "List_claude_sessions"
      | Start_agent { project; _ } -> "Start_agent(" ^ project ^ ")"
      | Resume_session { session_id } -> "Resume_session(" ^ session_id ^ ")"
      | Stop_session { thread_id } -> "Stop_session(" ^ thread_id ^ ")"
      | Cleanup_channels -> "Cleanup_channels"
      | Restart -> "Restart"
      | Rename_thread _ -> "Rename_thread"
      | Status -> "Status"
      | Desktop -> "Desktop"
      | Mobile -> "Mobile"
      | Wrapping None -> "Wrapping(None)"
      | Wrapping (Some n) -> Printf.sprintf "Wrapping(Some %d)" n
      | Help -> "Help"
      | Unknown s -> "Unknown(" ^ s ^ ")"
    in
    Format.fprintf fmt "%s" s
  in
  let eq a b = (pp Format.str_formatter a; let sa = Format.flush_str_formatter () in
                pp Format.str_formatter b; let sb = Format.flush_str_formatter () in
                sa = sb) in
  Alcotest.testable pp eq

let test_parse_desktop () =
  Alcotest.(check cmd_testable) "desktop command"
    Discord_agents.Command.Desktop
    (Discord_agents.Command.parse "!desktop")

let test_parse_mobile () =
  Alcotest.(check cmd_testable) "mobile command"
    Discord_agents.Command.Mobile
    (Discord_agents.Command.parse "!mobile")

let test_parse_wrapping_no_arg () =
  Alcotest.(check cmd_testable) "wrapping without arg"
    (Discord_agents.Command.Wrapping None)
    (Discord_agents.Command.parse "!wrapping")

let test_parse_wrapping_with_arg () =
  Alcotest.(check cmd_testable) "wrapping with arg"
    (Discord_agents.Command.Wrapping (Some 80))
    (Discord_agents.Command.parse "!wrapping 80")

let test_parse_wrapping_invalid () =
  let result = Discord_agents.Command.parse "!wrapping abc" in
  match result with
  | Discord_agents.Command.Unknown _ ->
    Alcotest.(check pass) "invalid wrapping is Unknown" () ()
  | _ -> Alcotest.fail "expected Unknown for invalid wrapping arg"

let test_parse_wrapping_zero () =
  let result = Discord_agents.Command.parse "!wrapping 0" in
  match result with
  | Discord_agents.Command.Unknown _ ->
    Alcotest.(check pass) "zero wrapping is Unknown" () ()
  | _ -> Alcotest.fail "expected Unknown for zero wrapping"

let test_parse_wrapping_negative () =
  let result = Discord_agents.Command.parse "!wrapping -5" in
  match result with
  | Discord_agents.Command.Unknown _ ->
    Alcotest.(check pass) "negative wrapping is Unknown" () ()
  | _ -> Alcotest.fail "expected Unknown for negative wrapping"

let command_tests = [
  Alcotest.test_case "desktop" `Quick test_parse_desktop;
  Alcotest.test_case "mobile" `Quick test_parse_mobile;
  Alcotest.test_case "wrapping no arg" `Quick test_parse_wrapping_no_arg;
  Alcotest.test_case "wrapping with arg" `Quick test_parse_wrapping_with_arg;
  Alcotest.test_case "wrapping invalid" `Quick test_parse_wrapping_invalid;
  Alcotest.test_case "wrapping zero" `Quick test_parse_wrapping_zero;
  Alcotest.test_case "wrapping negative" `Quick test_parse_wrapping_negative;
]

(* ── tool detail formatting ────────────────────────────────────── *)

let detail = Discord_agents.Agent_process.detail_of_tool_input

let test_detail_edit_diff () =
  let input = `Assoc [
    ("file_path", `String "lib/foo.ml");
    ("old_string", `String "let x = 1");
    ("new_string", `String "let x = 2");
  ] in
  let result = detail "Edit" input in
  Alcotest.(check bool) "contains diff fence"
    true (try ignore (Str.search_forward (Str.regexp_string "```diff") result 0);
              true with Not_found -> false);
  Alcotest.(check bool) "contains minus line"
    true (try ignore (Str.search_forward (Str.regexp_string "- let x = 1") result 0);
              true with Not_found -> false);
  Alcotest.(check bool) "contains plus line"
    true (try ignore (Str.search_forward (Str.regexp_string "+ let x = 2") result 0);
              true with Not_found -> false)

let test_detail_bash_command () =
  let input = `Assoc [("command", `String "dune build && dune test")] in
  let result = detail "Bash" input in
  Alcotest.(check bool) "contains bash fence"
    true (try ignore (Str.search_forward (Str.regexp_string "```bash") result 0);
              true with Not_found -> false);
  Alcotest.(check bool) "contains command"
    true (try ignore (Str.search_forward
            (Str.regexp_string "dune build && dune test") result 0);
              true with Not_found -> false)

let test_detail_write_with_lang () =
  let input = `Assoc [
    ("file_path", `String "src/main.py");
    ("content", `String "print('hello')");
  ] in
  let result = detail "Write" input in
  Alcotest.(check bool) "contains python fence"
    true (try ignore (Str.search_forward (Str.regexp_string "```python") result 0);
              true with Not_found -> false);
  Alcotest.(check bool) "contains content"
    true (try ignore (Str.search_forward
            (Str.regexp_string "print('hello')") result 0);
              true with Not_found -> false)

let test_detail_read_empty () =
  let input = `Assoc [("file_path", `String "lib/foo.ml")] in
  let result = detail "Read" input in
  Alcotest.(check string) "Read has no detail" "" result

let test_detail_grep () =
  let input = `Assoc [("pattern", `String "TODO|FIXME")] in
  let result = detail "Grep" input in
  Alcotest.(check bool) "contains pattern"
    true (try ignore (Str.search_forward (Str.regexp_string "TODO|FIXME") result 0);
              true with Not_found -> false)

let test_detail_unknown_tool () =
  let input = `Assoc [("foo", `String "bar")] in
  let result = detail "SomeTool" input in
  Alcotest.(check string) "unknown tool has no detail" "" result

let test_detail_truncation () =
  let long_cmd = String.make 1500 'x' in
  let input = `Assoc [("command", `String long_cmd)] in
  let result = detail "Bash" input in
  Alcotest.(check bool) "truncated with ..."
    true (try ignore (Str.search_forward (Str.regexp_string "...") result 0);
              true with Not_found -> false);
  (* Total should be well under 2000 chars *)
  Alcotest.(check bool) "total under 1000"
    true (String.length result < 1000)

let test_lang_of_path () =
  let lang = Discord_agents.Agent_process.lang_of_path in
  Alcotest.(check string) "ocaml" "ocaml" (lang "lib/foo.ml");
  Alcotest.(check string) "python" "python" (lang "script.py");
  Alcotest.(check string) "bash" "bash" (lang "run.sh");
  Alcotest.(check string) "rust" "rust" (lang "src/main.rs");
  Alcotest.(check string) "no extension" "" (lang "Makefile");
  Alcotest.(check string) "nix" "nix" (lang "flake.nix")

let tool_detail_tests = [
  Alcotest.test_case "edit diff" `Quick test_detail_edit_diff;
  Alcotest.test_case "bash command" `Quick test_detail_bash_command;
  Alcotest.test_case "write with lang" `Quick test_detail_write_with_lang;
  Alcotest.test_case "read empty" `Quick test_detail_read_empty;
  Alcotest.test_case "grep pattern" `Quick test_detail_grep;
  Alcotest.test_case "unknown tool" `Quick test_detail_unknown_tool;
  Alcotest.test_case "truncation" `Quick test_detail_truncation;
  Alcotest.test_case "lang_of_path" `Quick test_lang_of_path;
]

(* ── runner ──────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "discord_formatting" [
    "reformat_tables", reformat_tables_tests;
    "find_trailing_table_start", trailing_table_tests;
    "split_message", split_message_tests;
    "scan_fences", scan_fences_tests;
    "wrap_line", wrap_line_tests;
    "wrap_text", wrap_text_tests;
    "table_width", table_width_tests;
    "command_parsing", command_tests;
    "tool_detail", tool_detail_tests;
  ]
