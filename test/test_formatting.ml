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

let test_split_no_separator_utf8_safe () =
  (* Long unbroken non-ASCII (e.g. a Codex error JSON without spaces).
     Each codepoint is 4 bytes; the no-separator fallback used to slice
     mid-codepoint. Validate every chunk is well-formed UTF-8 by walking
     the bytes — a continuation byte (0x80–0xBF) at chunk start signals
     a mid-codepoint split. *)
  let emoji = "\xF0\x9F\x98\x80" in  (* 😀 U+1F600 *)
  let input = String.concat "" (List.init 1000 (fun _ -> emoji)) in
  let chunks = Discord_agents.Agent_process.split_message input in
  let well_formed s =
    let n = String.length s in
    let rec walk i =
      if i >= n then true
      else
        let c = Char.code s.[i] in
        let need =
          if c < 0x80 then 0
          else if c < 0xC0 then -1   (* mid-codepoint at boundary — bad *)
          else if c < 0xE0 then 1
          else if c < 0xF0 then 2
          else 3
        in
        if need < 0 || i + need >= n then false
        else
          let ok = ref true in
          for k = 1 to need do
            let cc = Char.code s.[i + k] in
            if cc < 0x80 || cc >= 0xC0 then ok := false
          done;
          !ok && walk (i + 1 + need)
    in
    walk 0
  in
  List.iter (fun chunk ->
    Alcotest.(check bool) "chunk is well-formed UTF-8" true (well_formed chunk)
  ) chunks

let split_message_tests = [
  Alcotest.test_case "short message" `Quick test_split_short;
  Alcotest.test_case "split at paragraph" `Quick test_split_long_at_paragraph;
  Alcotest.test_case "preserves code blocks" `Quick
    test_split_preserves_code_blocks;
  Alcotest.test_case "all chunks under limit" `Quick
    test_split_all_chunks_under_limit;
  Alcotest.test_case "no-separator fallback is UTF-8 safe" `Quick
    test_split_no_separator_utf8_safe;
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

let test_wrap_preserves_inline_code () =
  let input = "Use `git commit --amend --no-edit` to fix the last commit" in
  let result = Discord_agents.Agent_process.wrap_line ~max_width:25 input in
  (* The backtick span should not be split across lines *)
  List.iter (fun line ->
    let backticks = ref 0 in
    String.iter (fun c -> if c = '`' then incr backticks) line;
    Alcotest.(check bool)
      (Printf.sprintf "balanced backticks in: %s" line)
      true (!backticks mod 2 = 0)
  ) result

let wrap_line_tests = [
  Alcotest.test_case "short line" `Quick test_wrap_short_line;
  Alcotest.test_case "long line" `Quick test_wrap_long_line;
  Alcotest.test_case "single long word" `Quick test_wrap_single_long_word;
  Alcotest.test_case "preserves inline code" `Quick
    test_wrap_preserves_inline_code;
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
      | List_codex_sessions -> "List_codex_sessions"
      | List_gemini_sessions -> "List_gemini_sessions"
      | Start_agent { project; _ } -> "Start_agent(" ^ project ^ ")"
      | Resume_session { session_id; kind = None } ->
        "Resume_session(" ^ session_id ^ ")"
      | Resume_session { session_id; kind = Some k } ->
        Printf.sprintf "Resume_session(%s,%s)"
          (Discord_agents.Config.string_of_agent_kind k) session_id
      | Stop_session { thread_id } -> "Stop_session(" ^ thread_id ^ ")"
      | Cleanup_channels -> "Cleanup_channels"
      | Restart -> "Restart"
      | Refresh -> "Refresh"
      | Rename_thread _ -> "Rename_thread"
      | Status -> "Status"
      | Desktop -> "Desktop"
      | Mobile -> "Mobile"
      | Wrapping None -> "Wrapping(None)"
      | Wrapping (Some n) -> Printf.sprintf "Wrapping(Some %d)" n
      | Lines None -> "Lines(None)"
      | Lines (Some n) -> Printf.sprintf "Lines(Some %d)" n
      | Scroll None -> "Scroll(None)"
      | Scroll (Some n) -> Printf.sprintf "Scroll(Some %d)" n
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

let test_parse_lines_no_arg () =
  Alcotest.(check cmd_testable) "lines without arg"
    (Discord_agents.Command.Lines None)
    (Discord_agents.Command.parse "!lines")

let test_parse_lines_with_arg () =
  Alcotest.(check cmd_testable) "lines with arg"
    (Discord_agents.Command.Lines (Some 60))
    (Discord_agents.Command.parse "!lines 60")

let test_parse_lines_invalid () =
  let result = Discord_agents.Command.parse "!lines abc" in
  match result with
  | Discord_agents.Command.Unknown _ ->
    Alcotest.(check pass) "invalid lines is Unknown" () ()
  | _ -> Alcotest.fail "expected Unknown for invalid lines arg"

let test_parse_lines_large () =
  (* Parser accepts any positive int; the handler enforces the upper
     bound so the user gets a friendly message instead of silent rejection. *)
  Alcotest.(check cmd_testable) "huge lines value parses"
    (Discord_agents.Command.Lines (Some 100000))
    (Discord_agents.Command.parse "!lines 100000")

let test_parse_scroll_no_arg () =
  Alcotest.(check cmd_testable) "scroll without arg"
    (Discord_agents.Command.Scroll None)
    (Discord_agents.Command.parse "!scroll")

let test_parse_scroll_forward () =
  Alcotest.(check cmd_testable) "scroll forward"
    (Discord_agents.Command.Scroll (Some 3))
    (Discord_agents.Command.parse "!scroll 3")

let test_parse_scroll_backward () =
  Alcotest.(check cmd_testable) "scroll backward"
    (Discord_agents.Command.Scroll (Some (-2)))
    (Discord_agents.Command.parse "!scroll -2")

let test_parse_scroll_zero () =
  let result = Discord_agents.Command.parse "!scroll 0" in
  match result with
  | Discord_agents.Command.Unknown _ ->
    Alcotest.(check pass) "zero scroll is Unknown" () ()
  | _ -> Alcotest.fail "expected Unknown for zero scroll"

let test_parse_resume_no_kind () =
  Alcotest.(check cmd_testable) "resume without kind"
    (Discord_agents.Command.Resume_session
       { session_id = "abc12345"; kind = None })
    (Discord_agents.Command.parse "!resume abc12345")

let test_parse_resume_with_gemini_kind () =
  Alcotest.(check cmd_testable) "resume with gemini kind"
    (Discord_agents.Command.Resume_session
       { session_id = "xyz"; kind = Some Discord_agents.Config.Gemini })
    (Discord_agents.Command.parse "!resume gemini xyz")

let test_parse_resume_with_claude_kind () =
  Alcotest.(check cmd_testable) "resume with claude kind"
    (Discord_agents.Command.Resume_session
       { session_id = "xyz"; kind = Some Discord_agents.Config.Claude })
    (Discord_agents.Command.parse "!resume claude xyz")

let test_parse_resume_invalid_kind () =
  match Discord_agents.Command.parse "!resume nonesuch xyz" with
  | Discord_agents.Command.Unknown _ ->
    Alcotest.(check pass) "invalid kind is Unknown" () ()
  | _ -> Alcotest.fail "expected Unknown for invalid agent kind"

let test_parse_codex_sessions () =
  Alcotest.(check cmd_testable) "codex-sessions"
    Discord_agents.Command.List_codex_sessions
    (Discord_agents.Command.parse "!codex-sessions")

let test_parse_resume_with_codex_kind () =
  Alcotest.(check cmd_testable) "resume with codex kind"
    (Discord_agents.Command.Resume_session
       { session_id = "abc"; kind = Some Discord_agents.Config.Codex })
    (Discord_agents.Command.parse "!resume codex abc")

let test_parse_gemini_sessions () =
  Alcotest.(check cmd_testable) "gemini-sessions"
    Discord_agents.Command.List_gemini_sessions
    (Discord_agents.Command.parse "!gemini-sessions")

let test_parse_start_gemini () =
  Alcotest.(check cmd_testable) "start project gemini"
    (Discord_agents.Command.Start_agent
       { project = "myproj"; kind = Discord_agents.Config.Gemini })
    (Discord_agents.Command.parse "!start myproj gemini")

let command_tests = [
  Alcotest.test_case "desktop" `Quick test_parse_desktop;
  Alcotest.test_case "mobile" `Quick test_parse_mobile;
  Alcotest.test_case "wrapping no arg" `Quick test_parse_wrapping_no_arg;
  Alcotest.test_case "wrapping with arg" `Quick test_parse_wrapping_with_arg;
  Alcotest.test_case "wrapping invalid" `Quick test_parse_wrapping_invalid;
  Alcotest.test_case "wrapping zero" `Quick test_parse_wrapping_zero;
  Alcotest.test_case "wrapping negative" `Quick test_parse_wrapping_negative;
  Alcotest.test_case "lines no arg" `Quick test_parse_lines_no_arg;
  Alcotest.test_case "lines with arg" `Quick test_parse_lines_with_arg;
  Alcotest.test_case "lines invalid" `Quick test_parse_lines_invalid;
  Alcotest.test_case "lines large parses" `Quick test_parse_lines_large;
  Alcotest.test_case "scroll no arg" `Quick test_parse_scroll_no_arg;
  Alcotest.test_case "scroll forward" `Quick test_parse_scroll_forward;
  Alcotest.test_case "scroll backward" `Quick test_parse_scroll_backward;
  Alcotest.test_case "scroll zero" `Quick test_parse_scroll_zero;
  Alcotest.test_case "resume without kind" `Quick test_parse_resume_no_kind;
  Alcotest.test_case "resume with gemini kind" `Quick test_parse_resume_with_gemini_kind;
  Alcotest.test_case "resume with claude kind" `Quick test_parse_resume_with_claude_kind;
  Alcotest.test_case "resume invalid kind" `Quick test_parse_resume_invalid_kind;
  Alcotest.test_case "codex-sessions" `Quick test_parse_codex_sessions;
  Alcotest.test_case "gemini-sessions" `Quick test_parse_gemini_sessions;
  Alcotest.test_case "resume with codex kind" `Quick test_parse_resume_with_codex_kind;
  Alcotest.test_case "start with gemini kind" `Quick test_parse_start_gemini;
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

let test_detail_no_truncation () =
  (* 1500 chars is under the 1600-char cap — should be preserved in full *)
  let long_cmd = String.make 1500 'x' in
  let input = `Assoc [("command", `String long_cmd)] in
  let result = detail "Bash" input in
  Alcotest.(check bool) "contains full command"
    true (try ignore (Str.search_forward (Str.regexp_string long_cmd) result 0);
              true with Not_found -> false);
  Alcotest.(check bool) "total includes full content"
    true (String.length result > 1500)

let test_detail_safety_cap () =
  (* Content exceeding max_detail_len (1600) should be truncated *)
  let huge_cmd = String.make 3000 'y' in
  let input = `Assoc [("command", `String huge_cmd)] in
  let result = detail "Bash" input in
  Alcotest.(check bool) "truncated marker present"
    true (try ignore (Str.search_forward (Str.regexp_string "... (truncated)") result 0);
              true with Not_found -> false);
  (* Result should be capped near max_detail_len, not the full 3000 *)
  Alcotest.(check bool) "result under 2000 chars"
    true (String.length result < 2000)

let test_detail_fence_heavy () =
  (* Fence-heavy content (many ```) must not overflow Discord's 2000-char
     limit after escape_code_fences expands each ``` to 6 bytes. *)
  let fence_bomb = String.concat "" (List.init 500 (fun _ -> "```")) in
  let input = `Assoc [("command", `String fence_bomb)] in
  let result = Discord_agents.Agent_process.detail_of_tool_input "Bash" input in
  Alcotest.(check bool) "fence-heavy payload stays under Discord limit"
    true (String.length result < 2000)

let test_truncate_for_display_fence_heavy () =
  (* truncate_for_display must also account for fence expansion. *)
  let fence_line = String.concat "" (List.init 200 (fun _ -> "```")) in
  let lines = [fence_line; fence_line; fence_line] in
  let t = Discord_agents.Agent_process.truncate_for_display
    ~max_lines:10 ~max_chars:1700 lines in
  let text = String.concat "\n" t.display in
  let escaped = Discord_agents.Agent_process.escape_code_fences text in
  Alcotest.(check bool) "escaped text fits in budget"
    true (String.length escaped <= 1700)

let test_take_fitting_prefix_plain () =
  let s = String.make 5000 'a' in
  let taken = Discord_agents.Agent_process.take_fitting_prefix
    ~max_chars:1700 s in
  Alcotest.(check int) "takes exactly budget" 1700 taken

let test_take_fitting_prefix_fences () =
  (* 1700 ``` sequences = 5100 raw bytes, 10200 escaped bytes.
     A budget of 1700 should take 283 complete fences (283*6=1698)
     plus no partial (next ``` would be 6 more = 1704 > 1700). *)
  let s = String.concat "" (List.init 1700 (fun _ -> "```")) in
  let taken = Discord_agents.Agent_process.take_fitting_prefix
    ~max_chars:1700 s in
  (* Should be a multiple of 3 (complete triples) *)
  Alcotest.(check int) "lands on triple boundary" 0 (taken mod 3);
  (* And the escaped length of that prefix should be <= 1700 *)
  let prefix = String.sub s 0 taken in
  let elen = Discord_agents.Agent_process.escaped_length prefix in
  Alcotest.(check bool) "escaped length within budget" true (elen <= 1700)

let test_take_fitting_prefix_malformed_utf8 () =
  (* Lone 4-byte lead byte at end of buffer must not cause overrun.
     Tool output from subprocess stdout can contain arbitrary bytes,
     so this case reaches production. *)
  let s = "\xF0" in  (* 4-byte lead, but only 1 byte available *)
  let taken = Discord_agents.Agent_process.take_fitting_prefix
    ~max_chars:10 s in
  Alcotest.(check bool) "prefix bounded by buffer length"
    true (taken <= String.length s);
  (* Must not raise *)
  let _ = String.sub s 0 taken in
  ()

let test_take_fitting_prefix_utf8 () =
  (* Emoji (4-byte UTF-8) should never be split mid-codepoint. *)
  let emoji = "\xF0\x9F\x98\x80" in  (* 😀 *)
  let s = String.concat "" (List.init 500 (fun _ -> emoji)) in
  let taken = Discord_agents.Agent_process.take_fitting_prefix
    ~max_chars:100 s in
  Alcotest.(check int) "prefix lands on codepoint boundary" 0 (taken mod 4);
  Alcotest.(check bool) "prefix under budget" true (taken <= 100)

let test_truncate_for_display_single_pass () =
  (* A huge line count (10000) should not cause quadratic work.
     The single-pass algorithm must terminate quickly and return
     a shown count capped by max_lines. *)
  let lines = List.init 10000 (fun i -> Printf.sprintf "line %d" i) in
  let t = Discord_agents.Agent_process.truncate_for_display
    ~max_lines:40 ~max_chars:1700 lines in
  Alcotest.(check bool) "shown at most max_lines" true (t.shown <= 40);
  Alcotest.(check int) "total counted" 10000 t.total;
  Alcotest.(check bool) "marked truncated" true t.was_truncated

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
  Alcotest.test_case "no truncation" `Quick test_detail_no_truncation;
  Alcotest.test_case "safety cap" `Quick test_detail_safety_cap;
  Alcotest.test_case "fence heavy" `Quick test_detail_fence_heavy;
  Alcotest.test_case "truncate fence heavy" `Quick test_truncate_for_display_fence_heavy;
  Alcotest.test_case "take_fitting_prefix plain" `Quick test_take_fitting_prefix_plain;
  Alcotest.test_case "take_fitting_prefix fences" `Quick test_take_fitting_prefix_fences;
  Alcotest.test_case "take_fitting_prefix utf8" `Quick test_take_fitting_prefix_utf8;
  Alcotest.test_case "take_fitting_prefix malformed" `Quick test_take_fitting_prefix_malformed_utf8;
  Alcotest.test_case "truncate single pass" `Quick test_truncate_for_display_single_pass;
  Alcotest.test_case "lang_of_path" `Quick test_lang_of_path;
]

(* ── escape_nested_fences ──────────────────────────────────────── *)

let test_escape_nested_no_fences () =
  let input = "plain text\nno fences here" in
  Alcotest.(check string) "no fences unchanged"
    input (Discord_agents.Agent_process.escape_nested_fences input)

let test_escape_nested_normal_code_block () =
  let input = "text\n```ocaml\nlet x = 1\n```\nmore text" in
  Alcotest.(check string) "normal code block unchanged"
    input (Discord_agents.Agent_process.escape_nested_fences input)

let test_escape_nested_backticks_inside_code () =
  let input = "```\nPrintf.sprintf \"```\\n%s\\n```\"\n```" in
  let result = Discord_agents.Agent_process.escape_nested_fences input in
  (* The outer fences should be preserved *)
  Alcotest.(check bool) "starts with fence"
    true (String.length result >= 3 && String.sub result 0 3 = "```");
  (* The inner content should contain zero-width spaces (escaped fences) *)
  Alcotest.(check bool) "contains zero-width space"
    true (try ignore (Str.search_forward
            (Str.regexp_string "\xE2\x80\x8B") result 0); true
          with Not_found -> false);
  (* Result should be longer than input due to ZWS insertions *)
  Alcotest.(check bool) "result longer than input"
    true (String.length result > String.length input)

let escape_nested_fences_tests = [
  Alcotest.test_case "no fences" `Quick test_escape_nested_no_fences;
  Alcotest.test_case "normal code block" `Quick
    test_escape_nested_normal_code_block;
  Alcotest.test_case "backticks inside code" `Quick
    test_escape_nested_backticks_inside_code;
]

(* ── stream_event predicates (shared by every parser test group) ──── *)

(* Helpers to pattern-match on stream_event lists without depending on
   a pp. Each returns Some payload when the events list is exactly
   the expected single-event shape, None otherwise. *)
let expect_session_id events =
  match events with
  | [Discord_agents.Agent_process.Result { session_id = Some sid; _ }] ->
    Some sid
  | _ -> None

let expect_text events =
  match events with
  | [Discord_agents.Agent_process.Text_delta s] -> Some s
  | _ -> None

let expect_tool events =
  match events with
  | [Discord_agents.Agent_process.Tool_use info] -> Some info
  | _ -> None

let expect_tool_result events =
  match events with
  | [Discord_agents.Agent_process.Tool_result { content }] -> Some content
  | _ -> None

(* ── parse_codex_json_line ────────────────────────────────────────── *)

let parse_codex = Discord_agents.Agent_process.parse_codex_json_line

let test_codex_thread_started () =
  let line = {|{"type":"thread.started","thread_id":"abc-123"}|} in
  Alcotest.(check (option string)) "captures thread_id as session_id"
    (Some "abc-123") (expect_session_id (parse_codex line))

let test_codex_turn_started_dropped () =
  let line = {|{"type":"turn.started"}|} in
  Alcotest.(check int) "turn.started emits nothing"
    0 (List.length (parse_codex line))

let test_codex_turn_completed_flushes () =
  let line = {|{"type":"turn.completed","usage":{"input_tokens":1}}|} in
  match parse_codex line with
  | [Discord_agents.Agent_process.Result { session_id = None; _ }] -> ()
  | _ -> Alcotest.fail "expected single Result with no session_id"

let test_codex_agent_message () =
  let line = {|{"type":"item.completed","item":{"id":"i0","type":"agent_message","text":"hello world"}}|} in
  (* The trailing blank line keeps consecutive Codex agent_messages
     from running together when several land in a single turn. *)
  Alcotest.(check (option string)) "agent_message becomes Text_delta with separator"
    (Some "hello world\n\n") (expect_text (parse_codex line))

let test_codex_agent_message_started_dropped () =
  (* Codex emits agent_message only as item.completed in practice; an
     item.started should produce no event, not an Other-line log. *)
  let line = {|{"type":"item.started","item":{"id":"i0","type":"agent_message","text":""}}|} in
  Alcotest.(check int) "item.started agent_message emits nothing"
    0 (List.length (parse_codex line))

let test_codex_agent_message_already_terminated () =
  (* If Codex's text already ends in newlines, the separator helper
     normalizes the tail to exactly "\n\n" rather than appending and
     producing "\n\n\n+". *)
  let line = {|{"type":"item.completed","item":{"id":"i0","type":"agent_message","text":"hello\n\n\n"}}|} in
  Alcotest.(check (option string)) "trailing newlines collapsed to two"
    (Some "hello\n\n") (expect_text (parse_codex line))

let test_codex_agent_message_empty_dropped () =
  let line = {|{"type":"item.completed","item":{"id":"i0","type":"agent_message","text":""}}|} in
  Alcotest.(check int) "empty agent_message emits nothing"
    0 (List.length (parse_codex line))

let test_codex_command_started () =
  let line = {|{"type":"item.started","item":{"id":"i1","type":"command_execution","command":"ls -1","status":"in_progress"}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check string) "tool name" "Bash" info.tool_name;
    Alcotest.(check string) "summary" "ls -1" info.tool_summary;
    Alcotest.(check bool) "detail is bash code block"
      true (let d = info.tool_detail in
            String.length d >= 7 && String.sub d 0 7 = "```bash")
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_command_completed () =
  let line = {|{"type":"item.completed","item":{"id":"i1","type":"command_execution","command":"echo hi","aggregated_output":"hi\n","exit_code":0,"status":"completed"}}|} in
  Alcotest.(check (option string)) "completed emits Tool_result with output"
    (Some "hi\n") (expect_tool_result (parse_codex line))

let test_codex_command_completed_nonzero_exit () =
  let line = {|{"type":"item.completed","item":{"id":"i1","type":"command_execution","command":"false","aggregated_output":"","exit_code":1,"status":"completed"}}|} in
  Alcotest.(check (option string)) "nonzero exit prefixed with [exit N]"
    (Some "[exit 1]\n") (expect_tool_result (parse_codex line))

let test_codex_command_completed_empty_dropped () =
  let line = {|{"type":"item.completed","item":{"id":"i1","type":"command_execution","command":"true","aggregated_output":"","exit_code":0,"status":"completed"}}|} in
  Alcotest.(check int) "exit 0 with no output emits nothing"
    0 (List.length (parse_codex line))

let test_codex_file_change_add () =
  let line = {|{"type":"item.completed","item":{"id":"i2","type":"file_change","changes":[{"path":"/tmp/hello.txt","kind":"add"}],"status":"completed"}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check string) "add maps to Write" "Write" info.tool_name;
    Alcotest.(check bool) "summary mentions path"
      true (try ignore (Str.search_forward
              (Str.regexp_string "/tmp/hello.txt") info.tool_summary 0); true
            with Not_found -> false)
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_file_change_update () =
  let line = {|{"type":"item.completed","item":{"id":"i2","type":"file_change","changes":[{"path":"src/foo.ml","kind":"update"}]}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check string) "update maps to Edit" "Edit" info.tool_name
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_file_change_multi () =
  let line = {|{"type":"item.completed","item":{"id":"i2","type":"file_change","changes":[{"path":"a.ml","kind":"add"},{"path":"b.ml","kind":"update"}]}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check string) "multi-file maps to Edit" "Edit" info.tool_name;
    Alcotest.(check string) "summary reports count" "2 files" info.tool_summary
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_reasoning_dropped () =
  let line = {|{"type":"item.completed","item":{"id":"i3","type":"reasoning","text":"thinking..."}}|} in
  Alcotest.(check int) "reasoning emits nothing"
    0 (List.length (parse_codex line))

let test_codex_malformed_becomes_other () =
  let line = "not json at all" in
  match parse_codex line with
  | [Discord_agents.Agent_process.Other raw] ->
    Alcotest.(check string) "preserves raw line" line raw
  | _ -> Alcotest.fail "expected single Other event"

let test_codex_unknown_item_type () =
  let line = {|{"type":"item.completed","item":{"type":"novel_item_type"}}|} in
  match parse_codex line with
  | [Discord_agents.Agent_process.Other _] -> ()
  | _ -> Alcotest.fail "expected Other for unknown item type"

let test_codex_command_injection_summary () =
  (* Backticks in the command should not escape the inline code span
     when shown in a Discord status line. *)
  let line = {|{"type":"item.started","item":{"type":"command_execution","command":"echo `whoami`","status":"in_progress"}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check bool) "summary has no backticks"
      true (not (String.contains info.tool_summary '`'))
  | None -> Alcotest.fail "expected single Tool_use"

let codex_json_tests = [
  Alcotest.test_case "thread.started captures id" `Quick test_codex_thread_started;
  Alcotest.test_case "turn.started dropped" `Quick test_codex_turn_started_dropped;
  Alcotest.test_case "turn.completed flushes" `Quick test_codex_turn_completed_flushes;
  Alcotest.test_case "agent_message text" `Quick test_codex_agent_message;
  Alcotest.test_case "agent_message item.started dropped" `Quick test_codex_agent_message_started_dropped;
  Alcotest.test_case "agent_message trailing newlines normalized" `Quick test_codex_agent_message_already_terminated;
  Alcotest.test_case "empty agent_message dropped" `Quick test_codex_agent_message_empty_dropped;
  Alcotest.test_case "command start is Tool_use Bash" `Quick test_codex_command_started;
  Alcotest.test_case "command complete is Tool_result" `Quick test_codex_command_completed;
  Alcotest.test_case "nonzero exit prefixed" `Quick test_codex_command_completed_nonzero_exit;
  Alcotest.test_case "empty success dropped" `Quick test_codex_command_completed_empty_dropped;
  Alcotest.test_case "file_change add" `Quick test_codex_file_change_add;
  Alcotest.test_case "file_change update" `Quick test_codex_file_change_update;
  Alcotest.test_case "file_change multi" `Quick test_codex_file_change_multi;
  Alcotest.test_case "reasoning dropped" `Quick test_codex_reasoning_dropped;
  Alcotest.test_case "malformed becomes Other" `Quick test_codex_malformed_becomes_other;
  Alcotest.test_case "unknown item type" `Quick test_codex_unknown_item_type;
  Alcotest.test_case "backticks sanitized in summary" `Quick test_codex_command_injection_summary;
]

(* ── parse_codex_json_line: error frames + safety caps ────────────── *)

let expect_error events =
  match events with
  | [Discord_agents.Agent_process.Error msg] -> Some msg
  | _ -> None

let test_codex_top_level_error () =
  let line = {|{"type":"error","message":"model unavailable"}|} in
  Alcotest.(check (option string)) "top-level error surfaces"
    (Some "model unavailable") (expect_error (parse_codex line))

let test_codex_turn_failed () =
  let line = {|{"type":"turn.failed","error":{"message":"rate limited"}}|} in
  Alcotest.(check (option string)) "turn.failed surfaces nested message"
    (Some "rate limited") (expect_error (parse_codex line))

let test_codex_item_error () =
  let line = {|{"type":"item.completed","item":{"id":"i0","type":"error","message":"bad model metadata"}}|} in
  Alcotest.(check (option string)) "item-level error surfaces"
    (Some "bad model metadata") (expect_error (parse_codex line))

let test_codex_error_missing_message () =
  let line = {|{"type":"error"}|} in
  match expect_error (parse_codex line) with
  | Some _ -> ()
  | None -> Alcotest.fail "expected fallback error message"

let test_codex_turn_failed_scalar_error () =
  (* Codex sometimes inlines the error message as a string instead of
     wrapping it in an object. The previous parser raised Type_error
     here and the outer try silently swallowed the frame. *)
  let line = {|{"type":"turn.failed","error":"rate limited"}|} in
  Alcotest.(check (option string)) "scalar error string surfaces verbatim"
    (Some "rate limited") (expect_error (parse_codex line))

let test_codex_file_change_all_add_is_write () =
  (* Multi-file change where every entry is an add should display as
     Write (📝), not Edit (✏️) — matches single-file behavior. *)
  let line = {|{"type":"item.completed","item":{"type":"file_change","changes":[{"path":"a.ml","kind":"add"},{"path":"b.ml","kind":"add"}]}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check string) "all-add multi-file uses Write" "Write" info.tool_name
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_command_detail_oversized_capped () =
  (* Build a 10000-char command. The detail block must be capped well
     under Discord's 2000-char message limit. *)
  let huge = String.make 10_000 'a' in
  let line = Printf.sprintf
    {|{"type":"item.started","item":{"type":"command_execution","command":"%s","status":"in_progress"}}|}
    huge in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check bool) "detail under 2000 chars"
      true (String.length info.tool_detail < 2000);
    Alcotest.(check bool) "summary under 200 chars"
      true (String.length info.tool_summary < 200)
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_file_change_path_with_backticks () =
  (* A filename containing ``` must not break out of the detail code
     block. After escape_code_fences, no raw ``` should remain. *)
  let line = {|{"type":"item.completed","item":{"type":"file_change","changes":[{"path":"a.ml","kind":"add"},{"path":"weird```name.ml","kind":"update"}]}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    let detail = info.tool_detail in
    (* Outer fences only — count of triple-backtick runs should be 2. *)
    let count_triples s =
      let n = String.length s in
      let count = ref 0 in
      let i = ref 0 in
      while !i + 2 < n do
        if s.[!i] = '`' && s.[!i+1] = '`' && s.[!i+2] = '`' then begin
          incr count;
          i := !i + 3
        end else incr i
      done;
      !count
    in
    Alcotest.(check int) "exactly 2 fence markers (open + close)"
      2 (count_triples detail)
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_file_change_kind_default () =
  (* Missing kind should not produce an empty parens like "path ()". *)
  let line = {|{"type":"item.completed","item":{"type":"file_change","changes":[{"path":"foo.ml"}]}}|} in
  match expect_tool (parse_codex line) with
  | Some info ->
    Alcotest.(check bool) "summary not empty parens"
      false (try ignore (Str.search_forward
        (Str.regexp_string "()") info.tool_summary 0); true
      with Not_found -> false)
  | None -> Alcotest.fail "expected single Tool_use"

let test_codex_command_summary_utf8_safe () =
  (* 50 copies of a 4-byte emoji = 200 bytes. The 120-byte truncation
     must land on a codepoint boundary, not in the middle of the
     4-byte sequence. We check by walking the bytes: a UTF-8 lead
     byte must be followed by exactly the right number of continuation
     bytes; any premature end means truncation chopped a codepoint. *)
  let emoji = "\xF0\x9F\x98\x80" in  (* 😀 U+1F600 *)
  let cmd = String.concat "" (List.init 50 (fun _ -> emoji)) in
  let line = Printf.sprintf
    {|{"type":"item.started","item":{"type":"command_execution","command":"%s","status":"in_progress"}}|}
    cmd in
  match expect_tool (parse_codex line) with
  | Some info ->
    let s = info.tool_summary in
    let rec valid i =
      if i >= String.length s then true
      else
        let c = Char.code s.[i] in
        let need =
          if c < 0x80 then 0
          else if c < 0xC0 then -1   (* stray continuation *)
          else if c < 0xE0 then 1
          else if c < 0xF0 then 2
          else 3
        in
        if need < 0 then false
        else if i + need >= String.length s then false
        else
          let ok = ref true in
          for k = 1 to need do
            let cc = Char.code s.[i + k] in
            if cc < 0x80 || cc >= 0xC0 then ok := false
          done;
          !ok && valid (i + 1 + need)
    in
    Alcotest.(check bool) "summary is valid UTF-8" true (valid 0)
  | None -> Alcotest.fail "expected single Tool_use"

let codex_safety_tests = [
  Alcotest.test_case "top-level error surfaces" `Quick test_codex_top_level_error;
  Alcotest.test_case "turn.failed surfaces" `Quick test_codex_turn_failed;
  Alcotest.test_case "item.completed error surfaces" `Quick test_codex_item_error;
  Alcotest.test_case "error missing message has fallback" `Quick test_codex_error_missing_message;
  Alcotest.test_case "oversized command detail is capped" `Quick test_codex_command_detail_oversized_capped;
  Alcotest.test_case "file_change paths with backticks escaped" `Quick test_codex_file_change_path_with_backticks;
  Alcotest.test_case "missing file_change kind has default" `Quick test_codex_file_change_kind_default;
  Alcotest.test_case "command summary UTF-8 safe truncation" `Quick test_codex_command_summary_utf8_safe;
  Alcotest.test_case "turn.failed with scalar error string" `Quick test_codex_turn_failed_scalar_error;
  Alcotest.test_case "all-add multi-file uses Write tool" `Quick test_codex_file_change_all_add_is_write;
]

(* ── Session_store: legacy load defaults ─────────────────────────── *)

let test_legacy_codex_session_defaults_unconfirmed () =
  (* A session record persisted before [session_id_confirmed] existed
     must default to [false] for Codex sessions, so the next run
     starts fresh rather than attempting [codex exec resume <stale-uuid>]
     with the bot-generated placeholder. *)
  let json = Yojson.Safe.from_string {|[
    {"project_name":"foo","working_dir":"/tmp/foo",
     "agent_kind":"codex","session_id":"placeholder-uuid",
     "thread_id":"123","message_count":2}
  ]|} in
  let map = Discord_agents.Session_store.sessions_of_json json in
  let sessions = Discord_agents.Session_store.SessionMap.bindings map in
  match sessions with
  | [(_, s)] ->
    Alcotest.(check bool) "legacy Codex session defaults to unconfirmed"
      false s.session_id_confirmed
  | _ -> Alcotest.fail "expected exactly one session"

let test_legacy_claude_session_defaults_confirmed () =
  let json = Yojson.Safe.from_string {|[
    {"project_name":"foo","working_dir":"/tmp/foo",
     "agent_kind":"claude","session_id":"some-uuid",
     "thread_id":"123","message_count":2}
  ]|} in
  let map = Discord_agents.Session_store.sessions_of_json json in
  let sessions = Discord_agents.Session_store.SessionMap.bindings map in
  match sessions with
  | [(_, s)] ->
    Alcotest.(check bool) "legacy Claude session defaults to confirmed"
      true s.session_id_confirmed
  | _ -> Alcotest.fail "expected exactly one session"

let test_legacy_gemini_session_defaults_unconfirmed () =
  (* Same rationale as Codex — Gemini's session ids are server-allocated
     so a missing field on a legacy record must default to false to
     prevent a stale-uuid resume on first use after upgrade. *)
  let json = Yojson.Safe.from_string {|[
    {"project_name":"foo","working_dir":"/tmp/foo",
     "agent_kind":"gemini","session_id":"placeholder-uuid",
     "thread_id":"123","message_count":2}
  ]|} in
  let map = Discord_agents.Session_store.sessions_of_json json in
  let sessions = Discord_agents.Session_store.SessionMap.bindings map in
  match sessions with
  | [(_, s)] ->
    Alcotest.(check bool) "legacy Gemini session defaults to unconfirmed"
      false s.session_id_confirmed
  | _ -> Alcotest.fail "expected exactly one session"

(* Regression: the Resume_session handlers in bot.ml and control_api.ml
   construct a session record with [session_id_confirmed:true] because
   they've just resolved [session_id] from disk discovery and know
   it's resumable. Without that override, make_session would default
   Gemini sessions to unconfirmed (caller_pinned_session_id Gemini =
   false), and the next turn would emit gemini_args without --resume
   and start a fresh chat. This test pins the override. *)
let test_make_session_resume_override_gemini_confirms () =
  let s = Discord_agents.Session_store.make_session
    ~project_name:"foo" ~working_dir:"/tmp/foo"
    ~agent_kind:Discord_agents.Config.Gemini
    ~session_id:"recovered-from-disk"
    ~session_id_confirmed:true ~message_count:1
    ~thread_id:"123" ~system_prompt:None ~initial_prompt:None ()
  in
  Alcotest.(check bool)
    "resume override flips Gemini's session_id_confirmed to true"
    true s.session_id_confirmed

let test_make_session_default_gemini_unconfirmed () =
  let s = Discord_agents.Session_store.make_session
    ~project_name:"foo" ~working_dir:"/tmp/foo"
    ~agent_kind:Discord_agents.Config.Gemini
    ~session_id:"placeholder"
    ~thread_id:"123" ~system_prompt:None ~initial_prompt:None ()
  in
  Alcotest.(check bool)
    "fresh Gemini default is unconfirmed (id is placeholder until init)"
    false s.session_id_confirmed

let test_make_session_default_claude_confirmed () =
  let s = Discord_agents.Session_store.make_session
    ~project_name:"foo" ~working_dir:"/tmp/foo"
    ~agent_kind:Discord_agents.Config.Claude
    ~session_id:"caller-pinned"
    ~thread_id:"123" ~system_prompt:None ~initial_prompt:None ()
  in
  Alcotest.(check bool)
    "Claude default is confirmed (--session-id pins it)"
    true s.session_id_confirmed

let session_store_tests = [
  Alcotest.test_case "legacy Codex session unconfirmed" `Quick
    test_legacy_codex_session_defaults_unconfirmed;
  Alcotest.test_case "legacy Claude session confirmed" `Quick
    test_legacy_claude_session_defaults_confirmed;
  Alcotest.test_case "legacy Gemini session unconfirmed" `Quick
    test_legacy_gemini_session_defaults_unconfirmed;
  Alcotest.test_case "Resume override flips Gemini to confirmed" `Quick
    test_make_session_resume_override_gemini_confirms;
  Alcotest.test_case "fresh Gemini default unconfirmed" `Quick
    test_make_session_default_gemini_unconfirmed;
  Alcotest.test_case "fresh Claude default confirmed" `Quick
    test_make_session_default_claude_confirmed;
]

(* ── Bot.resume_not_found_message + Bot.merge_gemini_settings ───── *)

(* All three agent stores are enumerable now, so resume_not_found
   has no special cases — every kind gets the same format. *)
let test_resume_not_found_names_kind_and_sid kind expected_word =
  let msg = Discord_agents.Bot.resume_not_found_message
    ~kind:(Some kind) ~sid_prefix:"xyz" in
  let contains s =
    try ignore (Str.search_forward (Str.regexp_string s) msg 0); true
    with Not_found -> false
  in
  Alcotest.(check bool)
    (Printf.sprintf "error names %s and the sid" expected_word)
    true (contains expected_word && contains "xyz")

let test_resume_not_found_codex_uniform () =
  test_resume_not_found_names_kind_and_sid Discord_agents.Config.Codex "codex"

let test_resume_not_found_gemini_includes_sid () =
  test_resume_not_found_names_kind_and_sid Discord_agents.Config.Gemini "gemini"

let test_merge_gemini_settings_preserves_other_servers () =
  let existing = Some {|{
    "mcpServers": { "user-tool": { "command": "foo", "args": [] } },
    "theme": "dark"
  }|} in
  let merged = Discord_agents.Agent_process.merge_gemini_settings existing in
  let json = Yojson.Safe.from_string merged in
  let open Yojson.Safe.Util in
  let servers = json |> member "mcpServers" |> to_assoc in
  Alcotest.(check bool) "user-tool preserved"
    true (List.mem_assoc "user-tool" servers);
  Alcotest.(check bool) "discord-agents added"
    true (List.mem_assoc "discord-agents" servers);
  Alcotest.(check bool) "unrelated top-level keys preserved (theme)"
    true (List.mem_assoc "theme" (json |> to_assoc))

let test_merge_gemini_settings_overwrites_our_entry () =
  let existing = Some {|{
    "mcpServers": { "discord-agents": { "command": "stale", "args": [] } }
  }|} in
  let merged = Discord_agents.Agent_process.merge_gemini_settings existing in
  let json = Yojson.Safe.from_string merged in
  let open Yojson.Safe.Util in
  let our_cmd = json |> member "mcpServers" |> member "discord-agents"
                |> member "command" |> to_string in
  Alcotest.(check string) "stale entry replaced with current python3"
    "python3" our_cmd

let test_merge_gemini_settings_creates_when_absent () =
  let merged = Discord_agents.Agent_process.merge_gemini_settings None in
  let json = Yojson.Safe.from_string merged in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "creates mcpServers with our entry"
    true (List.mem_assoc "discord-agents"
            (json |> member "mcpServers" |> to_assoc))

let test_merge_gemini_settings_invalid_input_falls_back () =
  let merged = Discord_agents.Agent_process.merge_gemini_settings
    (Some "not valid json {") in
  let json = Yojson.Safe.from_string merged in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "invalid input falls back to fresh config"
    true (List.mem_assoc "discord-agents"
            (json |> member "mcpServers" |> to_assoc))

let test_merge_gemini_settings_non_object_falls_back () =
  (* Parseable JSON that isn't an object — list, scalar — would
     round-trip unchanged under naive logic, dropping our MCP entry. *)
  List.iter (fun text ->
    let merged = Discord_agents.Agent_process.merge_gemini_settings
      (Some text) in
    let json = Yojson.Safe.from_string merged in
    let open Yojson.Safe.Util in
    Alcotest.(check bool)
      (Printf.sprintf "non-object %S falls back to fresh config" text)
      true (List.mem_assoc "discord-agents"
              (json |> member "mcpServers" |> to_assoc))
  ) ["[]"; {|"a string"|}; "42"; "null"; "true"]

let resume_helpers_tests = [
  Alcotest.test_case "Codex resume error names kind+sid (uniform)" `Quick
    test_resume_not_found_codex_uniform;
  Alcotest.test_case "Gemini resume error names kind+sid" `Quick
    test_resume_not_found_gemini_includes_sid;
  Alcotest.test_case "merge_gemini_settings preserves other servers" `Quick
    test_merge_gemini_settings_preserves_other_servers;
  Alcotest.test_case "merge_gemini_settings overwrites our stale entry" `Quick
    test_merge_gemini_settings_overwrites_our_entry;
  Alcotest.test_case "merge_gemini_settings creates when absent" `Quick
    test_merge_gemini_settings_creates_when_absent;
  Alcotest.test_case "merge_gemini_settings invalid input falls back" `Quick
    test_merge_gemini_settings_invalid_input_falls_back;
  Alcotest.test_case "merge_gemini_settings non-object falls back" `Quick
    test_merge_gemini_settings_non_object_falls_back;
]

(* ── codex_args ────────────────────────────────────────────────────── *)

let codex_args = Discord_agents.Agent_process.codex_args

(* The args list now contains MCP TOML overrides, so exact-list
   matching would be brittle. Tests assert on structural shape: the
   right subcommand, the right flags present, the MCP server
   registered, and the prompt at the end after a -- separator. *)

let contains_pair list k v =
  let rec scan = function
    | a :: b :: _ when a = k && b = v -> true
    | _ :: rest -> scan rest
    | [] -> false
  in scan list

let last_two list =
  match List.rev list with
  | last :: penultimate :: _ -> Some (penultimate, last)
  | _ -> None

let test_codex_args_fresh () =
  let args = codex_args ~session_id:"placeholder-uuid"
    ~session_id_confirmed:false ~prompt:"hello" in
  Alcotest.(check bool) "starts with `codex exec`" true
    (List.length args >= 2 && List.nth args 0 = "codex"
     && List.nth args 1 = "exec");
  Alcotest.(check bool) "includes --json" true (List.mem "--json" args);
  Alcotest.(check bool) "includes --full-auto" true
    (List.mem "--full-auto" args);
  Alcotest.(check bool) "includes --skip-git-repo-check" true
    (List.mem "--skip-git-repo-check" args);
  Alcotest.(check bool) "no resume on fresh" false (List.mem "resume" args);
  Alcotest.(check (option (pair string string)))
    "ends with -- then prompt" (Some ("--", "hello")) (last_two args)

let test_codex_args_resume () =
  let sid = "019dc073-0e5f-74d3-8fcf-0bb027feab47" in
  let args = codex_args ~session_id:sid
    ~session_id_confirmed:true ~prompt:"continue" in
  Alcotest.(check bool) "includes resume <sid>" true
    (contains_pair args "resume" sid);
  Alcotest.(check (option (pair string string)))
    "ends with -- then prompt" (Some ("--", "continue")) (last_two args)

let test_codex_args_dash_prompt_safe () =
  (* A prompt starting with - must not be consumed as a Codex flag. *)
  let args = codex_args ~session_id:"x" ~session_id_confirmed:false
    ~prompt:"--help me" in
  Alcotest.(check (option (pair string string)))
    "prompt sits after --" (Some ("--", "--help me")) (last_two args)

let test_codex_args_includes_mcp_overrides () =
  (* Codex sessions should expose the bot's MCP tools the same way
     Claude (--mcp-config) and Gemini (.gemini/settings.json) do.
     Two -c overrides register the discord-agents server. *)
  let args = codex_args ~session_id:"x" ~session_id_confirmed:false
    ~prompt:"hi" in
  Alcotest.(check bool) "registers discord_agents.command"
    true (contains_pair args "-c"
            {|mcp_servers.discord_agents.command="python3"|});
  Alcotest.(check bool) "registers discord_agents.args" true
    (List.exists (fun s ->
      try ignore (Str.search_forward
        (Str.regexp_string "mcp_servers.discord_agents.args=[") s 0); true
      with Not_found -> false) args)

let test_codex_args_resume_keeps_mcp () =
  (* Resuming a Codex session should still expose MCP tools. *)
  let args = codex_args ~session_id:"x" ~session_id_confirmed:true
    ~prompt:"hi" in
  Alcotest.(check bool) "MCP override present on resume too"
    true (contains_pair args "-c"
            {|mcp_servers.discord_agents.command="python3"|})

let codex_args_tests = [
  Alcotest.test_case "fresh exec shape" `Quick test_codex_args_fresh;
  Alcotest.test_case "resume shape" `Quick test_codex_args_resume;
  Alcotest.test_case "dash-prefixed prompt sits after --" `Quick
    test_codex_args_dash_prompt_safe;
  Alcotest.test_case "fresh includes MCP overrides" `Quick
    test_codex_args_includes_mcp_overrides;
  Alcotest.test_case "resume keeps MCP overrides" `Quick
    test_codex_args_resume_keeps_mcp;
]

(* ── escape_toml_string + compose_session_prompt ──────────────────── *)

let test_escape_toml_string_passthrough () =
  let s = "/home/me/bot/scripts/mcp-server.py" in
  Alcotest.(check string) "ordinary path passes through unchanged"
    s (Discord_agents.Agent_process.escape_toml_string s)

let test_escape_toml_string_quotes () =
  let s = {|path with "quotes"|} in
  Alcotest.(check string) "double-quote escaped to \\\""
    {|path with \"quotes\"|}
    (Discord_agents.Agent_process.escape_toml_string s)

let test_escape_toml_string_backslash () =
  let s = {|c:\Users\bot|} in
  Alcotest.(check string) "backslash escaped to \\\\"
    {|c:\\Users\\bot|}
    (Discord_agents.Agent_process.escape_toml_string s)

let test_escape_toml_string_control_chars () =
  (* Control bytes never appear in real filesystem paths, but the
     spec requires them to be escaped — verify we don't pass them
     through raw, which would produce invalid TOML. *)
  let s = "a\x01b\nc\td" in
  let escaped = Discord_agents.Agent_process.escape_toml_string s in
  Alcotest.(check bool) "no raw control bytes survive"
    true (String.for_all (fun c ->
      let code = Char.code c in
      code >= 0x20 && code <> 0x7F || c = '\\' || c = '"') escaped);
  Alcotest.(check bool) "literal newline becomes \\n"
    true (try ignore (Str.search_forward
      (Str.regexp_string "\\n") escaped 0); true
    with Not_found -> false)

let compose = Discord_agents.Agent_process.compose_session_prompt

let test_compose_claude_no_prepend () =
  (* Claude has --append-system-prompt, so the user prompt is
     unchanged regardless of whether system_prompt is set. *)
  let out = compose ~agent_kind:Discord_agents.Config.Claude
    ~system_prompt:(Some "INSTR") ~message_count:0 ~user_prompt:"hi" in
  Alcotest.(check string) "claude prompt unchanged" "hi" out

let test_compose_codex_first_turn_prepends () =
  let out = compose ~agent_kind:Discord_agents.Config.Codex
    ~system_prompt:(Some "INSTR") ~message_count:0 ~user_prompt:"hi" in
  Alcotest.(check bool) "first-turn Codex receives <bot-context>"
    true (try ignore (Str.search_forward
      (Str.regexp_string "<bot-context>") out 0); true
    with Not_found -> false);
  Alcotest.(check bool) "system prompt embedded" true
    (try ignore (Str.search_forward (Str.regexp_string "INSTR") out 0); true
    with Not_found -> false);
  Alcotest.(check bool) "user prompt still present" true
    (try ignore (Str.search_forward (Str.regexp_string "hi") out 0); true
    with Not_found -> false)

let test_compose_codex_subsequent_turn_no_prepend () =
  let out = compose ~agent_kind:Discord_agents.Config.Codex
    ~system_prompt:(Some "INSTR") ~message_count:1 ~user_prompt:"hi" in
  Alcotest.(check string) "subsequent turn unchanged" "hi" out

let test_compose_gemini_first_turn_prepends () =
  let out = compose ~agent_kind:Discord_agents.Config.Gemini
    ~system_prompt:(Some "INSTR") ~message_count:0 ~user_prompt:"hi" in
  Alcotest.(check bool) "first-turn Gemini also gets <bot-context>"
    true (try ignore (Str.search_forward
      (Str.regexp_string "<bot-context>") out 0); true
    with Not_found -> false)

let test_compose_no_system_prompt_passthrough () =
  let out = compose ~agent_kind:Discord_agents.Config.Codex
    ~system_prompt:None ~message_count:0 ~user_prompt:"hi" in
  Alcotest.(check string) "no system prompt = unchanged" "hi" out

let prompt_helpers_tests = [
  Alcotest.test_case "escape_toml_string passthrough" `Quick
    test_escape_toml_string_passthrough;
  Alcotest.test_case "escape_toml_string quotes" `Quick
    test_escape_toml_string_quotes;
  Alcotest.test_case "escape_toml_string backslash" `Quick
    test_escape_toml_string_backslash;
  Alcotest.test_case "escape_toml_string control chars" `Quick
    test_escape_toml_string_control_chars;
  Alcotest.test_case "compose: Claude unchanged (uses --append flag)" `Quick
    test_compose_claude_no_prepend;
  Alcotest.test_case "compose: Codex first turn prepends bot-context" `Quick
    test_compose_codex_first_turn_prepends;
  Alcotest.test_case "compose: Codex subsequent turn unchanged" `Quick
    test_compose_codex_subsequent_turn_no_prepend;
  Alcotest.test_case "compose: Gemini first turn prepends bot-context" `Quick
    test_compose_gemini_first_turn_prepends;
  Alcotest.test_case "compose: no system prompt = passthrough" `Quick
    test_compose_no_system_prompt_passthrough;
]

(* ── parse_gemini_stream_json_line ────────────────────────────────── *)

let parse_gemini = Discord_agents.Agent_process.parse_gemini_stream_json_line

let test_gemini_init_captures_session_id () =
  let line = {|{"type":"init","timestamp":"t","session_id":"s-42","model":"m"}|} in
  Alcotest.(check (option string)) "init session_id"
    (Some "s-42") (expect_session_id (parse_gemini line))

let test_gemini_init_without_session_dropped () =
  let line = {|{"type":"init","model":"m"}|} in
  Alcotest.(check int) "init with no session_id emits nothing"
    0 (List.length (parse_gemini line))

let test_gemini_user_message_skipped () =
  let line = {|{"type":"message","role":"user","content":"hello"}|} in
  Alcotest.(check int) "user echo not forwarded"
    0 (List.length (parse_gemini line))

let test_gemini_assistant_delta () =
  let line = {|{"type":"message","role":"assistant","content":"Hello","delta":true}|} in
  Alcotest.(check (option string)) "assistant content becomes Text_delta"
    (Some "Hello") (expect_text (parse_gemini line))

let test_gemini_assistant_empty_dropped () =
  let line = {|{"type":"message","role":"assistant","content":"","delta":true}|} in
  Alcotest.(check int) "empty assistant content emits nothing"
    0 (List.length (parse_gemini line))

let test_gemini_shell_tool_use () =
  let line = {|{"type":"tool_use","tool_name":"run_shell_command","tool_id":"t1","parameters":{"command":"ls /tmp"}}|} in
  match expect_tool (parse_gemini line) with
  | Some info ->
    Alcotest.(check string) "maps to Bash" "Bash" info.tool_name;
    Alcotest.(check string) "summary is the command" "ls /tmp" info.tool_summary;
    Alcotest.(check bool) "detail is bash code block"
      true (let d = info.tool_detail in
            String.length d >= 7 && String.sub d 0 7 = "```bash")
  | None -> Alcotest.fail "expected single Tool_use"

let test_gemini_read_file_tool_use () =
  let line = {|{"type":"tool_use","tool_name":"read_file","parameters":{"file_path":"/etc/hostname"}}|} in
  match expect_tool (parse_gemini line) with
  | Some info ->
    Alcotest.(check string) "maps to Read" "Read" info.tool_name;
    Alcotest.(check bool) "summary mentions path"
      true (try ignore (Str.search_forward
              (Str.regexp_string "/etc/hostname") info.tool_summary 0); true
            with Not_found -> false)
  | None -> Alcotest.fail "expected single Tool_use"

let test_gemini_write_file_tool_use () =
  let line = {|{"type":"tool_use","tool_name":"write_file","parameters":{"file_path":"/tmp/a.ml","content":"let () = ()"}}|} in
  match expect_tool (parse_gemini line) with
  | Some info ->
    Alcotest.(check string) "maps to Write" "Write" info.tool_name;
    Alcotest.(check bool) "detail uses ocaml lang from path"
      true (let d = info.tool_detail in
            String.length d >= 8 && String.sub d 0 8 = "```ocaml")
  | None -> Alcotest.fail "expected single Tool_use"

let test_gemini_unknown_tool_passthrough () =
  let line = {|{"type":"tool_use","tool_name":"novel_tool","parameters":{"x":"y"}}|} in
  match expect_tool (parse_gemini line) with
  | Some info ->
    Alcotest.(check string) "unknown tool name preserved"
      "novel_tool" info.tool_name
  | None -> Alcotest.fail "expected single Tool_use"

let test_gemini_tool_result_success () =
  let line = {|{"type":"tool_result","tool_id":"t1","status":"success","output":"hello world"}|} in
  Alcotest.(check (option string)) "success carries output"
    (Some "hello world") (expect_tool_result (parse_gemini line))

let test_gemini_tool_result_error () =
  let line = {|{"type":"tool_result","tool_id":"t1","status":"error","output":"bad path","error":{"type":"invalid","message":"nope"}}|} in
  match expect_tool_result (parse_gemini line) with
  | Some content ->
    Alcotest.(check bool) "content marked as error"
      true (String.length content >= 7 && String.sub content 0 7 = "[error]");
    Alcotest.(check bool) "includes error message"
      true (try ignore (Str.search_forward
              (Str.regexp_string "nope") content 0); true
            with Not_found -> false)
  | None -> Alcotest.fail "expected single Tool_result"

let test_gemini_tool_result_empty_dropped () =
  let line = {|{"type":"tool_result","tool_id":"t1","status":"success","output":""}|} in
  Alcotest.(check int) "empty output emits nothing"
    0 (List.length (parse_gemini line))

let test_gemini_result_flushes () =
  let line = {|{"type":"result","status":"success","stats":{"total_tokens":1}}|} in
  match parse_gemini line with
  | [Discord_agents.Agent_process.Result { session_id = None; text = "" }] -> ()
  | _ -> Alcotest.fail "expected Result with empty text and no session_id"

let test_gemini_malformed_becomes_other () =
  let line = "YOLO mode is enabled." in
  match parse_gemini line with
  | [Discord_agents.Agent_process.Other raw] ->
    Alcotest.(check string) "preserves raw line" line raw
  | _ -> Alcotest.fail "expected Other event"

let test_gemini_unknown_type_becomes_other () =
  let line = {|{"type":"novel_event","foo":"bar"}|} in
  match parse_gemini line with
  | [Discord_agents.Agent_process.Other _] -> ()
  | _ -> Alcotest.fail "expected Other for unknown type"

let gemini_json_tests = [
  Alcotest.test_case "init captures session_id" `Quick test_gemini_init_captures_session_id;
  Alcotest.test_case "init missing session_id dropped" `Quick test_gemini_init_without_session_dropped;
  Alcotest.test_case "user echo skipped" `Quick test_gemini_user_message_skipped;
  Alcotest.test_case "assistant delta is Text_delta" `Quick test_gemini_assistant_delta;
  Alcotest.test_case "empty assistant dropped" `Quick test_gemini_assistant_empty_dropped;
  Alcotest.test_case "run_shell_command maps to Bash" `Quick test_gemini_shell_tool_use;
  Alcotest.test_case "read_file maps to Read" `Quick test_gemini_read_file_tool_use;
  Alcotest.test_case "write_file maps to Write" `Quick test_gemini_write_file_tool_use;
  Alcotest.test_case "unknown tool name passthrough" `Quick test_gemini_unknown_tool_passthrough;
  Alcotest.test_case "tool_result success carries output" `Quick test_gemini_tool_result_success;
  Alcotest.test_case "tool_result error formatted" `Quick test_gemini_tool_result_error;
  Alcotest.test_case "empty tool_result dropped" `Quick test_gemini_tool_result_empty_dropped;
  Alcotest.test_case "result flushes" `Quick test_gemini_result_flushes;
  Alcotest.test_case "malformed becomes Other" `Quick test_gemini_malformed_becomes_other;
  Alcotest.test_case "unknown type becomes Other" `Quick test_gemini_unknown_type_becomes_other;
]

(* ── gemini_args ──────────────────────────────────────────────────── *)

let gemini_args = Discord_agents.Agent_process.gemini_args

let test_gemini_args_fresh () =
  let args = gemini_args ~session_id:"placeholder"
    ~session_id_confirmed:false ~prompt:"hello" in
  Alcotest.(check (list string))
    "fresh invocation: -p prompt, -o stream-json, --yolo, no resume"
    ["gemini"; "-p"; "hello"; "-o"; "stream-json"; "--yolo"]
    args

let test_gemini_args_resume () =
  let args = gemini_args ~session_id:"abc-123"
    ~session_id_confirmed:true ~prompt:"continue" in
  Alcotest.(check (list string))
    "resume invocation appends --resume <id>"
    ["gemini"; "-p"; "continue"; "-o"; "stream-json"; "--yolo";
     "--resume"; "abc-123"]
    args

let gemini_args_tests = [
  Alcotest.test_case "fresh invocation args" `Quick test_gemini_args_fresh;
  Alcotest.test_case "resume invocation args" `Quick test_gemini_args_resume;
]

(* ── caller_pinned_session_id ─────────────────────────────────────── *)

let test_caller_pinned_claude () =
  Alcotest.(check bool) "Claude pins its own session id"
    true (Discord_agents.Config.caller_pinned_session_id Discord_agents.Config.Claude)

let test_caller_pinned_codex () =
  Alcotest.(check bool) "Codex allocates server-side"
    false (Discord_agents.Config.caller_pinned_session_id Discord_agents.Config.Codex)

let test_caller_pinned_gemini () =
  Alcotest.(check bool) "Gemini allocates server-side"
    false (Discord_agents.Config.caller_pinned_session_id Discord_agents.Config.Gemini)

let caller_pinned_tests = [
  Alcotest.test_case "Claude is caller-pinned" `Quick test_caller_pinned_claude;
  Alcotest.test_case "Codex is server-allocated" `Quick test_caller_pinned_codex;
  Alcotest.test_case "Gemini is server-allocated" `Quick test_caller_pinned_gemini;
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
    "escape_nested_fences", escape_nested_fences_tests;
    "codex_json", codex_json_tests;
    "codex_safety", codex_safety_tests;
    "codex_args", codex_args_tests;
    "prompt_helpers", prompt_helpers_tests;
    "gemini_json", gemini_json_tests;
    "gemini_args", gemini_args_tests;
    "caller_pinned", caller_pinned_tests;
    "session_store", session_store_tests;
    "resume_helpers", resume_helpers_tests;
  ]
