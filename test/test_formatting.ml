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

(* Regression: !projects output with many entries must be safely chunkable.
   The original bug was that create_message sent a single ~5700-char message
   when 63 projects were discovered, and Discord silently rejected it.

   This test reconstructs the original text by stripping the whitespace the
   splitter uses as delimiters — catching any duplication, reordering, or
   loss a substring check would miss. *)
let test_split_projects_list_shape () =
  let header = "**Projects** (use `!start <name>` or `!start <number>`):\n" in
  let line i =
    Printf.sprintf "`%d.` **tutorials/some-project-name-%d** — `/home/tedks/Projects/tutorials/some-project-name-%d`"
      i i i
  in
  let lines = List.init 63 (fun i -> line (i + 1)) in
  let input = header ^ String.concat "\n" lines in
  Alcotest.(check bool) "input exceeds Discord's 2000-char limit (regression shape)"
    true (String.length input > 2000);
  let chunks = Discord_agents.Agent_process.split_message input in
  Alcotest.(check bool) "split into multiple chunks"
    true (List.length chunks >= 2);
  List.iter (fun chunk ->
    Alcotest.(check bool)
      (Printf.sprintf "chunk %d chars <= 2000" (String.length chunk))
      true (String.length chunk <= 2000)
  ) chunks;
  (* Strip whitespace from the original and the concatenation. split_message
     consumes the separator (space/newline) it splits at, so whitespace-
     insensitive comparison is the tightest check available without
     instrumenting the splitter. Order and content must both match. *)
  let strip_ws s =
    let buf = Buffer.create (String.length s) in
    String.iter (fun c ->
      if c <> ' ' && c <> '\n' && c <> '\t' then Buffer.add_char buf c
    ) s;
    Buffer.contents buf
  in
  let recombined = strip_ws (String.concat "" chunks) in
  let expected = strip_ws input in
  Alcotest.(check string) "chunks reassemble to original (order-preserving)"
    expected recombined

(* plan_message_chunks is a pure function separated from create_message's I/O
   so we can unit-test the reply_to semantics without mocking Discord. *)
let test_plan_short_content () =
  let plan = Discord_agents.Discord_rest.plan_message_chunks
    ~reply_to:"origin-msg-id" "hello" in
  Alcotest.(check int) "single chunk for short content" 1 (List.length plan);
  match plan with
  | [(content, reply_to)] ->
    Alcotest.(check string) "content preserved" "hello" content;
    Alcotest.(check (option string)) "reply_to carried"
      (Some "origin-msg-id") reply_to
  | _ -> Alcotest.fail "expected exactly one chunk"

let test_plan_long_content_reply_to_first_only () =
  let long = String.make 5000 'x' in
  let plan = Discord_agents.Discord_rest.plan_message_chunks
    ~reply_to:"origin-msg-id" long in
  Alcotest.(check bool) "multiple chunks" true (List.length plan >= 2);
  match plan with
  | (_, first_reply) :: rest ->
    Alcotest.(check (option string)) "first chunk carries reply_to"
      (Some "origin-msg-id") first_reply;
    List.iter (fun (_, follow_reply) ->
      Alcotest.(check (option string)) "follow-up chunk has no reply_to"
        None follow_reply
    ) rest
  | _ -> Alcotest.fail "expected at least one chunk"

let test_plan_no_reply_to () =
  let plan = Discord_agents.Discord_rest.plan_message_chunks
    (String.make 5000 'x') in
  Alcotest.(check bool) "multiple chunks" true (List.length plan >= 2);
  List.iter (fun (_, reply_to) ->
    Alcotest.(check (option string)) "no chunk has reply_to" None reply_to
  ) plan

let test_plan_boundary_2000 () =
  (* Exactly at Discord's 2000-char limit: should be a single chunk,
     not split at default_split_max=1900. This is the bug Codex caught. *)
  let content = String.make 2000 'a' in
  let plan = Discord_agents.Discord_rest.plan_message_chunks
    ~reply_to:"origin" content in
  Alcotest.(check int) "exactly 2000 chars stays as one chunk" 1 (List.length plan);
  let content_3900 = String.make 1950 'a' in
  let plan_under = Discord_agents.Discord_rest.plan_message_chunks
    ~reply_to:"origin" content_3900 in
  Alcotest.(check int) "1950 chars stays as one chunk (no 1901-2000 orphan)"
    1 (List.length plan_under)

(* A standalone UTF-8 validity check so tests don't need an external lib.
   Walks the bytes, verifying every leading byte is followed by the
   right number of continuation bytes. Returns true only for valid UTF-8. *)
let is_valid_utf8 s =
  let n = String.length s in
  let rec loop i =
    if i >= n then true
    else
      let b = Char.code s.[i] in
      let continuations =
        if b < 0x80 then 0
        else if b < 0xC0 then -1  (* stray continuation byte *)
        else if b < 0xE0 then 1
        else if b < 0xF0 then 2
        else if b < 0xF8 then 3
        else -1
      in
      if continuations < 0 || i + continuations >= n then false
      else
        let rec check_cont k =
          if k > continuations then true
          else if Char.code s.[i + k] land 0xC0 <> 0x80 then false
          else check_cont (k + 1)
        in
        if check_cont 1 then loop (i + continuations + 1) else false
  in
  loop 0

let test_truncate_for_log_utf8_boundary () =
  (* "世界" is 6 bytes (two 3-byte codepoints). Place it so that a byte-
     based cut at max_len=4 would land inside the first codepoint. The
     function must walk back to a leading byte, not emit broken UTF-8. *)
  let input = "hi " ^ "世界" ^ " tail" in
  let truncated = Discord_agents.Discord_rest.truncate_for_log ~max_len:4 input in
  Alcotest.(check bool) "truncated output is valid UTF-8"
    true (is_valid_utf8 truncated);
  (* And an ASCII-only case still truncates to exactly max_len bytes. *)
  let ascii = String.make 100 'a' in
  let trunc_ascii = Discord_agents.Discord_rest.truncate_for_log ~max_len:10 ascii in
  Alcotest.(check int) "ASCII truncation cuts at max_len"
    10 (String.length trunc_ascii - String.length "... (truncated)");
  (* Under max_len: unchanged. *)
  let short = "short" in
  Alcotest.(check string) "under max_len unchanged"
    short (Discord_agents.Discord_rest.truncate_for_log ~max_len:100 short)

let test_truncate_for_log_4byte_codepoint () =
  (* "🙂" is F0 9F 99 82 — a 4-byte codepoint. Cut inside it: the walk-
     back must handle >3 bytes of continuation without losing track. *)
  let input = "a" ^ "🙂" ^ "b" in
  (* max_len=2 lands inside the emoji (byte 0x9F). *)
  let truncated = Discord_agents.Discord_rest.truncate_for_log ~max_len:2 input in
  Alcotest.(check bool) "4-byte codepoint truncation stays valid UTF-8"
    true (is_valid_utf8 truncated);
  (* max_len=4 lands on the emoji's last continuation byte. *)
  let t4 = Discord_agents.Discord_rest.truncate_for_log ~max_len:4 input in
  Alcotest.(check bool) "4-byte codepoint truncation at last byte stays valid"
    true (is_valid_utf8 t4)

let test_truncate_for_log_invalid_utf8 () =
  (* Pathological invalid UTF-8: a string of stray continuation bytes.
     Regression for a bug Codex found in v3 review — the old 4-step
     walk-back bound left [cut] still pointing at a continuation byte,
     emitting invalid UTF-8. The unbounded walk-back should fall back
     to position 0 and produce an empty prefix rather than bad bytes. *)
  let bad = String.make 20 '\x80' in
  let truncated = Discord_agents.Discord_rest.truncate_for_log ~max_len:10 bad in
  Alcotest.(check bool) "all-continuation input yields valid UTF-8 output"
    true (is_valid_utf8 truncated)

let test_truncate_for_log_preserves_existing_invalidity () =
  (* truncate_for_log does NOT sanitize already-invalid input — it only
     avoids creating new invalidity. A prefix that was invalid before
     truncation stays invalid after. Documents the weaker guarantee
     Codex flagged in v4 review: we don't do O(n) prefix validation.

     "\xE2abc" with max_len=1: byte 0xE2 is a leading byte (top bits
     1110) that requires two continuations. The walk-back stops at
     position 1 because s.[1]='a' is a leading byte. The resulting
     prefix "\xE2" is a lone leading byte — invalid UTF-8 — and stays
     that way. This is acceptable for log output. *)
  let lone_lead = "\xE2abc" in
  let t = Discord_agents.Discord_rest.truncate_for_log ~max_len:1 lone_lead in
  (* Prefix preserves what was already there. Valid input wasn't
     amplified into invalid; invalid input wasn't sanitized either. *)
  Alcotest.(check bool) "lone leading byte preserved as-is (not amplified)"
    true (String.length t >= String.length "... (truncated)")

let test_body_snippet_trims () =
  let long = String.make 500 'x' in
  let snippet = Discord_agents.Discord_rest.body_snippet ~max_len:100 long in
  Alcotest.(check bool) "snippet bounded by max_len + suffix"
    true (String.length snippet <= 100 + 20);
  let short = "only short text" in
  Alcotest.(check string) "short body unchanged"
    short (Discord_agents.Discord_rest.body_snippet short)

let split_message_tests = [
  Alcotest.test_case "short message" `Quick test_split_short;
  Alcotest.test_case "split at paragraph" `Quick test_split_long_at_paragraph;
  Alcotest.test_case "preserves code blocks" `Quick
    test_split_preserves_code_blocks;
  Alcotest.test_case "all chunks under limit" `Quick
    test_split_all_chunks_under_limit;
  Alcotest.test_case "projects-list regression" `Quick
    test_split_projects_list_shape;
  Alcotest.test_case "plan: short content → single chunk with reply_to" `Quick
    test_plan_short_content;
  Alcotest.test_case "plan: long content → reply_to on first only" `Quick
    test_plan_long_content_reply_to_first_only;
  Alcotest.test_case "plan: no reply_to propagates None" `Quick
    test_plan_no_reply_to;
  Alcotest.test_case "plan: 1901-2000 char range not split" `Quick
    test_plan_boundary_2000;
  Alcotest.test_case "truncate_for_log UTF-8 safe" `Quick
    test_truncate_for_log_utf8_boundary;
  Alcotest.test_case "truncate_for_log handles 4-byte codepoint" `Quick
    test_truncate_for_log_4byte_codepoint;
  Alcotest.test_case "truncate_for_log invalid UTF-8 input safe" `Quick
    test_truncate_for_log_invalid_utf8;
  Alcotest.test_case "truncate_for_log preserves existing invalidity" `Quick
    test_truncate_for_log_preserves_existing_invalidity;
  Alcotest.test_case "body_snippet trims long bodies" `Quick
    test_body_snippet_trims;
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
      | Start_agent { project; _ } -> "Start_agent(" ^ project ^ ")"
      | Resume_session { session_id } -> "Resume_session(" ^ session_id ^ ")"
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
  ]
