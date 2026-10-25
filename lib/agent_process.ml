(** Agent subprocess management using Eio.

    Spawns Claude/Codex/Gemini as subprocesses with proper I/O
    handling. Each agent has its own JSONL event schema; we translate
    them all into a common [stream_event] type that the runner
    consumes:

    - Claude   — [parse_stream_json_line] over [--output-format
      stream-json].
    - Codex    — [parse_codex_json_line] over [exec --json].
    - Gemini   — [parse_gemini_stream_json_line] over [-o stream-json].

    The shared event type is the abstraction; per-agent parsers are
    the only place vendor-specific shapes appear. *)

type tool_info = {
  tool_name: string;        (** Tool name (e.g. "Read", "Edit", "Bash") *)
  tool_summary: string;     (** Human-readable summary of what the tool is doing *)
  tool_detail: string;      (** Syntax-highlighted code block for Discord display *)
}

type stream_event =
  | Text_delta of string   (** Incremental text from the agent *)
  | Result of { text: string; session_id: string option }
  | Tool_use of tool_info  (** Agent is using a tool *)
  | Tool_result of { content: string }  (** Output from a tool execution *)
  | Error of string
  | Other of string        (** Unrecognized event type *)

(** Scan text for ``` fences, returning whether it ends inside a code block
    and the language hint of the most recent opening fence.
    Returns (in_code, lang) where lang may be "" for bare fences. *)
let scan_fences text =
  let in_code = ref false in
  let lang = ref "" in
  let i = ref 0 in
  let len = String.length text in
  while !i + 2 < len do
    if text.[!i] = '`' && text.[!i+1] = '`' && text.[!i+2] = '`' then begin
      if not !in_code then begin
        let rest_start = !i + 3 in
        let eol = match String.index_from_opt text rest_start '\n' with
          | Some nl -> nl | None -> len in
        let l = String.trim (String.sub text rest_start (eol - rest_start)) in
        lang := (if String.length l > 0 && String.length l <= 20
                    && not (String.contains l ' ') then l else "");
        in_code := true
      end else begin
        in_code := false;
        lang := ""
      end;
      i := !i + 3
    end else
      incr i
  done;
  (!in_code, !lang)

(** Test whether a line looks like a markdown table row.
    A table row starts with optional whitespace then '|'. *)
let is_table_line line =
  let s = String.trim line in
  String.length s > 0 && s.[0] = '|'

(** Parse a table row into cells by splitting on '|'.
    Strips the leading and trailing empty cells produced by the outer pipes. *)
let parse_table_cells line =
  let parts = String.split_on_char '|' line in
  (* Drop leading empty from "| a | b |" → [""; " a "; " b "; ""] *)
  let parts = match parts with "" :: rest -> rest | p -> p in
  let parts = match List.rev parts with "" :: rest -> List.rev rest | _ -> parts in
  List.map String.trim parts

(** Test whether a row is a separator row (e.g. |---|---|).
    Separator cells contain only dashes and colons. *)
let is_separator_row cells =
  cells <> [] && List.for_all (fun cell ->
    String.length cell > 0 &&
    String.to_seq cell |> Seq.for_all (fun c -> c = '-' || c = ':')
  ) cells

let utf8_prefix_len_for_table_cell ~max_bytes s =
  let n = String.length s in
  let rec loop i =
    if i >= n || i >= max_bytes then i
    else
      let c = Char.code s.[i] in
      let raw_step =
        if c < 0x80 then 1
        else if c < 0xC0 then 1
        else if c < 0xE0 then 2
        else if c < 0xF0 then 3
        else 4
      in
      let step = min raw_step (n - i) in
      if i + step > max_bytes then i else loop (i + step)
  in
  loop 0

(** Default wrapping widths for desktop and mobile Discord clients. *)
let desktop_width = 120
let mobile_width = 60

(** Default number of lines to show for tool/code output.
    Users can adjust with !lines. *)
let default_output_lines = 40

(** Count backticks in a string, used to track inline code span state. *)
let count_backticks s =
  let n = ref 0 in
  String.iter (fun c -> if c = '`' then incr n) s;
  !n

(** Wrap a single line at word boundaries to fit within [max_width].
    Returns a list of wrapped lines. Does not break inside inline code
    spans (backtick pairs). Note: splits on spaces only, so leading
    indentation with tabs or multiple consecutive spaces is not preserved. *)
let wrap_line ~max_width line =
  let len = String.length line in
  if len <= max_width then [line]
  else
    let words = String.split_on_char ' ' line in
    let rec build current_line lines backtick_open = function
      | [] ->
        List.rev (current_line :: lines)
      | word :: rest ->
        let cur_len = String.length current_line in
        let word_len = String.length word in
        let word_backticks = count_backticks word in
        let new_open = if word_backticks mod 2 = 1 then not backtick_open
                       else backtick_open in
        if cur_len = 0 then
          (* First word on the line — take it even if it exceeds max_width *)
          build word lines new_open rest
        else if cur_len + 1 + word_len <= max_width || backtick_open then
          (* Fits, or we're inside a backtick span — don't break *)
          build (current_line ^ " " ^ word) lines new_open rest
        else
          build word (current_line :: lines) new_open rest
    in
    build "" [] false words

(** Wrap lines in text that are outside code blocks to fit within [max_width].
    Lines inside code blocks (``` fences) are left unchanged. *)
let wrap_text ~max_width text =
  let lines = String.split_on_char '\n' text in
  let in_code = ref false in
  let buf = Buffer.create (String.length text) in
  let first = ref true in
  let add_line line =
    if not !first then Buffer.add_char buf '\n';
    first := false;
    Buffer.add_string buf line
  in
  List.iter (fun line ->
    let trimmed = String.trim line in
    if String.length trimmed >= 3
       && trimmed.[0] = '`' && trimmed.[1] = '`' && trimmed.[2] = '`' then begin
      in_code := not !in_code;
      add_line line
    end else if !in_code then
      add_line line
    else
      List.iter add_line (wrap_line ~max_width line)
  ) lines;
  Buffer.contents buf

(** Render a table block with padded columns and wrap in a code block.
    Each column is padded to the maximum cell width in that column.
    If the table exceeds [max_width], column widths are shrunk proportionally
    and cell contents are truncated to fit.
    Separator rows are regenerated to match the padded widths. *)
let render_padded_table ?(max_width=desktop_width) table_lines =
  let parsed = List.map parse_table_cells table_lines in
  (* Compute max width per column *)
  let max_cols = List.fold_left (fun acc row -> max acc (List.length row)) 0 parsed in
  let widths = Array.make max_cols 0 in
  List.iter (fun cells ->
    List.iteri (fun i cell ->
      if i < max_cols && not (is_separator_row [cell]) then
        widths.(i) <- max widths.(i) (String.length cell)
    ) cells
  ) parsed;
  (* Ensure minimum width of 3 for separator dashes *)
  Array.iteri (fun i w -> if w < 3 then widths.(i) <- 3) widths;
  (* Constrain total width to max_width if needed.
     Total row width = 1 (leading |) + sum_i(1 + w_i + 1 + 1) = 1 + 3*n + sum(w_i)
     where n = max_cols.  We subtract the structural chars to get available space. *)
  let structural = 1 + 3 * max_cols in
  let total_content = Array.fold_left (+) 0 widths in
  let total_row_width = structural + total_content in
  if total_row_width > max_width && max_cols > 0 then begin
    let available = max max_cols (max_width - structural) in
    (* Shrink columns proportionally *)
    let scale = float_of_int available /. float_of_int total_content in
    if scale < 1.0 then
      Array.iteri (fun i w ->
        widths.(i) <- max 3 (int_of_float (float_of_int w *. scale))
      ) widths
  end;
  (* Render each row *)
  let buf = Buffer.create 256 in
  Buffer.add_string buf "```\n";
  List.iter (fun cells ->
    let is_sep = is_separator_row cells in
    Buffer.add_char buf '|';
    for i = 0 to max_cols - 1 do
      let cell = if i < List.length cells then List.nth cells i else "" in
      let w = widths.(i) in
      if is_sep then begin
        Buffer.add_char buf ' ';
        for _ = 1 to w do Buffer.add_char buf '-' done;
        Buffer.add_char buf ' '
      end else begin
        Buffer.add_char buf ' ';
        let display = if String.length cell > w
          then
            let taken = utf8_prefix_len_for_table_cell ~max_bytes:w cell in
            String.sub cell 0 taken
          else cell in
        Buffer.add_string buf display;
        for _ = 1 to w - String.length display do Buffer.add_char buf ' ' done;
        Buffer.add_char buf ' '
      end;
      Buffer.add_char buf '|'
    done;
    Buffer.add_char buf '\n'
  ) parsed;
  Buffer.add_string buf "```";
  Buffer.contents buf

(** Reformat markdown tables in text for Discord display.
    Discord doesn't render markdown tables, so we wrap them in code blocks
    with padded columns for readable alignment. Skips tables already inside
    code blocks (``` fences). *)
let reformat_tables ?(max_width=desktop_width) text =
  let lines = String.split_on_char '\n' text in
  let buf = Buffer.create (String.length text) in
  let in_code = ref false in
  let table_acc = ref [] in
  let first = ref true in
  let add_raw line =
    if not !first then Buffer.add_char buf '\n';
    first := false;
    Buffer.add_string buf line
  in
  let flush_table () =
    match !table_acc with
    | [] -> ()
    | rows ->
      let rendered = render_padded_table ~max_width (List.rev rows) in
      if not !first then Buffer.add_char buf '\n';
      first := false;
      Buffer.add_string buf rendered;
      table_acc := []
  in
  List.iter (fun line ->
    let trimmed = String.trim line in
    if String.length trimmed >= 3
       && trimmed.[0] = '`' && trimmed.[1] = '`' && trimmed.[2] = '`' then
      in_code := not !in_code;
    if !in_code then begin
      flush_table ();
      add_raw line
    end else if is_table_line line then
      table_acc := line :: !table_acc
    else begin
      flush_table ();
      add_raw line
    end
  ) lines;
  flush_table ();
  Buffer.contents buf

(** Find the byte offset where a trailing table block begins.
    Used by the streaming splitter to avoid breaking tables across messages.
    Returns [Some offset] if the text ends with table lines (outside code
    blocks) and there is non-table content before them.
    Returns [None] if there's no trailing table or the entire text is
    table content (nothing to split off). *)
let find_trailing_table_start text =
  let lines = String.split_on_char '\n' text in
  let arr = Array.of_list lines in
  let n = Array.length arr in
  if n = 0 then None
  else
    (* Forward pass: compute byte offsets and table status per line,
       tracking code block state so | inside ``` is not a table line. *)
    let in_code = ref false in
    let offsets = Array.make n 0 in
    let is_tbl = Array.make n false in
    let pos = ref 0 in
    Array.iteri (fun i line ->
      offsets.(i) <- !pos;
      let trimmed = String.trim line in
      if String.length trimmed >= 3
         && trimmed.[0] = '`' && trimmed.[1] = '`' && trimmed.[2] = '`' then
        in_code := not !in_code;
      is_tbl.(i) <- (not !in_code && is_table_line line);
      pos := !pos + String.length line + 1
    ) arr;
    (* Check if last non-empty line is a table line *)
    let last = ref (n - 1) in
    while !last >= 0 && arr.(!last) = "" do decr last done;
    if !last < 0 || not is_tbl.(!last) then None
    else
      (* Walk backwards past table lines and empty lines within the block *)
      let i = ref !last in
      while !i > 0 && (is_tbl.(!i - 1) || arr.(!i - 1) = "") do decr i done;
      (* Skip leading empty lines in the table block *)
      while !i < n && arr.(!i) = "" do incr i done;
      if !i = 0 then None  (* entire text is table — can't split *)
      else Some offsets.(!i)

(** Discord's maximum message length. *)
let discord_max_len = 2000

(** Maximum characters for tool output content displayed in a Discord
    code block.  Leaves room for fences, status headers, and hints. *)
let max_output_display_chars = 1700

(* ── Discord size budgeting ────────────────────────────────────────

   All budget checks below operate on *escaped* length: each triple-
   backtick ``` gets rewritten to ``\u200B` by escape_code_fences,
   adding 3 bytes per occurrence. UTF-8 multi-byte sequences are
   preserved whole — we never split a codepoint.

   The single primitive below (walk_to_budget) powers every truncation
   site so there is one cost model to audit. *)

(** Length of a UTF-8 codepoint starting at byte position [i] in [s].
    For malformed bytes (including stray continuation bytes) returns 1
    so progress is guaranteed. *)
let utf8_step s i =
  let c = Char.code s.[i] in
  if c < 0x80 then 1         (* ASCII *)
  else if c < 0xC0 then 1    (* continuation byte — malformed context *)
  else if c < 0xE0 then 2    (* 2-byte sequence *)
  else if c < 0xF0 then 3    (* 3-byte sequence *)
  else 4                     (* 4-byte sequence *)

(** Walk back to the nearest UTF-8 codepoint boundary at or before [pos].
    A continuation byte (0x80–0xBF) means we're mid-codepoint; lead
    bytes and ASCII bytes start a codepoint and are valid split points.
    Used by [split_message] to keep its fallback (no separator within
    budget) from chopping a multibyte character. *)
let utf8_boundary_at_or_before s pos =
  let n = String.length s in
  let p = ref (min pos n) in
  while !p > 0 && !p < n
    && (let c = Char.code s.[!p] in c >= 0x80 && c < 0xC0) do
    decr p
  done;
  !p

(** Walk forward from byte [start] in [s], advancing one unit at a time
    (a triple-backtick counts as 6 budget bytes / 3 source bytes; any
    other UTF-8 codepoint costs its raw byte length).  Returns the byte
    offset one past the last unit that fit within [budget], along with
    the budget consumed.  The returned offset is always on a codepoint
    boundary, so [String.sub s start (result - start)] is valid UTF-8. *)
let walk_to_budget ~start ~budget s =
  let n = String.length s in
  let consumed = ref 0 in
  let i = ref start in
  let stop = ref false in
  while not !stop && !i < n do
    let is_triple = !i + 2 < n
      && s.[!i] = '`' && s.[!i+1] = '`' && s.[!i+2] = '`' in
    let raw_step = if is_triple then 3 else utf8_step s !i in
    (* Clamp step to buffer so a malformed lead byte claiming more bytes
       than remain (e.g. lone 0xF0 at end of subprocess stdout) can't
       produce an offset past the buffer end. *)
    let step = min raw_step (n - !i) in
    let cost = if is_triple then 6 else step in
    if !consumed + cost > budget then stop := true
    else begin
      consumed := !consumed + cost;
      i := !i + step
    end
  done;
  (!i, !consumed)

(** Post-escape byte length of [s]: len + 3 per ``` occurrence. *)
let escaped_length s =
  let (_, consumed) = walk_to_budget ~start:0 ~budget:max_int s in
  consumed

(** Largest prefix of [s] (starting at [start]) whose escaped length is
    at most [max_chars].  Returns the byte count to take, guaranteed to
    land on a UTF-8 codepoint boundary.  Always advances at least one
    codepoint to prevent infinite loops, even if a single unit exceeds
    the budget. *)
let take_fitting_prefix ?(start=0) ~max_chars s =
  let n = String.length s in
  if start >= n then 0
  else
    let (next, _) = walk_to_budget ~start ~budget:max_chars s in
    if next > start then next - start
    else
      (* Single unit at [start] exceeds budget — emit it anyway,
         clamped to the buffer in case of malformed UTF-8 lead bytes *)
      let is_triple = start + 2 < n
        && s.[start] = '`' && s.[start+1] = '`' && s.[start+2] = '`' in
      let raw_step = if is_triple then 3 else utf8_step s start in
      min raw_step (n - start)

let is_ascii_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

(** Largest display-safe prefix, preferring a prior whitespace boundary
    over the hard UTF-8 budget boundary. If no whitespace fits in the
    window, falls back to [take_fitting_prefix] so long unbroken tokens
    still make progress. *)
let take_word_safe_fitting_prefix ?(start=0) ~max_chars s =
  let n = String.length s in
  let hard_len = take_fitting_prefix ~start ~max_chars s in
  let hard_end = start + hard_len in
  if hard_end >= n then hard_len
  else
    let rec find_space i =
      if i <= start then None
      else if is_ascii_whitespace s.[i - 1] then Some i
      else find_space (i - 1)
    in
    match find_space hard_end with
    | Some i when i > start -> i - start
    | _ -> hard_len

(** UTF-8 safe truncation for inline summary strings: caps at [max_chars]
    bytes (post-fence-escape) and appends "...".  Returns [s] unchanged
    if it already fits.  Always lands on a UTF-8 codepoint boundary. *)
let truncate_inline ~max_chars s =
  if escaped_length s <= max_chars then s
  else
    let taken = take_fitting_prefix ~max_chars s in
    String.sub s 0 taken ^ "..."

(** Split a single line into chunks each fitting within [max_chars]
    post-escape.  Short lines pass through unchanged. *)
let chunk_long_line ~max_chars line =
  if escaped_length line <= max_chars then [line]
  else
    let n = String.length line in
    let chunks = ref [] in
    let pos = ref 0 in
    while !pos < n do
      let len = take_word_safe_fitting_prefix ~start:!pos ~max_chars line in
      chunks := String.sub line !pos len :: !chunks;
      pos := !pos + len
    done;
    List.rev !chunks

(** Split text into lines, then split any oversized lines into chunks.
    Produces a uniform list where every entry fits within max_chars,
    so paging by index always lands on a Discord-displayable unit. *)
let split_into_chunks ~max_chars text =
  List.concat_map (chunk_long_line ~max_chars)
    (String.split_on_char '\n' text)

(** Truncation result: the displayable lines, how many were shown,
    the total line count, and whether anything was hidden (either lines
    dropped OR a line char-truncated by the single-line fallback). *)
type truncation = {
  display : string list;
  shown : int;
  total : int;
  was_truncated : bool;
}

(** Truncate lines for Discord display in a single O(n) pass.
    Respects both [max_lines] and [max_chars] (evaluated against the
    post-escape length).  If the first line alone exceeds [max_chars],
    it is char-truncated so the user always sees something.

    Invariant: [String.concat "\n" display |> escape_code_fences] has
    length at most [max_chars] bytes. *)
let truncate_for_display ~max_lines ~max_chars (lines : string list) =
  (* Walk the input once: accumulate lines while line count and escaped
     char budget both allow it. Remaining lines are counted but not kept. *)
  let rec loop lines shown elen acc =
    match lines with
    | [] -> { display = List.rev acc; shown; total = shown;
              was_truncated = false }
    | l :: rest ->
      if shown >= max_lines then
        { display = List.rev acc; shown;
          total = shown + 1 + List.length rest;
          was_truncated = true }
      else
        let sep = if shown > 0 then 1 else 0 in
        let new_elen = elen + escaped_length l + sep in
        if new_elen <= max_chars then
          loop rest (shown + 1) new_elen (l :: acc)
        else if shown = 0 then
          (* First line alone exceeds budget — char-truncate it so the
             user always sees something rather than an empty block. *)
          let taken = take_word_safe_fitting_prefix ~max_chars l in
          let trimmed = String.sub l 0 taken in
          { display = [trimmed]; shown = 1;
            total = 1 + List.length rest;
            was_truncated = true }
        else
          { display = List.rev acc; shown;
            total = shown + 1 + List.length rest;
            was_truncated = true }
  in
  loop lines 0 0 []

(** Default split threshold — leaves room for code block fences added
    during splitting (closing "\n```" = 4 chars, opening "```lang\n" ≤ 24 chars). *)
let default_split_max = 1900

(** Split text into chunks for Discord's 2000-char limit.
    Splits at paragraph breaks > newlines > spaces.
    Handles code blocks by closing/reopening ``` with language hints
    preserved (e.g. ```ocaml gets reopened as ```ocaml). *)
let split_message ?(max_len=default_split_max) text =
  let len = String.length text in
  if len <= max_len then [text]
  else
    let find_split_point pos limit =
      let try_find sep =
        let sep_len = String.length sep in
        let best = ref None in
        let i = ref pos in
        while !i + sep_len <= limit do
          if String.sub text !i sep_len = sep then best := Some !i;
          incr i
        done;
        !best
      in
      match try_find "\n\n" with
      | Some p -> p + 2
      | None ->
        match try_find "\n" with
        | Some p -> p + 1
        | None ->
          match try_find " " with
          | Some p -> p + 1
          | None ->
            (* No whitespace within budget — split on a UTF-8
               codepoint boundary so we never produce malformed
               bytes for long unbroken non-ASCII content (Codex
               JSON error blobs without spaces are the typical
               case).  Boundary-walking is bounded by [pos] so a
               full continuation-byte run still makes progress. *)
            max (pos + 1) (utf8_boundary_at_or_before text limit)
    in
    (* code_state: None = not in code block, Some lang = in code block.
       lang may be "" for bare ``` fences. *)
    let rec split pos code_state acc =
      if pos >= len then List.rev acc
      else
        let remaining = len - pos in
        let prefix = match code_state with
          | None -> ""
          | Some lang -> "```" ^ lang ^ "\n"
        in
        (* Reserve space for both the prefix and a potential closing "\n```" (4 chars) *)
        let closing_reserve = 4 in
        let effective_max = max_len - String.length prefix - closing_reserve in
        if remaining <= effective_max then
          List.rev ((prefix ^ String.sub text pos remaining) :: acc)
        else
          let split_at = find_split_point pos (pos + effective_max) in
          let raw_chunk = String.sub text pos (split_at - pos) in
          let chunk = prefix ^ raw_chunk in
          let (ends_in_code, lang) = scan_fences chunk in
          let chunk = if ends_in_code then chunk ^ "\n```" else chunk in
          let next_state = if ends_in_code then Some lang else None in
          split split_at next_state (chunk :: acc)
    in
    split 0 None []

(** Sanitize a string for use inside Discord inline backticks.
    Replaces backticks with single quotes and newlines with spaces. *)
let sanitize_for_inline_code s =
  String.init (String.length s) (fun i ->
    match s.[i] with
    | '`' -> '\''
    | '\n' | '\r' -> ' '
    | c -> c)

(** Extract a short summary from tool_use input JSON for display.
    The result is safe for embedding in Discord inline code spans. *)
let summarize_tool_input name input =
  let open Yojson.Safe.Util in
  let get key = input |> member key |> to_string_option in
  let get_any key =
    match input |> member key with
    | `Null -> None
    | v -> Some (Yojson.Safe.to_string v)
    | exception _ -> None
  in
  let basename s =
    match String.rindex_opt s '/' with
    | Some i -> String.sub s (i + 1) (String.length s - i - 1)
    | None -> s
  in
  let truncate n s = truncate_inline ~max_chars:n s in
  let clean n s = truncate n (sanitize_for_inline_code s) in
  let safe_basename p = sanitize_for_inline_code (basename p) in
  match name with
  | "Read" ->
    let path = match get "file_path" with Some p -> p | None -> "" in
    let detail = match get_any "offset", get_any "limit" with
      | Some o, Some l -> " (offset " ^ o ^ ", limit " ^ l ^ ")"
      | _, Some l -> " (" ^ l ^ " lines)"
      | _ -> "" in
    clean 120 path ^ detail
  | "Edit" ->
    let path = match get "file_path" with Some p -> p | None -> "" in
    let old_str = match get "old_string" with
      | Some s -> " — replacing " ^ truncate 40 s | None -> "" in
    clean 80 path ^ old_str
  | "Write" ->
    (match get "file_path" with Some p -> clean 120 p | None -> "")
  | "Bash" ->
    (match get "command" with
     | Some c -> clean 120 c | None -> "")
  | "Grep" ->
    (match get "pattern" with
     | Some pat ->
       let path = match get "path" with
         | Some p -> " in " ^ safe_basename p | None -> "" in
       "/" ^ clean 60 pat ^ "/" ^ path
     | None -> "")
  | "Glob" ->
    (match get "pattern" with
     | Some pat -> clean 80 pat | None -> "")
  | "Agent" | "Task" ->
    (match get "description" with
     | Some d -> clean 80 d | None -> "")
  | "Skill" ->
    (match get "skill" with
     | Some s -> clean 40 s | None -> "")
  | "WebSearch" | "WebFetch" ->
    (match get "query" with
     | Some q -> clean 80 q
     | None ->
       match get "url" with Some u -> clean 80 u | None -> "")
  | _ ->
    (* For unknown tools, try to show something useful *)
    let keys = Yojson.Safe.Util.keys input in
    match keys with
    | k :: _ -> (match get k with Some v -> clean 60 v | None -> "")
    | [] -> ""

(** Maximum length for tool input detail code blocks shown inline in
    Discord status messages.  Must fit within a single Discord message
    (~2000 chars) alongside the status header.  Doubled from the original
    800 to show more context for diffs and commands.
    Tool *output* uses line-based truncation with !scroll pagination
    instead of this character cap. *)
let max_detail_len = 1_600

(** Guess a syntax highlighting language from a file extension. *)
let lang_of_path path =
  match String.rindex_opt path '.' with
  | None -> ""
  | Some i ->
    let ext = String.sub path (i + 1) (String.length path - i - 1) in
    match String.lowercase_ascii ext with
    | "ml" | "mli" -> "ocaml"
    | "py" -> "python"
    | "js" | "mjs" | "cjs" -> "javascript"
    | "ts" | "tsx" -> "typescript"
    | "rs" -> "rust"
    | "go" -> "go"
    | "rb" -> "ruby"
    | "sh" | "bash" -> "bash"
    | "json" -> "json"
    | "yaml" | "yml" -> "yaml"
    | "toml" -> "toml"
    | "html" -> "html"
    | "css" -> "css"
    | "sql" -> "sql"
    | "c" | "h" -> "c"
    | "cpp" | "cc" | "hpp" -> "cpp"
    | "java" -> "java"
    | "md" -> "markdown"
    | "nix" -> "nix"
    | "ex" | "exs" -> "elixir"
    | _ -> ""

(** Escape triple backticks in text destined for a Discord code block.
    Discord has no escape mechanism inside code blocks, so we replace
    ``` with `` ` (zero-width space before last backtick) to prevent
    premature fence closure. *)
let escape_code_fences s =
  let len = String.length s in
  if len < 3 then s
  else
    let buf = Buffer.create (len + 10) in
    let i = ref 0 in
    while !i < len do
      if !i + 2 < len && s.[!i] = '`' && s.[!i+1] = '`' && s.[!i+2] = '`' then begin
        (* Replace ``` with ``\u200B` — zero-width space breaks the fence *)
        Buffer.add_string buf "``\xE2\x80\x8B`";
        i := !i + 3
      end else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    done;
    Buffer.contents buf

(** Escape nested triple backticks in Discord message text.
    Scans line by line tracking code block state. Lines that contain ```
    while already inside a code block get their ``` escaped with a
    zero-width space, preventing premature fence closure. Top-level
    fence markers (that open/close code blocks) are left untouched.

    Limitation: a bare ``` line inside a code block is ambiguous — it
    could be literal content or a closing fence. We treat lines that are
    only backticks and whitespace as closing fences. This is inherent to
    markdown: there is no escape mechanism inside code blocks, and Discord
    does not support 4+ backtick fences as an alternative delimiter. *)
let escape_nested_fences text =
  let lines = String.split_on_char '\n' text in
  let in_code = ref false in
  let buf = Buffer.create (String.length text) in
  let first = ref true in
  List.iter (fun line ->
    if not !first then Buffer.add_char buf '\n';
    first := false;
    let trimmed = String.trim line in
    let is_fence = String.length trimmed >= 3
      && trimmed.[0] = '`' && trimmed.[1] = '`' && trimmed.[2] = '`' in
    if is_fence && not !in_code then begin
      (* Opening fence — pass through unchanged *)
      in_code := true;
      Buffer.add_string buf line
    end else if is_fence && !in_code then begin
      (* Could be a closing fence or a nested fence.
         Check: a closing fence is just ``` (possibly with trailing space).
         A nested fence would have content after it like ```ocaml or ```diff
         but we can't reliably distinguish, so we check if the line is ONLY
         backticks/whitespace — if so, it's a closing fence. *)
      let only_backticks = String.for_all (fun c ->
        c = '`' || c = ' ' || c = '\t') trimmed in
      if only_backticks then begin
        (* Closing fence — pass through, exit code block *)
        in_code := false;
        Buffer.add_string buf line
      end else begin
        (* Nested fence (e.g. ```ocaml inside a code block) — escape it *)
        Buffer.add_string buf (escape_code_fences line)
      end
    end else if !in_code then
      (* Inside code block — escape any ``` sequences in the content *)
      Buffer.add_string buf (escape_code_fences line)
    else
      Buffer.add_string buf line
  ) lines;
  Buffer.contents buf

(** Wrap [content] in a Discord code block with [lang] syntax hinting,
    capping the post-escape byte length at [max_detail_len] (UTF-8 safe,
    fence-aware) and rewriting any inner ``` so they cannot close the
    outer fence. Used by both Claude's tool-detail rendering and Codex's
    command/file-change detail rendering. *)
let code_block_capped ~lang content =
  let capped =
    if escaped_length content <= max_detail_len then content
    else
      let taken = take_fitting_prefix ~max_chars:max_detail_len content in
      String.sub content 0 taken ^ "\n... (truncated)"
  in
  Printf.sprintf "```%s\n%s\n```" lang (escape_code_fences capped)

(** Generate a syntax-highlighted code block showing tool content details.
    Returns "" if the tool doesn't have interesting content to show. *)
let detail_of_tool_input name input =
  let open Yojson.Safe.Util in
  let get key = input |> member key |> to_string_option in
  match name with
  | "Edit" ->
    let old_s = match get "old_string" with Some s -> s | None -> "" in
    let new_s = match get "new_string" with Some s -> s | None -> "" in
    if old_s = "" && new_s = "" then ""
    else
      let content =
        if old_s <> "" && new_s <> "" then
          (* Show as diff-style *)
          let old_lines = String.split_on_char '\n' old_s in
          let new_lines = String.split_on_char '\n' new_s in
          let buf = Buffer.create 256 in
          List.iter (fun l ->
            Buffer.add_string buf ("- " ^ l ^ "\n")
          ) old_lines;
          List.iter (fun l ->
            Buffer.add_string buf ("+ " ^ l ^ "\n")
          ) new_lines;
          Buffer.contents buf
        else if new_s <> "" then new_s
        else old_s
      in
      code_block_capped ~lang:"diff" content
  | "Bash" ->
    (match get "command" with
     | Some cmd when String.length cmd > 0 ->
       code_block_capped ~lang:"bash" cmd
     | _ -> "")
  | "Write" ->
    let path = match get "file_path" with Some p -> p | None -> "" in
    let lang = lang_of_path path in
    (match get "content" with
     | Some c when String.length c > 0 -> code_block_capped ~lang c
     | _ -> "")
  | "Grep" ->
    (match get "pattern" with
     | Some pat when String.length pat > 0 ->
       code_block_capped ~lang:""
         ("/" ^ truncate_inline ~max_chars:200 pat ^ "/")
     | _ -> "")
  | _ -> ""

(** Yojson accessor helpers shared by all three parsers. Collapse the
    noisy [j |> member k |> to_string_option |> Option.value ~default]
    chain that recurs everywhere. *)

let get_string_opt key json =
  Yojson.Safe.Util.(json |> member key |> to_string_option)

let get_string ~default key json =
  Option.value (get_string_opt key json) ~default

let get_int_opt key json =
  Yojson.Safe.Util.(json |> member key |> to_int_option)

(** Parse a stream-json line (Claude / Gemini share-the-shape, though
    Gemini has its own parser).  Returns a list because a single
    assistant message can contain both text and tool_use content blocks. *)
let parse_stream_json_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    match get_string_opt "type" json with
    | Some "assistant" ->
      let content = json |> member "message" |> member "content" in
      let events = match content with
        | `List items ->
          List.filter_map (fun item ->
            match get_string_opt "type" item with
            | Some "text" ->
              (match get_string_opt "text" item with
               | Some t -> Some (Text_delta t)
               | None -> None)
            | Some "tool_use" ->
              let name = get_string ~default:"unknown" "name" item in
              let input = item |> member "input" in
              let summary = summarize_tool_input name input in
              let detail = detail_of_tool_input name input in
              Some (Tool_use { tool_name = name; tool_summary = summary;
                               tool_detail = detail })
            | Some "tool_result" ->
              (* Tool result content block — extract the output text *)
              let content = match item |> member "content" with
                | `String s -> s
                | `List parts ->
                  (* Content may be a list of {type: "text", text: "..."} *)
                  let texts = List.filter_map (fun p ->
                    match get_string_opt "type" p with
                    | Some "text" -> get_string_opt "text" p
                    | _ -> None
                  ) parts in
                  String.concat "\n" texts
                | _ -> ""
              in
              if content = "" then None
              else Some (Tool_result { content })
            | _ -> None
          ) items
        | _ -> []
      in
      (match events with [] -> [Other line] | _ -> events)
    | Some "result" ->
      let result_text = get_string ~default:"" "result" json in
      let session_id = get_string_opt "session_id" json in
      [Result { text = result_text; session_id }]
    | Some "user" ->
      (* User messages carry tool_result content blocks with tool output.
         Also check tool_use_result for structured stdout/stderr. *)
      let structured_output =
        match json |> member "tool_use_result" with
        | `Null -> None
        | obj ->
          let stdout = get_string ~default:"" "stdout" obj in
          let stderr = get_string ~default:"" "stderr" obj in
          let combined = match stdout, stderr with
            | "", "" -> ""
            | s, "" -> s
            | "", e -> "STDERR:\n" ^ e
            | s, e -> s ^ "\nSTDERR:\n" ^ e
          in
          if combined = "" then None else Some combined
        | exception _ -> None
      in
      (match structured_output with
       | Some content -> [Tool_result { content }]
       | None ->
         (* Fall back to content blocks *)
         let content = json |> member "message" |> member "content" in
         let results = match content with
           | `List items ->
             List.filter_map (fun item ->
               match get_string_opt "type" item with
               | Some "tool_result" ->
                 let c = match item |> member "content" with
                   | `String s -> s
                   | _ -> "" in
                 if c = "" then None
                 else Some (Tool_result { content = c })
               | _ -> None
             ) items
           | _ -> []
         in
         if results = [] then [Other line] else results)
    | _ -> [Other line]
  with _ -> [Other line]

(* ── Codex JSON parser ─────────────────────────────────────────────

   Codex emits one JSON object per line on stdout when invoked with
   [--json]. The dispatcher [parse_codex_json_line] matches on the
   top-level [type] field and delegates to a small helper per shape:

   | top-level type    | helper / treatment
   |-------------------|---------------------------------------------
   | thread.started    | capture session id via Result event
   | turn.started      | ignored
   | turn.completed    | flush via empty Result event
   | turn.failed       | lift error message into Error event
   | error             | lift error message into Error event
   | item.started      | parse_codex_item ~completed:false
   | item.completed    | parse_codex_item ~completed:true

   Item-level shapes recognized inside item.* envelopes:

   | item.type         | helper / treatment
   |-------------------|---------------------------------------------
   | agent_message     | Text_delta (one whole message, not a delta)
   | command_execution | Tool_use on start, Tool_result on completion
   | file_change       | Tool_use describing the path(s) modified
   | error             | Error event (non-fatal)
   | reasoning         | dropped

   Anything unrecognized falls through to Other(raw line). Errors are
   forwarded as [Error] events so [Agent_runner] can surface Codex's
   stdout-only error frames; without this they vanish into [Other]
   and the user sees an empty "agent exited with code N" message. *)

(** Codex error frames carry the message in one of three shapes:
    - top-level [{message}] (the [error] frame)
    - nested object [{error: {message}}] (the [turn.failed] frame)
    - nested string [{error: "..."}] (occasionally seen; the message
      is inlined as a string)
    Resolving each explicitly avoids a [Type_error] from the Yojson
    accessors when [error] turns out to be a scalar. *)
let codex_error_message json =
  let default = "unspecified codex error" in
  match Yojson.Safe.Util.member "error" json with
  | `Assoc _ as nested -> get_string ~default "message" nested
  | `String s -> s
  | _ -> get_string ~default "message" json

(** A single file-change entry. [kind] is normalized to the verb used
    by Codex ("add", "update", "delete") — empty values become
    "modified" so the rendered summary never says "path ()". *)
type file_change_entry = { path : string; kind : string }

let parse_file_change_entries item =
  let raw = match Yojson.Safe.Util.member "changes" item with
    | `List xs -> xs | _ -> [] in
  List.filter_map (fun c ->
    let path = get_string ~default:"" "path" c in
    let kind = match get_string ~default:"" "kind" c with
      | "" -> "modified" | k -> k in
    if path = "" then None else Some { path; kind }
  ) raw

(* Per-item helpers. Each takes the raw [completed] flag so the
   item.started / item.completed split is owned where the semantics
   are actually understood, not in the dispatcher.

   Codex emits [agent_message] / [file_change] / [error] only as
   item.completed in the wild; the explicit no-op on item.started
   future-proofs against a Codex change without polluting Other-line
   debug logs. [command_execution] is the only shape with a real
   start-and-end lifecycle. *)

(** Fully-formed assistant text. Codex emits this once per item with
    the complete message — there are no incremental deltas. We
    normalize the tail to exactly "\n\n" so successive messages in one
    turn render as separate paragraphs ([msg1msg2] would otherwise
    corrupt markdown), without producing "\n\n\n+" when the message
    already ended in newlines. *)
let parse_codex_agent_message ~completed item =
  let normalize_paragraph_end s =
    let rec back i =
      if i = 0 || s.[i-1] <> '\n' then i else back (i - 1)
    in
    String.sub s 0 (back (String.length s)) ^ "\n\n"
  in
  if not completed then []
  else match get_string_opt "text" item with
    | Some text when text <> "" -> [Text_delta (normalize_paragraph_end text)]
    | _ -> []

(** Bash command lifecycle. The started event opens the tool block;
    the completed event closes it with the captured output. *)
let parse_codex_command_execution ~completed item =
  let cmd = get_string ~default:"" "command" item in
  if not completed then
    let summary = truncate_inline ~max_chars:120
      (sanitize_for_inline_code cmd) in
    let detail = if cmd = "" then ""
      else code_block_capped ~lang:"bash" cmd in
    [Tool_use { tool_name = "Bash";
                tool_summary = summary; tool_detail = detail }]
  else
    let output = get_string ~default:"" "aggregated_output" item in
    let header = match get_int_opt "exit_code" item with
      | Some 0 | None -> ""
      | Some n -> Printf.sprintf "[exit %d]\n" n in
    let content = header ^ output in
    if content = "" then [] else [Tool_result { content }]

(** Files modified by Codex in a single item. Single-file changes get
    a compact inline summary; multi-file changes use a code-block
    detail listing each path on its own line. The tool name reflects
    whether every entry is an [add] (Write) or there's at least one
    in-place edit (Edit). Empty-path entries are dropped silently —
    Codex doesn't emit them in practice and they would render as a
    contentless tool_use. *)
let parse_codex_file_change ~completed item =
  if not completed then [] else
  let entries = parse_file_change_entries item in
  let tool_name_for entries =
    if List.for_all (fun e -> e.kind = "add") entries then "Write"
    else "Edit"
  in
  match entries with
  | [] -> []
  | [{ path; kind }] ->
    let summary = truncate_inline ~max_chars:120
      (Printf.sprintf "%s (%s)"
         (sanitize_for_inline_code path) kind) in
    [Tool_use { tool_name = tool_name_for entries;
                tool_summary = summary; tool_detail = "" }]
  | many ->
    let buf = Buffer.create 256 in
    List.iter (fun { path; kind } ->
      Buffer.add_string buf (Printf.sprintf "%s %s\n" kind path)
    ) many;
    let detail = code_block_capped ~lang:"" (Buffer.contents buf) in
    let summary = Printf.sprintf "%d files" (List.length many) in
    [Tool_use { tool_name = tool_name_for many;
                tool_summary = summary; tool_detail = detail }]

(** Non-fatal error item. Forwarded as an [Error] event so
    [Agent_runner] can surface the message to Discord. *)
let parse_codex_error ~completed item =
  if completed then [Error (codex_error_message item)] else []

(** Item-level dispatch. Every known item type has explicit handling;
    only genuinely unrecognized shapes fall through to [Other]. *)
let parse_codex_item ~completed ~line item =
  match get_string_opt "type" item with
  | Some "agent_message" -> parse_codex_agent_message ~completed item
  | Some "command_execution" -> parse_codex_command_execution ~completed item
  | Some "file_change" -> parse_codex_file_change ~completed item
  | Some "error" -> parse_codex_error ~completed item
  | Some "reasoning" -> []
  | _ -> [Other line]

let parse_codex_json_line line =
  try
    let json = Yojson.Safe.from_string line in
    match get_string_opt "type" json with
    | Some "thread.started" ->
      (match get_string_opt "thread_id" json with
       | Some tid -> [Result { text = ""; session_id = Some tid }]
       | None -> [Other line])
    | Some "turn.completed" -> [Result { text = ""; session_id = None }]
    | Some "turn.started" -> []
    | Some ("turn.failed" | "error") -> [Error (codex_error_message json)]
    | Some "item.started" ->
      parse_codex_item ~completed:false ~line
        (Yojson.Safe.Util.member "item" json)
    | Some "item.completed" ->
      parse_codex_item ~completed:true ~line
        (Yojson.Safe.Util.member "item" json)
    | _ -> [Other line]
  with _ -> [Other line]

(* ── Gemini JSON parser ────────────────────────────────────────────

   Unmaintained: Gemini support survives only as an API-key fallback
   since Google withdrew the consumer OAuth path. Keep it compiling;
   don't extend it. See docs/agy-harness.md.

   Gemini's [-o stream-json] schema is flat — one event per line, each
   with a top-level [type] field. The events we care about:

   | type        | shape we read
   |-------------|----------------------------------------------
   | init        | {session_id} → captured for resume
   | message     | {role: assistant, content, delta:true} → text
   | message     | {role: user, ...} → ignored (echo of our prompt)
   | tool_use    | {tool_name, parameters} → Tool_use after rename
   | tool_result | {status, output, error?} → Tool_result
   | result      | {status, stats} → flush

   Gemini's tool names differ from Claude's; [gemini_tool_name_of]
   maps the common ones so the runner's emoji/verb table stays agent-
   agnostic. Unknown names pass through unchanged. *)

let gemini_tool_name_of = function
  | "run_shell_command" -> "Bash"
  | "read_file" -> "Read"
  | "write_file" -> "Write"
  | "replace" | "edit" -> "Edit"
  | "search_file_content" -> "Grep"
  | "glob" -> "Glob"
  | "web_fetch" -> "WebFetch"
  | "google_web_search" | "web_search" -> "WebSearch"
  | other -> other

let parse_gemini_stream_json_line line =
  try
    let json = Yojson.Safe.from_string line in
    match get_string_opt "type" json with
    | Some "init" ->
      (match get_string_opt "session_id" json with
       | Some sid -> [Result { text = ""; session_id = Some sid }]
       | None -> [])
    | Some "message" ->
      (match get_string_opt "role" json with
       | Some "assistant" ->
         (match get_string_opt "content" json with
          | Some text when text <> "" -> [Text_delta text]
          | _ -> [])
       | _ -> [])  (* user echo or unknown role: ignored *)
    | Some "tool_use" ->
      let name = gemini_tool_name_of
        (get_string ~default:"unknown" "tool_name" json) in
      let params = Yojson.Safe.Util.member "parameters" json in
      let summary = summarize_tool_input name params in
      let detail = detail_of_tool_input name params in
      [Tool_use { tool_name = name; tool_summary = summary;
                  tool_detail = detail }]
    | Some "tool_result" ->
      let status = get_string ~default:"" "status" json in
      let output = get_string ~default:"" "output" json in
      let err_msg =
        match Yojson.Safe.Util.member "error" json with
        | `Assoc _ as obj -> get_string ~default:"" "message" obj
        | `String s -> s
        | _ -> ""
      in
      let content = match status, output, err_msg with
        | _, "", "" -> ""
        | "error", "", m -> Printf.sprintf "[error] %s" m
        | "error", o, "" -> Printf.sprintf "[error] %s" o
        | "error", o, m -> Printf.sprintf "[error] %s\n%s" m o
        | _, o, _ -> o
      in
      if content = "" then [] else [Tool_result { content }]
    | Some "result" ->
      (* Turn complete. session_id was captured at init; assistant text
         was already delivered as deltas. Empty Result triggers a flush
         in agent_runner, matching Codex's turn.completed handling. *)
      [Result { text = ""; session_id = None }]
    | _ -> [Other line]
  with _ -> [Other line]

(* ── agy (Antigravity CLI) JSON parser ─────────────────────────────

   agy's [--output-format stream-json] is undocumented in [--help] but
   accepted by its flag parser; see docs/agy-harness.md. Events are
   discriminated by a top-level [event] field, not [type]. Shapes
   captured from signed-in agy 1.1.3 runs:

     {"event":"init","conversation_id":"…","init":{"cwd":"…","tools":[…],…}}
     {"event":"step_update","step_update":{"conversation_id":"…",
      "step_index":N,"state":"DONE","step_type":"user_input|agent_response|…"}}
     {"event":"result","result":{"conversation_id":"…","status":"…",
      "response":"…","error":"…", …}}

   [init] arrives first, so the conversation id is reported from it as
   well as from [result]; a turn that dies mid-run still leaves a
   resumable id. [step_update] carries no text or tool detail in the
   runs captured so far (all of which ended in quota errors), so it
   passes through as [Other]. A text-delta field exists in the binary
   but hasn't been observed on the wire, so it isn't guessed at. Until
   it is, the final [response] is the only text the user sees,
   delivered as one [Text_delta]. Once text deltas are parsed, drop
   that delta or the reply will appear twice.

   An empty [conversation_id] (agy emits one when it fails before a
   conversation exists, e.g. signed out) is treated as no id, so the
   session stays unconfirmed and the next turn starts fresh. *)

let parse_agy_stream_json_line line =
  try
    let json = Yojson.Safe.from_string line in
    match get_string_opt "event" json with
    | Some "init" ->
      (match get_string_opt "conversation_id" json with
       | Some id when String.trim id <> "" ->
         [Result { text = ""; session_id = Some id }]
       | _ -> [Other line])
    | Some "result" ->
      let result = Yojson.Safe.Util.member "result" json in
      let session_id =
        match get_string_opt "conversation_id" result with
        | Some id when String.trim id <> "" -> Some id
        | _ -> None
      in
      let response = get_string ~default:"" "response" result in
      let status = get_string ~default:"" "status" result in
      let error = get_string ~default:"" "error" result in
      let text = if response = "" then [] else [Text_delta response] in
      (* agy's success status value hasn't been observed yet, so an
         unfamiliar non-empty status is logged, not failed. A result
         with no status at all doesn't match any shape we've seen and
         is reported, rather than passed off as a silent success.
         The "SUCCESS" arm is a guess that only keeps the log quiet. *)
      let err =
        match String.uppercase_ascii (String.trim status), error with
        | _, e when String.trim e <> "" -> [Error ("agy: " ^ e)]
        | "ERROR", _ -> [Error "agy: run ended with status ERROR"]
        | "", _ -> [Error "agy: result event has no status"]
        | "SUCCESS", _ -> []
        | other, _ ->
          Logs.info (fun m -> m "agy: unrecognized result status %S" other);
          []
      in
      text @ err @ [Result { text = ""; session_id }]
    | _ -> [Other line]
  with _ -> [Other line]

(** [agy --conversation <id>] with an id agy doesn't know does not
    fail: it silently starts a new conversation under a fresh id
    (verified on agy 1.1.3). The runner then adopts the new id, so the
    thread keeps working, but the agent has lost its earlier context.
    Say so once, on the first reported id that differs from the one we
    asked to resume. Returns a filter over each line's events. *)
let agy_resume_guard ~requested =
  let warned = ref false in
  fun events ->
    if !warned then events
    else
      match List.find_map (function
          | Result { session_id = Some id; _ } when id <> requested -> Some id
          | _ -> None) events with
      | None -> events
      | Some fresh ->
        warned := true;
        Text_delta (Printf.sprintf
          "⚠️ agy could not resume conversation `%s` and started a new \
           one (`%s`). Earlier context from this thread is not available \
           to it.\n\n"
          requested fresh)
        :: events

(** Build the command args for an agent invocation. *)
let model_arg = function
  | Some model when String.trim model <> "" -> ["--model"; String.trim model]
  | _ -> []

let claude_args ~model ~reasoning_effort
    ~session_id ~session_id_confirmed ~message_count:_
    ~fork_from_session_id ~prompt =
  let session_flag =
    match fork_from_session_id with
    | Some fork_from -> ["--resume"; fork_from; "--fork-session"]
    | None when not session_id_confirmed -> ["--session-id"; session_id]
    | None ->
      ["--resume"; session_id]
  in
  let effort_arg = match reasoning_effort with
    | Some effort ->
      ["--effort"; Config.string_of_reasoning_effort effort]
    | None -> []
  in
  ["claude"; "-p"; "--verbose"; "--output-format"; "stream-json"]
  @ model_arg model @ effort_arg @ session_flag @ [prompt]

(* ── MCP server config (shared by all three agents) ──────────────

   All three agents expose the bot's MCP tools (start_session,
   list_*, etc.) by pointing at the OCaml discord-agents-mcp
   executable. They each
   accept the pointer through a different mechanism:

   - Claude takes a [--mcp-config <path>] flag; we write the JSON to
     [~/.config/discord-agents/mcp-generated.json] and pass the path.
   - Codex takes [-c key=value] TOML overrides per invocation; we
     inject [mcp_servers.discord_agents.command/args] without
     touching the user's [~/.codex/config.toml].
   - Gemini has no flag; it loads [mcpServers] from
     [<cwd>/.gemini/settings.json]. We write the file into the
     worktree and add [.gemini/] to [.git/info/exclude]. *)

let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* Executable, not merely present: [Sys.file_exists] is true of
   directories and of a non-executable file, either of which would be
   accepted here and then fail at spawn time inside the agent, where we
   never see it. *)
let is_executable_file path =
  match Unix.stat path with
  | { Unix.st_kind = Unix.S_REG; _ } ->
    (try Unix.access path [Unix.X_OK]; true with Unix.Unix_error _ -> false)
  | _ -> false
  | exception Unix.Unix_error _ -> false

let first_executable paths =
  List.find_opt is_executable_file paths

let rec find_upwards ~from ~relative =
  let candidate = Filename.concat from relative in
  if is_executable_file candidate then Some candidate
  else
    let parent = Filename.dirname from in
    if String.equal parent from then None
    else find_upwards ~from:parent ~relative

(* Resolve the MCP server command, or explain why we couldn't.
   Parameterized rather than reading the environment directly so the
   branches are testable: this is the riskiest logic in the cutover and
   every branch fails in a way nobody sees until an agent silently has
   no tools.

   [Error] carries the candidates tried, for an operator-readable log. *)
type mcp_resolution_failure =
  | Override_not_executable of string
  | No_candidate_found of string list

let resolve_mcp_command ~env_override ~exe_dir ~cwd =
  match env_override with
  | Some path when String.trim path <> "" ->
    (* Trim: the guard used to test the trimmed value and return the
       untrimmed one, so " /usr/bin/foo" passed and then never ran. *)
    let path = String.trim path in
    if is_executable_file path then Ok path
    else Error (Override_not_executable path)
  | _ ->
    let siblings = [
      Filename.concat exe_dir "discord-agents-mcp";
      Filename.concat exe_dir "mcp_server.exe";
    ] in
    (match first_executable siblings with
     | Some path -> Ok path
     | None ->
       let relative = "_build/default/bin/mcp_server.exe" in
       (match find_upwards ~from:cwd ~relative with
        | Some path -> Ok path
        | None ->
          (* Name every directory the walk actually looked in, not just
             the first: "we tried X" listing one of eight paths sends
             the operator looking in the wrong place. *)
          let rec ancestors dir acc =
            let acc = Filename.concat dir relative :: acc in
            let parent = Filename.dirname dir in
            if String.equal parent dir then List.rev acc
            else ancestors parent acc
          in
          Error (No_candidate_found (siblings @ ancestors cwd []))))

(* Last resolution we logged about, so steady state stays quiet while
   every transition gets one line. [None] before the first resolve. *)
let last_logged_mcp_resolution :
  (string, mcp_resolution_failure) result option ref = ref None

let log_mcp_resolution result =
  let same =
    match !last_logged_mcp_resolution, result with
    | Some (Ok previous), Ok current -> String.equal previous current
    | Some (Error (Override_not_executable a)),
      Error (Override_not_executable b) -> String.equal a b
    | Some (Error (No_candidate_found _)), Error (No_candidate_found _) -> true
    | _ -> false
  in
  if not same then begin
    last_logged_mcp_resolution := Some result;
    match result with
    | Ok path -> Logs.info (fun m -> m "MCP server: %s" path)
    | Error (Override_not_executable path) ->
      (* Don't advise `dune build` at someone who told us exactly where
         to look; the problem is that path, not a missing build. *)
      Logs.err (fun m ->
        m "DISCORD_AGENTS_MCP_COMMAND=%s is not an executable file; \
           agents will start with no discord-agents tools." path)
    | Error (No_candidate_found candidates) ->
      Logs.err (fun m ->
        m "MCP server executable not found; agents will start with no \
           discord-agents tools. Tried: %s. Build it with `dune build` \
           (plain `dune exec discord-agents` does not build sibling \
           executables), or set DISCORD_AGENTS_MCP_COMMAND to its path."
          (String.concat ", " candidates))
  end

(* Resolved fresh on every session spawn rather than memoized at boot.
   The path normally lives in _build, so a `dune clean`, a removed
   worktree, or a deleted _build under a running bot would otherwise
   leave a cached [Ok path] naming a command that no longer exists —
   agents spawn with a config pointing at nothing, the failure happens
   inside the agent where we never see it, and none of the degrade
   paths below can fire. It reads the other way too: build the server
   after starting the bot and the next session picks it up, instead of
   the boot-time failure persisting until restart.

   Cost is a handful of [stat] calls per agent spawn — which is per
   turn, not per session, since every message spawns a fresh agent
   process. Still a few local-disk syscalls beside the PATH walk and
   process spawn every turn already pays for. Logging is
   suppressed unless the answer changed, so steady state is silent and
   each transition gets exactly one line. *)
let current_mcp_command () =
  let exe_dir =
    try Sys.executable_name |> absolute_path |> Filename.dirname
    with _ -> Filename.current_dir_name
  in
  let cwd = try Sys.getcwd () with _ -> Filename.current_dir_name in
  let result =
    resolve_mcp_command
      ~env_override:(Sys.getenv_opt "DISCORD_AGENTS_MCP_COMMAND")
      ~exe_dir ~cwd
  in
  log_mcp_resolution result;
  result

let mcp_command_opt () =
  match current_mcp_command () with
  | Ok path -> Some path
  | Error _ -> None

(* The old override named a Python script and is silently ignored after
   the cutover, which lands the operator in the no-tools state above.
   Startup-only: it describes the environment, not the build, so
   repeating it per spawn would be noise. *)
let warn_if_deprecated_mcp_script_env () =
  match Sys.getenv_opt "DISCORD_AGENTS_MCP_SCRIPT" with
  | Some value when String.trim value <> "" ->
    Logs.warn (fun m ->
      m "DISCORD_AGENTS_MCP_SCRIPT=%s is no longer used; the MCP server \
         is now a native executable. Set DISCORD_AGENTS_MCP_COMMAND \
         instead — it accepts scripts/mcp-server.py directly if you \
         need to roll back to the Python server." value)
  | _ -> ()

(* [~command] rather than a second lookup: with resolution no longer
   memoized, a writer that re-resolved could disagree with the check
   that let it run, and the disagreement would be written into a
   config file. Passing the resolved path makes that unrepresentable. *)
let mcp_server_entry ?(args = []) ~command () =
  `Assoc [
    ("command", `String command);
    ("args", `List (List.map (fun a -> `String a) args));
  ]

let mcp_json ?args ~command () =
  Yojson.Safe.to_string
    (`Assoc [
      ("mcpServers", `Assoc [
        ("discord-agents", mcp_server_entry ?args ~command ());
      ]);
    ])

let write_file_safely ~path contents =
  match Disk_health.preflight_write path with
  | Error err ->
    Logs.warn (fun m ->
      m "agent_process: refused write to %s: %s" path err)
  | Ok () ->
    (try
       Resource.with_flock (path ^ ".lock") (fun () ->
         Resource.cleanup_atomic_write_temps path;
         Resource.write_file_atomic path contents);
       Disk_health.note_write_success path
     with exn ->
       Disk_health.note_write_failure path exn;
       Logs.warn (fun m ->
         m "agent_process: failed to write %s: %s"
           path (Printexc.to_string exn)))

type read_file_result =
  | File_missing
  | File_contents of string
  | File_read_error of exn

let read_file_result path =
  try
    if not (Sys.file_exists path) then
      File_missing
    else
      let ic = open_in path in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        let n = in_channel_length ic in
        let s = Bytes.create n in
        really_input ic s 0 n;
        File_contents (Bytes.to_string s))
  with exn ->
    if Sys.file_exists path then File_read_error exn else File_missing

let read_file_opt path =
  match read_file_result path with
  | File_contents contents -> Some contents
  | File_missing
  | File_read_error _ -> None

let log_file_read_failure ~path exn =
  Logs.warn (fun m ->
    m "agent_process: failed to read %s; leaving it unchanged: %s"
      path (Printexc.to_string exn))

let contains_substring text needle =
  let nlen = String.length needle in
  let tlen = String.length text in
  if nlen = 0 then true
  else
    let rec scan i =
      if i + nlen > tlen then false
      else if String.sub text i nlen = needle then true
      else scan (i + 1)
    in scan 0

let error_has_diagnostic_line ~diagnostic error =
  String.split_on_char '\n' error
  |> List.exists (fun line ->
       String.ends_with ~suffix:diagnostic (String.trim line))

let claude_session_id_in_use_error ~session_id error =
  error_has_diagnostic_line error
    ~diagnostic:(Printf.sprintf
      "Error: Session ID %s is already in use." session_id)

let claude_session_id_missing_error ~session_id error =
  error_has_diagnostic_line error
    ~diagnostic:(Printf.sprintf
      "No conversation found with session ID: %s" session_id)

let claude_retry_confirmation
    ~session_id ~session_id_confirmed ~message_count
    ~fork_from_session_id error =
  if Option.is_some fork_from_session_id then
    None
  else if not session_id_confirmed
          && claude_session_id_in_use_error ~session_id error then
    Some true
  else if session_id_confirmed && message_count = 0
          && claude_session_id_missing_error ~session_id error then
    Some false
  else
    None

let run_with_claude_session_recovery
    ~kind ~session_id ~session_id_confirmed ~message_count
    ~fork_from_session_id ~run =
  match run session_id_confirmed with
  | Result.Error error ->
    (match kind with
     | Config.Claude ->
       (match claude_retry_confirmation
                ~session_id ~session_id_confirmed ~message_count
                ~fork_from_session_id error with
        | Some retry_confirmation ->
          let retry_flag =
            if retry_confirmation then "--resume" else "--session-id"
          in
          Logs.warn (fun m ->
            m "agent_process: recovering Claude session id %s; retrying once with %s"
              session_id retry_flag);
          (match run retry_confirmation with
           | Result.Ok () -> Result.Ok ()
           | Result.Error retry_error ->
             Result.Error (Printf.sprintf
               "Claude %s recovery retry failed: %s"
               retry_flag retry_error))
        | None -> Result.Error error)
     | Config.Codex
     | Config.Gemini
     | Config.Agy -> Result.Error error)
  | Result.Ok () -> Result.Ok ()

(** Claude: write the MCP config to a well-known location and return
    the path for [--mcp-config]. *)
let claude_mcp_config_path () =
  match mcp_command_opt () with
  | None -> None
  | Some command ->
    let config_dir = Resource.app_config_dir () in
    let config_path = Filename.concat config_dir "mcp-generated.json" in
    write_file_safely ~path:config_path (mcp_json ~command ());
    (* [write_file_safely] swallows both a disk-preflight refusal and a
       write exception, so "we tried" is not "there is a file". Claude
       does not degrade when --mcp-config names a missing file, it exits
       1 — so promising a path we didn't write would turn a no-tools
       session into no session at all, exactly when read-only-disk
       handling is trying to keep sessions alive.

       Read it back rather than stat it: the write is a rename, so a
       refused write leaves whatever was there before — a config from
       another worktree naming an executable that no longer exists,
       which is the silent-no-tools failure this PR is closing. A
       directory at that path would also satisfy [Sys.file_exists] and
       then make Claude exit 1. *)
    let intended = mcp_json ~command () in
    match read_file_result config_path with
    (* Trimmed, because the comparison is with the file the writer
       produced, not with the string handed to it: write_file_atomic
       appends a newline of its own (lib/resource.ml). Comparing
       untrimmed made this reject every healthy write — the config was
       written correctly and then declared stale, so Claude ran with no
       MCP tools at all. Trim rather than [intended ^ "\n"] so this
       doesn't silently re-break if the writer's framing changes. *)
    | File_contents actual when String.equal (String.trim actual) intended ->
      Some config_path
    | _ ->
      Logs.warn (fun m ->
        m "MCP config %s does not match what we tried to write; \
           starting Claude without --mcp-config rather than pointing it \
           at a stale or missing config" config_path);
      None

(** Inject our [discord-agents] entry into a Gemini settings JSON
    string (preserving any other [mcpServers] and unrelated
    top-level keys the user might have set). Falls back to a fresh
    config when the input is missing, malformed, or parseable but
    not an object — in any of those cases the existing content
    couldn't have meaningfully held our entry, so we'd lose nothing
    by replacing it. *)
(* [~entry:None] withdraws our server instead of registering it, for the
   case where no MCP executable resolved. Gemini is the only agent whose
   config is a file it reads unconditionally on every run — Claude's
   flag and Codex's overrides simply aren't passed — so for Gemini
   "don't write the config" is not the same as "run without it". A
   settings.json written while a build tree existed keeps naming that
   vanished path afterwards. Removing our entry leaves the user's other
   servers and unrelated keys exactly where they were. *)
let gemini_settings_with ~our_entry existing =
  let set_our_server servers =
    let others = List.filter (fun (k, _) -> k <> "discord-agents") servers in
    match our_entry with
    | Some entry -> ("discord-agents", entry) :: others
    | None -> others
  in
  let render_fresh () =
    Yojson.Safe.pretty_to_string
      (`Assoc [("mcpServers", `Assoc (set_our_server []))])
  in
  match existing with
  | None -> render_fresh ()
  | Some text ->
    try
      match Yojson.Safe.from_string text with
      | `Assoc fields ->
        let prior_servers = match List.assoc_opt "mcpServers" fields with
          | Some (`Assoc servers) -> servers
          | _ -> [] in
        let new_fields =
          ("mcpServers", `Assoc (set_our_server prior_servers))
          :: List.filter (fun (k, _) -> k <> "mcpServers") fields
        in
        Yojson.Safe.pretty_to_string (`Assoc new_fields)
      | _ -> render_fresh ()  (* parseable but non-object: discard *)
    with _ -> render_fresh ()

let merge_gemini_settings ?args ~command existing =
  gemini_settings_with ~our_entry:(Some (mcp_server_entry ?args ~command ()))
    existing

(* [None] means "leave the file exactly as it is".

   The register path's "parseable but non-object: discard" reasoning
   does not transfer here. There we *must* install our entry, so
   replacing a file that could never have held it costs nothing. Here
   there is nothing to install, so rewriting a file we can't understand
   is pure loss — a truncated settings.json, one whose [mcpServers]
   isn't an object, or one that simply never held our entry would come
   back as [{"mcpServers": {}}] with the user's keys gone. We only
   rewrite when we can see our own entry to remove.

   Note Yojson does accept [//] comments, so a hand-commented file that
   *does* hold our entry is rewritten (and its comments dropped) rather
   than left alone. That is the register path's behavior too. *)
let gemini_settings_without_our_entry existing =
  match existing with
  | None -> None
  | Some text ->
    match Yojson.Safe.from_string text with
    | exception _ -> None
    | `Assoc fields ->
      (match List.filter (fun (key, _) -> key = "mcpServers") fields with
       (* Exactly one [mcpServers], and it holds our entry. Duplicate
          top-level keys are legal JSON that Yojson keeps and that
          different readers resolve differently — Gemini's parser takes
          the last, [List.assoc_opt] the first — and rewriting collapses
          them to one, dropping whichever block we didn't act on. There
          is no rewrite here that can't lose something, so decline. *)
       | [(_, `Assoc servers)] when List.mem_assoc "discord-agents" servers ->
         Some (gemini_settings_with ~our_entry:None (Some text))
       | _ :: _ :: _ ->
         Logs.warn (fun m ->
           m "Gemini settings has duplicate mcpServers keys; declining to \
              rewrite rather than collapsing them");
         None
       | _ -> None)
    | _ -> None

(** Resolve the [info/exclude] file Git actually reads for
    [working_dir]. Uses [git rev-parse --git-path info/exclude],
    which Does The Right Thing in both regular checkouts and linked
    worktrees: the previous version used [--git-dir] and concatenated
    [info/exclude], which in worktrees pointed at the per-worktree
    gitdir's [info/exclude] — a file Git never reads, so the append
    silently no-op'd from Git's perspective. Returns None if
    [working_dir] isn't a git repo. *)
let resolve_git_info_exclude ~working_dir =
  if working_dir = "" then None
  else
    try
      let cmd = Printf.sprintf
        "git -C %s rev-parse --git-path info/exclude 2>/dev/null"
        (Filename.quote working_dir) in
      let ic = Unix.open_process_in cmd in
      let line = try input_line ic with End_of_file -> "" in
      let _ = Unix.close_process_in ic in
      let trimmed = String.trim line in
      if trimmed = "" then None
      else if Filename.is_relative trimmed then
        Some (Filename.concat working_dir trimmed)
      else
        Some trimmed
    with _ -> None

(** True if [text] already contains a line that is exactly the
    pattern [.gemini/] (or [.gemini]) — ignoring leading/trailing
    whitespace, but rejecting commented (`# .gemini/`) and negated
    (`!.gemini/`) lines that look similar but don't actually
    exclude the directory. *)
let exclude_already_lists_gemini text =
  String.split_on_char '\n' text
  |> List.exists (fun line ->
    let trimmed = String.trim line in
    trimmed = ".gemini/" || trimmed = ".gemini")

let merge_gemini_exclude existing =
  match existing with
  | Some text when exclude_already_lists_gemini text -> text
  | existing ->
    let text = Option.value existing ~default:"" in
    let prefix =
      if text = "" || String.ends_with ~suffix:"\n" text then text
      else text ^ "\n"
    in
    prefix ^ ".gemini/\n"

(** Gemini: drop our entry into [.gemini/settings.json] (merging into
    any pre-existing user config) and ensure [.gemini/] is in the
    repo's [info/exclude]. Both writes are idempotent — re-running
    against a worktree we've already configured leaves the file and
    the exclude line unchanged in shape. *)
(* [?resolved] exists so the unresolved branch is reachable from a test
   without arranging for the real resolution to fail. It also pins the
   answer for the duration of one call: resolving once here and using
   that value throughout means the withdraw/register decision and the
   command written into the file can't come from two different lookups. *)
let setup_gemini_mcp ?resolved ~working_dir () =
  let resolved =
    match resolved with Some r -> r | None -> mcp_command_opt ()
  in
  if working_dir = "" then ()
    (* Defense-in-depth: the resume handlers reject empty working_dir
       before we get here, but a future caller shouldn't write
       .gemini/ into the bot's own directory by accident. *)
  else
  let gemini_dir = Filename.concat working_dir ".gemini" in
  let settings_path = Filename.concat gemini_dir "settings.json" in
  (* Unresolved: withdraw our entry rather than leave one naming a
     command that is no longer there. Control and project channel
     sessions run in persistent directories, not throwaway worktrees,
     so a settings.json written while a build tree existed outlives it.
     But touch nothing else — no .gemini/ to create, no info/exclude
     line to add, and no rewrite of a file that doesn't contain our
     entry. With no server to register, every one of those would be a
     change made on a user's behalf for no benefit. *)
  match resolved with
  | None ->
    (match read_file_result settings_path with
     | File_read_error exn -> log_file_read_failure ~path:settings_path exn
     | File_missing -> ()
     | File_contents existing ->
       (match gemini_settings_without_our_entry (Some existing) with
        | None -> ()
        | Some updated -> write_file_safely ~path:settings_path updated))
  | Some command ->
  (try
     if not (Sys.file_exists gemini_dir) then Unix.mkdir gemini_dir 0o755
   with _ -> ());
  (match read_file_result settings_path with
   | File_read_error exn -> log_file_read_failure ~path:settings_path exn
   | File_missing ->
     write_file_safely ~path:settings_path
       (merge_gemini_settings ~command None)
   | File_contents existing ->
     write_file_safely ~path:settings_path
       (merge_gemini_settings ~command (Some existing)));
  match resolve_git_info_exclude ~working_dir with
  | None -> ()
  | Some exclude_path ->
    match read_file_result exclude_path with
    | File_read_error exn -> log_file_read_failure ~path:exclude_path exn
    | File_contents existing when exclude_already_lists_gemini existing -> ()
    | File_contents existing ->
      write_file_safely ~path:exclude_path (merge_gemini_exclude (Some existing))
    | File_missing ->
      write_file_safely ~path:exclude_path (merge_gemini_exclude None)

(* ── agy MCP (user-global config) ───────────────────────────────────

   agy 1.1.3 loads MCP servers in print mode only from the user-global
   [~/.gemini/config/mcp_config.json]; workspace [.agents/] configs are
   ignored (measured; see docs/agy-harness.md). That file is the user's
   own, shared with their interactive agy and the Antigravity IDE, so
   the entry we keep there is static and inert outside the bot. It runs
   the server named by [DISCORD_AGENTS_MCP_FOR_AGY], which only the
   bot's agy children carry, and exits at once without it. agy tolerates
   a server that exits like that (verified: the turn succeeds).

   The user's file is never damaged to make room for us. We decline,
   and the session runs without MCP, when the file:
   - is a symlink (managed elsewhere, e.g. dotfiles);
   - won't parse, isn't an object, or has a non-object [mcpServers];
   - has duplicate [mcpServers] keys or duplicate entries under our name;
   - has comments, which Yojson accepts but would drop on rewrite;
   - would not re-serialise as standard JSON (e.g. 1e400);
   - has an entry under our name that isn't ours, or ours marked
     [disabled: true] by the user.
   An entry is ours when its command and args match. Extra keys the
   user or IDE adds to it are kept, and their presence doesn't trigger
   a rewrite.

   Writes follow the dotfiles installer for the sibling config.json:
   under a bot-owned lock, keep the raw bytes, rebuild, and re-read just
   before the rename, refusing if anything changed in the meantime. agy
   and the IDE don't take our lock, so a change landing in the last
   instant before the rename can still be lost. That window is
   microseconds, against one write per change of our entry. The
   original file mode is kept. *)

let agy_mcp_env_var = "DISCORD_AGENTS_MCP_FOR_AGY"
let agy_mcp_server_name = "discord_agents"

let agy_mcp_command_json = `String "/bin/sh"
let agy_mcp_args_json =
  `List [
    `String "-c";
    `String (Printf.sprintf
      "[ -n \"$%s\" ] || exit 0; exec \"$%s\"" agy_mcp_env_var agy_mcp_env_var);
    `String "discord-agents-mcp";
  ]

let agy_mcp_entry =
  `Assoc [("command", agy_mcp_command_json); ("args", agy_mcp_args_json)]

type agy_mcp_update =
  | Agy_mcp_present          (** our entry is already there, as is *)
  | Agy_mcp_write of string  (** write this to install it *)
  | Agy_mcp_declined of string  (** leave the file alone; why *)

(** Whether [text] has a [//] or [/*] outside a string literal. *)
let json_has_comment text =
  let n = String.length text in
  let rec go i in_string =
    if i >= n then false
    else
      match text.[i], in_string with
      | '\\', true -> go (i + 2) true
      | '"', _ -> go (i + 1) (not in_string)
      | '/', false when i + 1 < n && (text.[i+1] = '/' || text.[i+1] = '*') -> true
      | _ -> go (i + 1) in_string
  in
  go 0 false

let count_key k fields = List.length (List.filter (fun (k', _) -> k' = k) fields)

(** Yojson reads 1e400 as infinity and would write it back as a token
    that isn't JSON at all. *)
let rec json_has_nonfinite : Yojson.Safe.t -> bool = function
  | `Float f -> not (Float.is_finite f)
  | `Assoc kvs -> List.exists (fun (_, v) -> json_has_nonfinite v) kvs
  | `List vs -> List.exists json_has_nonfinite vs
  | _ -> false

(** Pure: what to do with the config file's current contents ([None]
    when it doesn't exist). *)
let agy_mcp_config_update existing =
  let render fields =
    if json_has_nonfinite (`Assoc fields) then
      Agy_mcp_declined "would not re-serialise as standard JSON"
    else
      match Yojson.Safe.pretty_to_string ~std:true (`Assoc fields) with
      | text -> Agy_mcp_write (text ^ "\n")
      | exception _ -> Agy_mcp_declined "would not re-serialise as standard JSON"
  in
  let fresh_servers = [("mcpServers", `Assoc [(agy_mcp_server_name, agy_mcp_entry)])] in
  match existing with
  | None -> render fresh_servers
  | Some text when String.trim text = "" -> render fresh_servers
  | Some text when json_has_comment text ->
    Agy_mcp_declined "has comments, which a rewrite would drop"
  | Some text ->
    match Yojson.Safe.from_string text with
    | exception _ -> Agy_mcp_declined "not valid JSON"
    | `Assoc fields when count_key "mcpServers" fields > 1 ->
      Agy_mcp_declined "has more than one mcpServers key"
    | `Assoc fields ->
      let with_servers servers =
        if List.mem_assoc "mcpServers" fields then
          List.map (fun (k, v) ->
            if k = "mcpServers" then (k, `Assoc servers) else (k, v)) fields
        else fields @ [("mcpServers", `Assoc servers)]
      in
      (match List.assoc_opt "mcpServers" fields with
       | Some (`Assoc servers) when count_key agy_mcp_server_name servers > 1 ->
         Agy_mcp_declined "has more than one discord_agents entry"
       | Some (`Assoc servers) ->
         (match List.assoc_opt agy_mcp_server_name servers with
          | None -> render (with_servers (servers @ [(agy_mcp_server_name, agy_mcp_entry)]))
          | Some (`Assoc entry) ->
            let field k = List.assoc_opt k entry in
            let ours =
              field "command" = Some agy_mcp_command_json
              && field "args" = Some agy_mcp_args_json in
            (match field "disabled" with
             | Some (`Bool true) ->
               Agy_mcp_declined "the discord_agents entry is disabled there"
             | _ when ours -> Agy_mcp_present
             | _ ->
               (* An older entry of ours mentions our variable; anything
                  else under this name is somebody else's server. *)
               let mentions_our_var =
                 match field "args" with
                 | Some (`List args) ->
                   List.exists (function
                     | `String a -> contains_substring a agy_mcp_env_var
                     | _ -> false) args
                 | _ -> false
               in
               if not mentions_our_var then
                 Agy_mcp_declined "a different discord_agents server is configured there"
               else
                 let entry' =
                   List.filter (fun (k, _) -> k <> "command" && k <> "args") entry
                   @ [("command", agy_mcp_command_json); ("args", agy_mcp_args_json)] in
                 render (with_servers (List.map (fun (k, v) ->
                   if k = agy_mcp_server_name then (k, `Assoc entry') else (k, v)) servers)))
          | Some _ -> Agy_mcp_declined "the discord_agents entry is not an object")
       | None -> render (with_servers [(agy_mcp_server_name, agy_mcp_entry)])
       | Some _ -> Agy_mcp_declined "mcpServers is not an object")
    | _ -> Agy_mcp_declined "not a JSON object"

let agy_mcp_config_path () =
  match Sys.getenv_opt "HOME" with
  | Some home when home <> "" ->
    Some (Filename.concat home ".gemini/config/mcp_config.json")
  | _ -> None

let agy_mcp_lock_path () =
  Filename.concat (Resource.app_config_dir ()) "agy-mcp-config.lock"

type agy_mcp_file =
  | Agy_file_missing
  | Agy_file_symlink
  | Agy_file of { raw : string; perm : int }

let read_agy_mcp_file path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Result.Ok Agy_file_missing
  | exception exn -> Result.Error exn
  | { Unix.st_kind = Unix.S_LNK; _ } -> Result.Ok Agy_file_symlink
  | { Unix.st_kind = Unix.S_REG; st_perm; _ } ->
    (match In_channel.with_open_bin path In_channel.input_all with
     | raw -> Result.Ok (Agy_file { raw; perm = st_perm })
     | exception exn -> Result.Error exn)
  | _ -> Result.Error (Failure "not a regular file")

(* Replace [path] with [contents] if it still holds [expected] (raw bytes;
   [None] = must still be absent). Same-directory temp + rename, mode
   [perm]. *)
let replace_agy_mcp_file ~path ~expected ~perm contents =
  let dir = Filename.dirname path in
  let tmp = Filename.temp_file ~temp_dir:dir (Filename.basename path ^ ".tmp.") "" in
  let cleanup () = try Sys.remove tmp with Sys_error _ -> () in
  match
    Out_channel.with_open_bin tmp (fun oc -> output_string oc contents);
    Unix.chmod tmp perm;
    let still = match read_agy_mcp_file path, expected with
      | Result.Ok Agy_file_missing, None -> true
      | Result.Ok (Agy_file { raw; _ }), Some e -> String.equal raw e
      | _ -> false
    in
    if still then (Unix.rename tmp path; true) else (cleanup (); false)
  with
  | ok -> ok
  | exception exn -> cleanup (); raise exn

let agy_mcp_reported = ref ""
(** Log a reason once per distinct reason, not on every spawn. *)
let report_agy_mcp_unavailable why =
  if !agy_mcp_reported <> why then begin
    agy_mcp_reported := why;
    Logs.warn (fun m ->
      m "agent_process: agy sessions run without discord-agents MCP tools: %s" why)
  end

(** Make sure our inert entry is in agy's global MCP config. Returns
    whether it is there afterwards. Blocking (file lock, fsync-free
    writes): call it off the Eio domain. The config directory has to
    exist already: it belongs to agy, and creating it would be making
    agy's state on the user's behalf. *)
let setup_agy_mcp ?path ?lock_path () =
  match (match path with Some p -> Some p | None -> agy_mcp_config_path ()) with
  | None -> report_agy_mcp_unavailable "HOME is not set"; false
  | Some path ->
    if not (try Sys.is_directory (Filename.dirname path) with Sys_error _ -> false)
    then begin
      report_agy_mcp_unavailable
        (Printf.sprintf "%s does not exist (run agy once to create it)"
           (Filename.dirname path));
      false
    end else
      let decline why =
        report_agy_mcp_unavailable (Printf.sprintf "left %s alone: %s" path why);
        `Declined
      in
      let decide = function
        | Result.Error exn -> decline ("cannot read it: " ^ Printexc.to_string exn)
        | Result.Ok Agy_file_symlink -> decline "it is a symlink"
        | Result.Ok file ->
          let raw = match file with Agy_file { raw; _ } -> Some raw | _ -> None in
          match agy_mcp_config_update raw with
          | Agy_mcp_present -> `Present
          | Agy_mcp_declined why -> decline why
          | Agy_mcp_write contents -> `Write (raw, file, contents)
      in
      (* The common case (entry already there) takes no lock. *)
      match decide (read_agy_mcp_file path) with
      | `Present -> true
      | `Declined -> false
      | `Write _ ->
        let lock_path = Option.value lock_path ~default:(agy_mcp_lock_path ()) in
        (try
           Resource.with_flock lock_path (fun () ->
             match decide (read_agy_mcp_file path) with
             | `Present -> true
             | `Declined -> false
             | `Write (expected, file, contents) ->
               let perm = match file with Agy_file { perm; _ } -> perm | _ -> 0o644 in
               replace_agy_mcp_file ~path ~expected ~perm contents
               || (ignore (decline "it changed while we were updating it; will retry next turn");
                   false))
         with exn ->
           ignore (decline ("could not update it: " ^ Printexc.to_string exn));
           false)

(** Escape a string for embedding inside a TOML double-quoted string.
    Beyond backslash and double-quote (the cases we actually hit with
    filesystem paths), the TOML spec also requires control characters
    [U+0000..U+001F] and [U+007F] to be escaped — these would never
    appear in a real path but we round-trip them faithfully so this
    helper is correct in isolation. *)
let escape_toml_string s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
    let code = Char.code c in
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"'  -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | _ when code < 0x20 || code = 0x7F ->
      Buffer.add_string buf (Printf.sprintf "\\u%04X" code)
    | _ -> Buffer.add_char buf c) s;
  Buffer.contents buf

(** TOML overrides that register the discord-agents MCP server for a
    Codex invocation, without touching the user's ~/.codex/config.toml.
    Two key=value pairs because Codex parses each [-c] independently. *)
(* Codex starts stdio MCP servers with an allowlisted environment
   (HOME, PATH and a few more), so the caller's thread id, which the
   server reads from DISCORD_AGENTS_THREAD_ID, has to be put in the
   server's own env table. -c overrides apply to this run only. *)
let codex_mcp_overrides_for ?(args = []) ?thread_id ~command () =
  let toml_string s = "\"" ^ escape_toml_string s ^ "\"" in
  [ "-c"; "mcp_servers.discord_agents.command=" ^ toml_string command;
    "-c"; "mcp_servers.discord_agents.args=["
          ^ String.concat ", " (List.map toml_string args) ^ "]" ]
  @ (match thread_id with
     | Some id ->
       ["-c"; "mcp_servers.discord_agents.env.DISCORD_AGENTS_THREAD_ID="
              ^ toml_string id]
     | None -> [])

let codex_mcp_overrides ?thread_id () =
  match mcp_command_opt () with
  | None -> []
  | Some path -> codex_mcp_overrides_for ?thread_id ~command:path ()

(** Codex allocates its session id server-side in the thread.started
    event, so the UUID the bot pre-generated is invalid for resume on
    the first run. [parse_codex_json_line] emits the real id via
    Result.session_id; the agent_runner forwards it through
    [on_session_id], and bot.ml persists it together with
    session_id_confirmed=true.

    Subsequent runs resume only if [session_id_confirmed] — gating on
    message_count would resume with the placeholder UUID after a
    first-turn failure that happened before thread.started, or skip
    resume after a first-turn failure that happened after it. The
    "--" separator prevents prompts that happen to begin with "-"
    from being mistaken for a Codex flag.

    [--dangerously-bypass-approvals-and-sandbox] disables both the
    approval prompts (which a non-interactive subprocess can't
    answer anyway) and Codex's workspace-write filesystem sandbox.
    The lighter-weight [--full-auto] only handles the approval
    half. We pick the heavier flag because the bot's stated trust
    model (README.md "no sandboxing in discord-agents itself") is
    that the user runs it on a personal machine and Codex's sandbox
    has historically blocked legitimate edits inside the worktree.
    See issue #35 for making this configurable. *)
let codex_reasoning_effort_overrides = function
  | Some Config.Max ->
    (* Callers reject this combination through Config, but keep the
       process boundary defensive so persisted bad state cannot pass an
       unsupported Codex override. *)
    []
  | Some effort ->
    ["-c"; Printf.sprintf "model_reasoning_effort=\"%s\""
       (Config.string_of_reasoning_effort effort)]
  | None -> []

let codex_args ?mcp_overrides ~model ~reasoning_effort
    ~session_id ~session_id_confirmed ~prompt () =
  let mcp_overrides =
    match mcp_overrides with Some o -> o | None -> codex_mcp_overrides () in
  let base = ["codex"; "exec"; "--json";
              "--dangerously-bypass-approvals-and-sandbox";
              "--skip-git-repo-check"]
             @ model_arg model
             @ mcp_overrides in
  let base = base @ codex_reasoning_effort_overrides reasoning_effort in
  if not session_id_confirmed then base @ ["--"; prompt]
  else base @ ["resume"; session_id; "--"; prompt]

(** Like Codex, Gemini allocates its session id server-side in the
    [init] event and we capture it from the parser's first emitted
    Result. Subsequent runs resume only when [session_id_confirmed],
    matching the same coherence contract Codex uses.

    [--skip-trust] is required for bot-created worktrees that Gemini
    has not seen interactively before; without it, headless runs exit
    before emitting stream-json events. [--yolo] auto-approves tool
    calls so Gemini doesn't block on an interactive approval prompt
    that the non-interactive subprocess can't answer. It is *not* a
    full equivalent of Codex's
    [--dangerously-bypass-approvals-and-sandbox]: that flag also
    drops Codex's filesystem sandbox, while Gemini's [--yolo] only
    covers approvals. The closer Codex analogue for [--yolo] is
    [--full-auto].

    Gemini's MCP server config is written into [.gemini/settings.json]
    by [setup_gemini_mcp], which run_streaming calls before invoking
    [gemini_args]. *)
let gemini_args ~model ~session_id ~session_id_confirmed ~prompt =
  let base = ["gemini"; "--skip-trust"] @ model_arg model
             @ ["-p"; prompt; "-o"; "stream-json"; "--yolo"] in
  if not session_id_confirmed then base
  else base @ ["--resume"; session_id]

(** agy's print mode gives up after [--print-timeout], 5 minutes by
    default, which real agent turns routinely exceed. Pass a ceiling
    long enough that only a genuinely stuck run hits it; the bot's own
    stop/interrupt paths remain the normal way to end a turn. *)
let agy_print_timeout_seconds = 12 * 60 * 60
let agy_print_timeout = Printf.sprintf "%ds" agy_print_timeout_seconds

(** agy allocates its conversation id server-side and reports it in
    the terminal [result] event; we resume with [--conversation] only
    once that id is confirmed, the same coherence contract as Codex
    and Gemini. [--dangerously-skip-permissions] is required because
    headless print mode auto-denies any tool that would need approval;
    it is the agy counterpart of Codex's
    [--dangerously-bypass-approvals-and-sandbox] under the bot's
    single-tenant trust model.

    [--add-dir <working_dir>] is what makes the worktree agy's
    workspace. With only the cwd set, agy 1.1.3 reports "No workspace is
    open" and points its file tools at its own scratch directory
    (verified). An empty [working_dir] is never passed: the resume
    handlers reject it, and agy would read the flag's value as missing.

    MCP comes from the user-global config; see [setup_agy_mcp]. *)
let agy_args ~model ~working_dir ~session_id ~session_id_confirmed ~prompt =
  let base = ["agy"; "-p"; prompt; "--output-format"; "stream-json";
              "--dangerously-skip-permissions";
              "--print-timeout"; agy_print_timeout]
             @ (if working_dir = "" then [] else ["--add-dir"; working_dir])
             @ model_arg model in
  if not session_id_confirmed then base
  else base @ ["--conversation"; session_id]

let executable_in_path = Resource.executable_in_path

let setsid_command () =
  match executable_in_path "setsid" with
  | Some path -> path
  | None ->
    failwith
      "agent_process: required command `setsid` was not found in PATH; install util-linux or start discord-agents with setsid available"

let session_thread_id_env = "DISCORD_AGENTS_THREAD_ID"

(** Build the child environment with caller identity supplied by the bot.
    Remove any inherited value first so restarting the bot from inside an
    agent session cannot leak that session's thread id into later children.
    The writer override is removed too: it is the bot's own startup
    decision, and an agent that launches a bot (scripts/run-branch.sh)
    must face the writer guard rather than inherit a bypass. *)
let child_environment ?(environment = Unix.environment ()) ~thread_id () =
  let stripped = [session_thread_id_env; Host.force_writer_env; agy_mcp_env_var] in
  let inherited =
    environment
    |> Array.to_list
    |> List.filter (fun entry ->
      not (List.exists (fun name ->
        String.starts_with ~prefix:(name ^ "=") entry) stripped))
  in
  Array.of_list
    (inherited @ [Printf.sprintf "%s=%s" session_thread_id_env thread_id])

(** Where an agent turn runs. [Remote] carries what the run needs:
    [destination] for ssh, [host] for messages, and [writer_ssh], the
    destination the executor uses to reach this bot for MCP. *)
type target =
  | Local
  | Remote of { host : string; destination : string; writer_ssh : string }

(** The MCP server command for an agent on an executor: ssh back to the
    writer and run the writer's own MCP server there, so executors need
    nothing installed and can never run a stale server.

    The caller's thread id is not baked in. As on the local path, the
    agent carries it in [DISCORD_AGENTS_THREAD_ID], and this command
    forwards it at run time. So the config is the same for every thread
    and a stale or shared copy (Gemini's lives in the worktree) can never
    route tool calls to another thread. The forwarded value must be all
    digits (a Discord id), which is what lets it go unquoted into the
    command line that the writer's login shell parses; the server path
    is passed pre-quoted for that shell as a positional parameter, so
    this [sh] never re-parses it.

    [config_home] is the bot's [XDG_CONFIG_HOME] equivalent (the parent
    of its app config dir). The server finds the bot's control socket
    from it; over ssh it would otherwise see the writer's login
    environment and could reach another bot instance's socket. Note the
    writer's login shell must not print on stdout for non-interactive
    shells, or it corrupts the MCP stream. *)
let remote_mcp_command ~writer_ssh ~mcp_path ~config_home =
  let script =
    Printf.sprintf
      {|case "${%s:-}" in ''|*[!0-9]*) echo "discord-agents-mcp: %s must be a Discord id" >&2; exit 1;; esac; exec ssh %s -- "$1" "env %s=$%s XDG_CONFIG_HOME=$3 $2"|}
      session_thread_id_env session_thread_id_env
      (String.concat " " Host_exec.ssh_options)
      session_thread_id_env session_thread_id_env
  in
  ("sh", ["-c"; script; "discord-agents-mcp"; writer_ssh;
          Remote_script.quote mcp_path; Remote_script.quote config_home])

let is_discord_id s = s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

(* Upper bound on one setup command (reading or writing a small file on
   an executor). It keeps a hung executor from holding a turn, and a stop
   requested meanwhile, indefinitely. *)
let remote_setup_timeout_s = 60

(** Register the MCP server in [working_dir]/.gemini/settings.json on an
    executor, with the same merge as [setup_gemini_mcp] (other servers and
    keys are kept), and keep [.gemini/] out of git there. With no server
    to register ([mcp = None]), withdraw our entry and change nothing
    else, as locally. Writes go to a per-process temp file renamed into
    place, so an interrupted or concurrent write never leaves a partial
    file. Failures are logged, not raised: as locally, Gemini then runs
    without bot tools rather than not at all, and the entry carries no
    thread id, so a stale one cannot misroute. Blocking; call from a
    system thread. *)
let setup_gemini_mcp_remote ?ssh ~(host : Host.t) ~working_dir ~mcp () =
  let run ?input argv =
    Host_exec.run_capture ?ssh ~timeout_s:remote_setup_timeout_s
      ~cwd:working_dir ?input host argv in
  let warn what f =
    Logs.warn (fun m -> m "agent_process: %s on %s failed: %s" what host.name
      (Host_exec.describe_failure ~host:host.name f
       ^ ": " ^ Host_exec.failure_detail f))
  in
  let settings = ".gemini/settings.json" in
  (* Present: Ok (Some contents); absent: Ok None; unreadable: Error. *)
  let read_optional path =
    match run ["test"; "-e"; path] with
    | Error (Host_exec.Exited (1, _)) -> Ok None
    | Error f -> Error f
    | Ok _ -> Result.map Option.some (run ["cat"; "--"; path])
  in
  let existing = read_optional settings in
  (* An unreachable host fails every later step too, each paying the
     connect timeout; the agent spawn that follows reports it. *)
  match existing with
  | Error (Host_exec.Unreachable _ as f) -> warn "reaching the host" f
  | _ ->
  let write_settings contents =
    match run ~input:contents
            ["sh"; "-c";
             "mkdir -p .gemini && cat > \".gemini/settings.json.tmp.$$\" \
              && mv -f \".gemini/settings.json.tmp.$$\" .gemini/settings.json"]
    with
    | Ok _ -> ()
    | Error f -> warn "writing .gemini/settings.json" f
  in
  match mcp with
  | None ->
    (match existing with
     | Error f -> warn "reading .gemini/settings.json" f
     | Ok existing ->
       Option.iter write_settings (gemini_settings_without_our_entry existing))
  | Some (command, args) ->
  (match existing with
   | Error f -> warn "reading .gemini/settings.json" f
   | Ok existing -> write_settings (merge_gemini_settings ~args ~command existing));
  (* stderr dropped: capture merges it into the output, and a git
     warning would otherwise become part of the path. *)
  match run ["sh"; "-c"; "git rev-parse --git-path info/exclude 2>/dev/null"] with
  | Error _ -> ()  (* not a git checkout: nothing to exclude from *)
  | Ok path ->
    let path = String.trim path in
    (* As locally: an exclude file we cannot read is left alone, never
       replaced by one holding only our line. *)
    match read_optional path with
    | Error f -> warn "reading .git/info/exclude" f
    | Ok (Some text) when exclude_already_lists_gemini text -> ()
    | Ok current ->
      match run ~input:(merge_gemini_exclude current)
              ["sh"; "-c";
               "mkdir -p \"$(dirname \"$1\")\" && cat > \"$1.tmp.$$\" \
                && mv -f \"$1.tmp.$$\" \"$1\"";
               "_"; path] with
      | Ok _ -> ()
      | Error f -> warn "updating .git/info/exclude" f

(** A failed remote turn, worded like the local one ("agent exited with
    code N: <stderr>") so callers that match on the agent's own stderr,
    such as Claude session recovery, work unchanged; other failures name
    the host and stage. *)
let remote_failure_message ~host = function
  | Host_exec.Exited (code, stderr) ->
    Printf.sprintf "agent exited with code %d on %s: %s" code host stderr
  | Host_exec.Transport_lost detail ->
    (* Also what a deliberate stop looks like from here. *)
    Printf.sprintf "the connection to %s ended (stopped, or the connection \
                    dropped)%s" host (if detail = "" then "" else ": " ^ detail)
  | failure ->
    let detail = Host_exec.failure_detail failure in
    Host_exec.describe_failure ~host failure
    ^ (if detail = "" then "" else ": " ^ detail)

let escape_prompt_block text =
  let b = Buffer.create (String.length text) in
  String.iter (function
    | '&' -> Buffer.add_string b "&amp;"
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | c -> Buffer.add_char b c
  ) text;
  Buffer.contents b

let agy_no_mcp_notice =
  "NOTE: this session runs on agy, and this turn is not connected to the \
   discord-agents MCP server. None of the MCP tools listed above are \
   available now. Do not attempt them or report their results; tell the \
   user what you would have done and suggest a Claude or Codex session \
   for orchestration. Later turns say whether the tools are available."

(** agy decides MCP per turn (the server or config can come and go), but
    the first-turn notice lives on in the conversation. So later turns
    of a session that was told about the tools restate their state. *)
let agy_mcp_status_line ~available =
  if available then "discord-agents MCP tools are available this turn."
  else "discord-agents MCP tools are NOT available this turn; do not attempt them."

(** Compose the per-turn prompt sent to a non-Claude agent, prepending
    the session's system prompt on the first turn so MCP-aware agents
    know what tools they have. Claude takes [--append-system-prompt]
    via run_streaming and skips this. The conversation history carries
    the system prompt forward on subsequent turns, so we only inject
    it when [message_count = 0].

    Pure function so it's testable without a subprocess. *)
let compose_session_prompt ?(agy_mcp = false) ~agent_kind ~system_prompt
    ~message_count ~goal_context ~user_prompt () =
  let needs_inline =
    message_count = 0
    && (match agent_kind with
        | Config.Codex | Config.Gemini | Config.Agy -> true
        | Config.Claude -> false)
  in
  let user_prompt = match goal_context, agent_kind with
    | Some goal, Config.Codex ->
      Printf.sprintf "<codex-goal>\n%s\n</codex-goal>\n\n%s"
        (escape_prompt_block goal) user_prompt
    | _ -> user_prompt
  in
  (* When agy runs without the discord-agents MCP server (remote turn,
     no server resolved, or a global config we wouldn't touch), the
     channel system prompts still list those tools. Say so, or a control
     agent under agy would try tools it doesn't have. *)
  let system_prompt = match system_prompt, agent_kind with
    | Some sp, Config.Agy when not agy_mcp ->
      Some (sp ^ "\n\n" ^ agy_no_mcp_notice)
    | _ -> system_prompt
  in
  match system_prompt with
  | Some sp when needs_inline ->
    Printf.sprintf "<bot-context>\n%s\n</bot-context>\n\n%s" sp user_prompt
  | Some _ when agent_kind = Config.Agy ->
    Printf.sprintf "<bot-context>%s</bot-context>\n\n%s"
      (agy_mcp_status_line ~available:agy_mcp) user_prompt
  | _ -> user_prompt

(** Spawn an agent and stream its output via a callback.
    The callback is called with each parsed event as it arrives.
    [?on_pid] is called with the child PID immediately after spawn,
    so the caller can track active subprocesses for cleanup.
    Returns when the process exits. *)
let run_streaming ~sw ~env ?(target = Local) ?ssh
    ~working_dir ~kind ~session_id ~thread_id ~message_count
    ?fork_from_session_id ?(session_id_confirmed=false) ?system_prompt
    ?(model=None) ?(reasoning_effort=None) ?(goal_context=None)
    ~prompt ~on_event ?on_pid () =
  let mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Path.(fs / working_dir) in
  let child_env = child_environment ~thread_id () in
  (* agy reaches the MCP server through its global config entry, which
     runs whatever [agy_mcp_env_var] names. Remote turns don't get it
     yet: the executor's agy has its own config and no route back. *)
  let agy_mcp_command =
    match kind, target with
    | Config.Agy, Local ->
      (match mcp_command_opt () with
       | Some command
         when Eio_unix.run_in_systhread (fun () -> setup_agy_mcp ()) ->
         Some command
       | _ -> None)
    | Config.Agy, Remote _
    | (Config.Claude | Config.Codex | Config.Gemini), _ -> None
  in
  let child_env = match agy_mcp_command with
    | Some command ->
      Array.append child_env [| Printf.sprintf "%s=%s" agy_mcp_env_var command |]
    | None -> child_env
  in
  (* Codex/Gemini/agy get the system prompt prepended to the first user
     turn (compose_session_prompt); Claude gets it via the dedicated
     [--append-system-prompt] flag instead. *)
  let prompt = compose_session_prompt
    ~agy_mcp:(Option.is_some agy_mcp_command)
    ~agent_kind:kind ~system_prompt ~message_count ~goal_context
    ~user_prompt:prompt () in
  let close_noerr flow =
    try Eio.Resource.close flow with _ -> ()
  in
  (* For a remote turn: the ssh command that reaches this bot's MCP server,
     or None when no server resolves (as locally, run without tools). *)
  let remote_mcp =
    match target with
    | Local -> None
    | Remote { writer_ssh; _ } ->
      Option.map (fun mcp_path ->
        remote_mcp_command ~writer_ssh ~mcp_path
          ~config_home:(Filename.dirname (Resource.app_config_dir ())))
        (mcp_command_opt ())
  in
  let claude_mcp_arg = match kind, target with
    | Config.Claude, Local ->
      (* No resolvable server: start without the flag rather than
         point Claude at a config naming a command that isn't there.
         The error is already logged once at resolution. *)
      (match claude_mcp_config_path () with
       | Some path -> ["--mcp-config"; path]
       | None -> [])
    | Config.Claude, Remote _ ->
      (* Inline JSON: nothing to write on the executor. *)
      (match remote_mcp with
       | Some (command, args) -> ["--mcp-config"; mcp_json ~args ~command ()]
       | None -> [])
    | (Config.Codex | Config.Gemini | Config.Agy), _ -> []
  in
  let codex_mcp_overrides =
    match target, remote_mcp with
    | Local, _ -> Some (codex_mcp_overrides ~thread_id ())
    | Remote _, Some (command, args) ->
      Some (codex_mcp_overrides_for ~args ~thread_id ~command ())
    | Remote _, None -> Some []
  in
  (match kind, target with
   | Config.Gemini, Local -> setup_gemini_mcp ~working_dir ()
   | Config.Gemini, Remote { host; destination; _ } ->
     let host =
       { (Host.local_host host) with Host.transport = Host.Ssh destination } in
     Eio_unix.run_in_systhread (fun () ->
       setup_gemini_mcp_remote ?ssh ~host ~working_dir ~mcp:remote_mcp ())
   (* agy: global config, handled with [agy_mcp_command] above. *)
   | (Config.Claude | Config.Codex | Config.Agy), _ -> ());
  let command_args ~session_id_confirmed =
    let agent_args = match kind with
      | Config.Claude ->
        let base = claude_args ~model ~reasoning_effort
            ~fork_from_session_id ~session_id ~session_id_confirmed
            ~message_count ~prompt
          @ claude_mcp_arg
        in
        (* --mcp-config is variadic in the Claude CLI: it keeps eating
           following arguments as additional config paths until it hits
           one starting with `-`. That is safe here only because the
           prompt positional is already in [base] and the sole thing
           appended after is another flag. Anything non-flag appended
           below would silently become a second config file, and Claude
           hard-fails on a config path that doesn't exist. *)
        (match system_prompt with
         | Some sp -> base @ ["--append-system-prompt"; sp]
         | None -> base)
      | Config.Codex ->
        codex_args ?mcp_overrides:codex_mcp_overrides ~model ~reasoning_effort
          ~session_id ~session_id_confirmed ~prompt ()
      | Config.Gemini ->
        gemini_args ~model ~session_id ~session_id_confirmed ~prompt
      | Config.Agy ->
        agy_args ~model ~working_dir ~session_id ~session_id_confirmed ~prompt
    in
    match target with
    | Local -> [setsid_command (); "--wait"] @ agent_args
    (* Remote: the wrapper gives the agent its own session on the
       executor; setsid here applies to the local ssh instead. *)
    | Remote _ -> agent_args
  in
  let agy_guard =
    match kind with
    | Config.Agy when session_id_confirmed ->
      agy_resume_guard ~requested:session_id
    | Config.Agy | Config.Claude | Config.Codex | Config.Gemini -> Fun.id
  in
  let parse_events line = match kind with
    | Config.Claude -> parse_stream_json_line line
    | Config.Codex -> parse_codex_json_line line
    | Config.Gemini -> parse_gemini_stream_json_line line
    | Config.Agy -> agy_guard (parse_agy_stream_json_line line)
  in
  let parse_line line =
    if String.length line > 0 then List.iter on_event (parse_events line)
  in
  (* A remote turn: the local ssh is the tracked child (in its own
     process group, like a local agent), so stop, restart and startup
     reaping work unchanged; killing it makes the executor's watchdog
     kill the agent. The lock key is the thread, which is stable even
     while Codex and Gemini swap a placeholder session id for theirs. *)
  (* The agent carries the caller's thread id in its environment, as
     locally; the MCP command forwards it (see [remote_mcp_command]). *)
  let run_remote ~host ~destination argv =
    if not (is_discord_id thread_id) then
      Result.Error (Printf.sprintf
        "cannot run on %s: thread id %S is not a Discord id" host thread_id)
    else
      Host_exec.run_streaming_ssh ~sw ~mgr ?ssh
        ~prefix:[setsid_command (); "--wait"] ~destination ~cwd:working_dir
        ~lock_key:thread_id ~env:[session_thread_id_env, thread_id]
        ~argv ~on_line:parse_line ?on_spawn:on_pid ()
      |> Result.map_error (remote_failure_message ~host)
  in
  let run_once args =
    let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
    try
      let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
      try
        (* A signed-out agy prompts for an authorization code on stdin
           for up to a minute. With an inherited stdin it would read the
           bot's terminal (the foreground tmux pane under run-branch.sh),
           so give it /dev/null, as remote turns already get from the
           executor wrapper. The child holds its own copy; ours is
           closed as soon as the spawn returns. *)
        let stdin =
          match kind with
          | Config.Agy ->
            Some (Eio.Path.open_in ~sw Eio.Path.(fs / "/dev/null"))
          | Config.Claude | Config.Codex | Config.Gemini -> None
        in
        let proc =
          Fun.protect
            ~finally:(fun () -> Option.iter close_noerr stdin)
            (fun () ->
               Eio.Process.spawn ~sw mgr ~cwd ~env:child_env ?stdin
                 ~stdout:stdout_w ~stderr:stderr_w args)
        in
        (* Close write ends so reads get EOF when process exits. Do this before
           callbacks so callback failures cannot strand pipe writers on [sw]. *)
        Eio.Resource.close stdout_w;
        Eio.Resource.close stderr_w;
        (try
           (match on_pid with Some f -> f (Eio.Process.pid proc) | None -> ())
         with exn ->
           close_noerr stdout_r;
           close_noerr stderr_r;
           raise exn);
        (* Read stdout and stderr concurrently to avoid deadlock.
           If the child fills the stderr pipe buffer (~64KB) before stdout
           hits EOF, the child blocks on write and stdout never closes. *)
        let stderr_buf = Buffer.create 1024 in
        Eio.Fiber.both
          (fun () ->
            (* Drain stderr in parallel *)
            (try
               let sr = Eio.Buf_read.of_flow ~max_size:(64 * 1024) stderr_r in
               Buffer.add_string stderr_buf (Eio.Buf_read.take_all sr)
             with End_of_file -> ());
            Eio.Resource.close stderr_r)
          (fun () ->
            (* Read stdout line by line and parse events *)
            let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
            (try
               while true do
                 let line = Eio.Buf_read.line reader in
                 parse_line line
               done
             with End_of_file -> ());
            Eio.Resource.close stdout_r);
        let stderr_text = Buffer.contents stderr_buf in
        (* Wait for process to finish *)
        let status = Eio.Process.await proc in
        match status with
        | `Exited 0 -> Ok ()
        | `Exited code ->
          Error (Printf.sprintf "agent exited with code %d: %s" code stderr_text)
        | `Signaled sig_ ->
          Error (Printf.sprintf "agent killed by signal %d" sig_)
      with exn ->
        close_noerr stdout_r;
        close_noerr stdout_w;
        close_noerr stderr_r;
        close_noerr stderr_w;
        raise exn
    with exn ->
      close_noerr stdout_r;
      close_noerr stdout_w;
      raise exn
  in
  let run_once args =
    match target with
    | Local -> run_once args
    | Remote { host; destination; _ } -> run_remote ~host ~destination args
  in
  run_with_claude_session_recovery
    ~kind ~session_id ~session_id_confirmed ~message_count
    ~fork_from_session_id
    ~run:(fun confirmed ->
      run_once (command_args ~session_id_confirmed:confirmed))
