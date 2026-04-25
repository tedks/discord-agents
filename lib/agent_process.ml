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
          then String.sub cell 0 w
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
      let len = take_fitting_prefix ~start:!pos ~max_chars line in
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
          let taken = take_fitting_prefix ~max_chars l in
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

(** Build the command args for an agent invocation. *)
let claude_args ~session_id ~message_count ~prompt =
  let session_flag =
    if message_count = 0 then ["--session-id"; session_id]
    else ["--resume"; session_id]
  in
  ["claude"; "-p"; "--verbose"; "--output-format"; "stream-json"] @ session_flag @ [prompt]

(* ── MCP server config (shared by all three agents) ──────────────

   All three agents expose the bot's MCP tools (start_session,
   list_*, etc.) by pointing at scripts/mcp-server.py. They each
   accept the pointer through a different mechanism:

   - Claude takes a [--mcp-config <path>] flag; we write the JSON to
     [~/.config/discord-agents/mcp-generated.json] and pass the path.
   - Codex takes [-c key=value] TOML overrides per invocation; we
     inject [mcp_servers.discord_agents.command/args] without
     touching the user's [~/.codex/config.toml].
   - Gemini has no flag; it loads [mcpServers] from
     [<cwd>/.gemini/settings.json]. We write the file into the
     worktree and add [.gemini/] to [.git/info/exclude]. *)

let mcp_script_path = lazy (
  (* Allow an explicit override for installs where the script doesn't
     live next to the executable (e.g. a standalone binary, or a
     packaged install). Falls through to the heuristic search if
     unset. *)
  match Sys.getenv_opt "DISCORD_AGENTS_MCP_SCRIPT" with
  | Some path when path <> "" -> path
  | _ ->
    try
      let exe = Sys.executable_name in
      let exe = if Filename.is_relative exe
        then Filename.concat (Sys.getcwd ()) exe else exe in
      let rec find_root path =
        let candidate = Filename.concat path "scripts/mcp-server.py" in
        if Sys.file_exists candidate then candidate
        else
          let parent = Filename.dirname path in
          if parent = path then "scripts/mcp-server.py"
          else find_root parent
      in
      find_root (Filename.dirname exe)
    with _ -> "scripts/mcp-server.py"
)

let mcp_json = lazy (
  Printf.sprintf
    {|{"mcpServers":{"discord-agents":{"command":"python3","args":["%s"]}}}|}
    (Lazy.force mcp_script_path)
)

let write_file_safely ~path contents =
  try
    let oc = open_out path in
    output_string oc contents;
    close_out oc
  with _ -> ()

let read_file_opt path =
  try
    let ic = open_in path in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      Some (Bytes.to_string s))
  with _ -> None

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

(** Claude: write the MCP config to a well-known location and return
    the path for [--mcp-config]. *)
let claude_mcp_config_path () =
  let config_dir = Filename.concat (Sys.getenv "HOME") ".config/discord-agents" in
  let config_path = Filename.concat config_dir "mcp-generated.json" in
  write_file_safely ~path:config_path (Lazy.force mcp_json);
  config_path

(** Inject our [discord-agents] entry into a Gemini settings JSON
    string (preserving any other [mcpServers] and unrelated
    top-level keys the user might have set). Falls back to a fresh
    config when the input is missing, malformed, or parseable but
    not an object — in any of those cases the existing content
    couldn't have meaningfully held our entry, so we'd lose nothing
    by replacing it. *)
let merge_gemini_settings existing =
  let our_entry = `Assoc [
    ("command", `String "python3");
    ("args", `List [`String (Lazy.force mcp_script_path)]);
  ] in
  let render_fresh () =
    Yojson.Safe.pretty_to_string
      (`Assoc [("mcpServers", `Assoc [("discord-agents", our_entry)])])
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
        let new_servers =
          ("discord-agents", our_entry)
          :: List.filter (fun (k, _) -> k <> "discord-agents") prior_servers
        in
        let new_fields =
          ("mcpServers", `Assoc new_servers)
          :: List.filter (fun (k, _) -> k <> "mcpServers") fields
        in
        Yojson.Safe.pretty_to_string (`Assoc new_fields)
      | _ -> render_fresh ()  (* parseable but non-object: discard *)
    with _ -> render_fresh ()

(** Resolve the path to the [info/exclude] file for [working_dir]'s
    git repo. In a regular checkout this is [<wd>/.git/info/exclude];
    in a git worktree, [.git] is a *file* pointing at
    [<gitdir>/worktrees/<name>], whose own [info/exclude] is the
    worktree-specific one. Shells out to [git rev-parse] so we don't
    have to re-implement that lookup ourselves; returns None if
    [working_dir] isn't a git repo at all. *)
let resolve_git_info_exclude ~working_dir =
  if working_dir = "" then None
  else
    try
      let cmd = Printf.sprintf
        "git -C %s rev-parse --git-dir 2>/dev/null"
        (Filename.quote working_dir) in
      let ic = Unix.open_process_in cmd in
      let line = try input_line ic with End_of_file -> "" in
      let _ = Unix.close_process_in ic in
      let trimmed = String.trim line in
      if trimmed = "" then None
      else
        let gitdir =
          if Filename.is_relative trimmed
          then Filename.concat working_dir trimmed
          else trimmed
        in
        Some (Filename.concat gitdir "info/exclude")
    with _ -> None

(** Gemini: drop our entry into [.gemini/settings.json] (merging into
    any pre-existing user config) and ensure [.gemini/] is in the
    repo's [info/exclude]. Both writes are idempotent — re-running
    against a worktree we've already configured leaves the file and
    the exclude line unchanged in shape. *)
let setup_gemini_mcp ~working_dir =
  let gemini_dir = Filename.concat working_dir ".gemini" in
  (try
     if not (Sys.file_exists gemini_dir) then Unix.mkdir gemini_dir 0o755
   with _ -> ());
  let settings_path = Filename.concat gemini_dir "settings.json" in
  let merged = merge_gemini_settings (read_file_opt settings_path) in
  write_file_safely ~path:settings_path merged;
  match resolve_git_info_exclude ~working_dir with
  | None -> ()
  | Some exclude_path ->
    match read_file_opt exclude_path with
    | Some existing when contains_substring existing ".gemini/" -> ()
    | _ ->
      try
        (* exclude_path is in [<gitdir>/info/]; create info/ if needed. *)
        let info_dir = Filename.dirname exclude_path in
        (try if not (Sys.file_exists info_dir) then Unix.mkdir info_dir 0o755
         with _ -> ());
        let oc = open_out_gen [Open_append; Open_creat] 0o644 exclude_path in
        Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
          output_string oc "\n.gemini/\n")
      with _ -> ()

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
let codex_mcp_overrides () =
  let path = escape_toml_string (Lazy.force mcp_script_path) in
  [ "-c"; {|mcp_servers.discord_agents.command="python3"|};
    "-c"; Printf.sprintf
      {|mcp_servers.discord_agents.args=["%s"]|} path ]

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
    from being mistaken for a Codex flag. *)
let codex_args ~session_id ~session_id_confirmed ~prompt =
  let base = ["codex"; "exec"; "--json"; "--full-auto";
              "--skip-git-repo-check"]
             @ codex_mcp_overrides () in
  if not session_id_confirmed then base @ ["--"; prompt]
  else base @ ["resume"; session_id; "--"; prompt]

(** Like Codex, Gemini allocates its session id server-side in the
    [init] event and we capture it from the parser's first emitted
    Result. Subsequent runs resume only when [session_id_confirmed],
    matching the same coherence contract Codex uses.

    [--yolo] auto-approves tool calls; without it Gemini blocks on an
    interactive approval prompt that the non-interactive subprocess
    can't answer (mirrors Codex's [--full-auto]).

    Gemini's MCP server config is written into [.gemini/settings.json]
    by [setup_gemini_mcp], which run_streaming calls before invoking
    [gemini_args]. *)
let gemini_args ~session_id ~session_id_confirmed ~prompt =
  let base = ["gemini"; "-p"; prompt; "-o"; "stream-json"; "--yolo"] in
  if not session_id_confirmed then base
  else base @ ["--resume"; session_id]

(** Compose the per-turn prompt sent to a non-Claude agent, prepending
    the session's system prompt on the first turn so MCP-aware agents
    know what tools they have. Claude takes [--append-system-prompt]
    via run_streaming and skips this. The conversation history carries
    the system prompt forward on subsequent turns, so we only inject
    it when [message_count = 0].

    Pure function so it's testable without a subprocess. *)
let compose_session_prompt ~agent_kind ~system_prompt ~message_count
    ~user_prompt =
  let needs_inline =
    message_count = 0
    && (match agent_kind with
        | Config.Codex | Config.Gemini -> true
        | Config.Claude -> false)
  in
  match system_prompt with
  | Some sp when needs_inline ->
    Printf.sprintf "<bot-context>\n%s\n</bot-context>\n\n%s" sp user_prompt
  | _ -> user_prompt

(** Spawn an agent and stream its output via a callback.
    The callback is called with each parsed event as it arrives.
    [?on_pid] is called with the child PID immediately after spawn,
    so the caller can track active subprocesses for cleanup.
    Returns when the process exits. *)
let run_streaming ~sw ~env ~working_dir ~kind ~session_id ~message_count
    ?(session_id_confirmed=true) ?system_prompt ~prompt ~on_event ?on_pid () =
  let mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Path.(fs / working_dir) in
  (* Codex/Gemini get the system prompt prepended to the first user
     turn (compose_session_prompt); Claude gets it via the dedicated
     [--append-system-prompt] flag instead. *)
  let prompt = compose_session_prompt
    ~agent_kind:kind ~system_prompt ~message_count ~user_prompt:prompt in
  let args = match kind with
    | Config.Claude ->
      let base = claude_args ~session_id ~message_count ~prompt in
      let base = base @ ["--mcp-config"; claude_mcp_config_path ()] in
      (match system_prompt with
       | Some sp -> base @ ["--append-system-prompt"; sp]
       | None -> base)
    | Config.Codex ->
      codex_args ~session_id ~session_id_confirmed ~prompt
    | Config.Gemini ->
      setup_gemini_mcp ~working_dir;
      gemini_args ~session_id ~session_id_confirmed ~prompt
  in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~cwd
    ~stdout:stdout_w ~stderr:stderr_w args in
  (match on_pid with Some f -> f (Eio.Process.pid proc) | None -> ());
  (* Close write ends so reads get EOF when process exits *)
  Eio.Resource.close stdout_w;
  Eio.Resource.close stderr_w;
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
          if String.length line > 0 then begin
            let events = match kind with
              | Config.Claude -> parse_stream_json_line line
              | Config.Codex -> parse_codex_json_line line
              | Config.Gemini -> parse_gemini_stream_json_line line
            in
            List.iter on_event events
          end
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
