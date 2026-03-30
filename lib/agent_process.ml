(** Agent subprocess management using Eio.

    Spawns Claude/Codex/Gemini as subprocesses with proper I/O handling.
    Claude and Gemini use stream-json output for real-time streaming.
    Codex uses plain text output. *)

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

(** Wrap a single line at word boundaries to fit within [max_width].
    Returns a list of wrapped lines. Preserves leading whitespace. *)
let wrap_line ~max_width line =
  let len = String.length line in
  if len <= max_width then [line]
  else
    let words = String.split_on_char ' ' line in
    let rec build current_line lines = function
      | [] ->
        List.rev (current_line :: lines)
      | word :: rest ->
        let cur_len = String.length current_line in
        let word_len = String.length word in
        if cur_len = 0 then
          (* First word on the line — take it even if it exceeds max_width *)
          build word lines rest
        else if cur_len + 1 + word_len <= max_width then
          build (current_line ^ " " ^ word) lines rest
        else
          build word (current_line :: lines) rest
    in
    build "" [] words

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
          | None -> limit
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
  let truncate n s =
    if String.length s <= n then s
    else String.sub s 0 n ^ "..."
  in
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

(** Maximum length for tool detail code blocks in Discord.
    Leaves room for the status line and other tool lines in the batch. *)
let max_detail_len = 800

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

(** Truncate a string, adding "..." if truncated. *)
let truncate_detail n s =
  if String.length s <= n then s
  else String.sub s 0 n ^ "\n..."

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

(** Generate a syntax-highlighted code block showing tool content details.
    Returns "" if the tool doesn't have interesting content to show. *)
let detail_of_tool_input name input =
  let open Yojson.Safe.Util in
  let get key = input |> member key |> to_string_option in
  (* Build a code block with fences escaped in the content *)
  let code_block lang content =
    Printf.sprintf "```%s\n%s\n```" lang
      (escape_code_fences (truncate_detail max_detail_len content))
  in
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
      code_block "diff" content
  | "Bash" ->
    (match get "command" with
     | Some cmd when String.length cmd > 0 -> code_block "bash" cmd
     | _ -> "")
  | "Write" ->
    let path = match get "file_path" with Some p -> p | None -> "" in
    let lang = lang_of_path path in
    (match get "content" with
     | Some c when String.length c > 0 -> code_block lang c
     | _ -> "")
  | "Grep" ->
    (match get "pattern" with
     | Some pat when String.length pat > 0 ->
       code_block "" ("/" ^ truncate_detail 200 pat ^ "/")
     | _ -> "")
  | _ -> ""

(** Parse a stream-json line into a list of events.
    Returns a list because a single assistant message can contain
    both text and tool_use content blocks. *)
let parse_stream_json_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let typ = json |> member "type" |> to_string_option in
    match typ with
    | Some "assistant" ->
      let msg = json |> member "message" in
      let content = msg |> member "content" in
      let events = match content with
        | `List items ->
          List.filter_map (fun item ->
            match item |> member "type" |> to_string_option with
            | Some "text" ->
              (match item |> member "text" |> to_string_option with
               | Some t -> Some (Text_delta t)
               | None -> None)
            | Some "tool_use" ->
              let name = item |> member "name" |> to_string_option
                |> Option.value ~default:"unknown" in
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
                    match p |> member "type" |> to_string_option with
                    | Some "text" -> p |> member "text" |> to_string_option
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
      let result_text = json |> member "result" |> to_string_option
        |> Option.value ~default:"" in
      let session_id = json |> member "session_id" |> to_string_option in
      [Result { text = result_text; session_id }]
    | Some "tool_result" ->
      (* Top-level tool result event *)
      let content = match json |> member "content" with
        | `String s -> s
        | _ -> json |> member "output" |> to_string_option
               |> Option.value ~default:"" in
      if content = "" then [Other line]
      else [Tool_result { content }]
    | _ -> [Other line]
  with _ -> [Other line]

(** Build the command args for an agent invocation. *)
let claude_args ~session_id ~message_count ~prompt =
  let session_flag =
    if message_count = 0 then ["--session-id"; session_id]
    else ["--resume"; session_id]
  in
  ["claude"; "-p"; "--verbose"; "--output-format"; "stream-json"] @ session_flag @ [prompt]

(** Spawn an agent and stream its output via a callback.
    The callback is called with each parsed event as it arrives.
    [?on_pid] is called with the child PID immediately after spawn,
    so the caller can track active subprocesses for cleanup.
    Returns when the process exits. *)
let run_streaming ~sw ~env ~working_dir ~kind ~session_id ~message_count
    ?system_prompt ~prompt ~on_event ?on_pid () =
  let mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Path.(fs / working_dir) in
  (* Generate MCP config with absolute path to the Python script.
     The static mcp.json has a relative path that breaks when Claude
     runs in a different working directory. *)
  let mcp_config =
    let script_path =
      try
        let exe = Sys.executable_name in
        let exe = if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe in
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
    in
    (* Write a temp MCP config with the absolute script path *)
    let config_dir = Filename.concat (Sys.getenv "HOME") ".config/discord-agents" in
    let config_path = Filename.concat config_dir "mcp-generated.json" in
    let json = Printf.sprintf
      {|{"mcpServers":{"discord-agents":{"command":"python3","args":["%s"]}}}|}
      script_path in
    (try
      let oc = open_out config_path in
      output_string oc json;
      close_out oc
    with _ -> ());
    config_path
  in
  let args = match kind with
    | Config.Claude ->
      let base = claude_args ~session_id ~message_count ~prompt in
      (* All Claude sessions get MCP tools for thread/session management *)
      let base = base @ ["--mcp-config"; mcp_config] in
      let base = match system_prompt with
        | Some sp -> base @ ["--append-system-prompt"; sp]
        | None -> base
      in
      base
    | Config.Codex -> ["codex"; "exec"; prompt]
    | Config.Gemini -> ["gemini"; "-p"; prompt; "-o"; "stream-json"]
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
            match kind with
            | Config.Claude | Config.Gemini ->
              let events = parse_stream_json_line line in
              List.iter on_event events
            | Config.Codex ->
              on_event (Text_delta line)
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
