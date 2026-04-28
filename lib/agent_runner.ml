(** Agent session runner — manages the lifecycle of agent subprocesses.

    Handles spawning Claude/Codex/Gemini, streaming output to Discord,
    typing indicator refresh, and message splitting for Discord's 2000-char limit.

    Key design: one agent invocation at a time per session, enforced by
    the session's `processing` flag. Large text deltas are split at
    1800-char boundaries before being sent to Discord.

    Tool use events are displayed as compact status messages with emoji
    indicators so users can follow what the agent is doing. *)

(** Typing indicator refresh interval in seconds. *)
let typing_interval = 8.0

(** Map tool names to emoji + verb for compact status display. *)
(** Escape underscores in a tool name for Discord display.
    Discord interprets __ as underline and _ as italic, so we
    prefix each underscore with a backslash. *)
let escape_discord_underscores s =
  let len = String.length s in
  let buf = Buffer.create (len + 8) in
  String.iter (fun c ->
    if c = '_' then Buffer.add_string buf "\\_"
    else Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let tool_display_info name =
  match name with
  | "Read" -> "\xF0\x9F\x93\x96", "Reading"            (* 📖 *)
  | "Edit" -> "\xE2\x9C\x8F\xEF\xB8\x8F", "Editing"   (* ✏️ *)
  | "Write" -> "\xF0\x9F\x93\x9D", "Writing"            (* 📝 *)
  | "Bash" -> "\xF0\x9F\x92\xBB", "Running"             (* 💻 *)
  | "Grep" -> "\xF0\x9F\x94\x8D", "Searching"           (* 🔍 *)
  | "Glob" -> "\xF0\x9F\x93\x82", "Finding files"       (* 📂 *)
  | "Agent" | "Task" -> "\xF0\x9F\xA4\x96", "Spawning agent"  (* 🤖 *)
  | "Skill" -> "\xE2\x9A\xA1", "Using skill"            (* ⚡ *)
  | _ -> "\xF0\x9F\x94\xA7", "Using " ^ escape_discord_underscores name  (* 🔧 *)

(** Format a tool use event as a descriptive status line with optional
    syntax-highlighted detail block.
    Shows enough detail to understand what the agent is doing. *)
let format_tool_status (info : Agent_process.tool_info) =
  let emoji, verb = tool_display_info info.tool_name in
  if info.tool_detail <> "" then
    (* Detail block has the full content — keep header short to avoid duplication *)
    Printf.sprintf "%s **%s**\n%s" emoji verb info.tool_detail
  else if info.tool_summary = "" then
    Printf.sprintf "%s **%s**..." emoji verb
  else
    Printf.sprintf "%s **%s** `%s`" emoji verb info.tool_summary

(** Sanitize a metadata value for safe embedding in the context header.
    Removes newlines, control characters, and brackets to prevent
    prompt injection via user-controlled fields like thread names. *)
let sanitize_context_value s =
  let max_len = 100 in
  let s = if String.length s > max_len then String.sub s 0 max_len else s in
  String.init (String.length s) (fun i ->
    match s.[i] with
    | '\n' | '\r' | '\t' -> ' '
    | '[' | ']' -> '_'
    | c when Char.code c < 32 -> ' '
    | c -> c)

(** Build a context header to prepend to the prompt so the agent knows
    where the message came from. Values are sanitized to prevent injection
    via user-controlled fields (e.g. thread names). *)
let build_context_header ~(session : Session_store.session) ~author_name
    ~channel_name ~channel_type =
  Printf.sprintf "[Discord context: project=%s, channel=%s (%s), from=%s]\n\n"
    (sanitize_context_value session.project_name)
    (sanitize_context_value channel_name)
    channel_type
    (sanitize_context_value author_name)

(** Download attachments to temp files in the working directory.
    Returns a list of (filename, local_path) for successfully downloaded files. *)
let download_attachments ~rest ~working_dir
    (attachments : Discord_types.attachment list) =
  List.filter_map (fun (a : Discord_types.attachment) ->
    (* Cap at 25MB to avoid huge downloads *)
    if a.size > 25 * 1024 * 1024 then begin
      Logs.warn (fun m -> m "agent_runner: skipping large attachment %s (%d bytes)"
        a.filename a.size);
      None
    end else
      let tmp_dir = Filename.concat working_dir ".discord-attachments" in
      (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      let local_path = Filename.concat tmp_dir
        (Printf.sprintf "%s_%s" a.id a.filename) in
      match Discord_rest.download_url rest ~url:a.url () with
      | Ok data ->
        (try
          let oc = open_out_bin local_path in
          output_string oc data;
          close_out oc;
          Some (a.filename, local_path)
        with exn ->
          Logs.warn (fun m -> m "agent_runner: failed to save %s: %s"
            a.filename (Printexc.to_string exn));
          None)
      | Error e ->
        Logs.warn (fun m -> m "agent_runner: failed to download %s: %s"
          a.filename e);
        None
  ) attachments

(** Build a prompt suffix describing downloaded attachments.
    Tells the agent where to find the files so it can use Read to view them. *)
let attachment_prompt_suffix downloaded =
  match downloaded with
  | [] -> ""
  | files ->
    let lines = List.map (fun (filename, path) ->
      Printf.sprintf "- %s: %s" filename path
    ) files in
    Printf.sprintf "\n\n[Attached files — use the Read tool to view them]\n%s"
      (String.concat "\n" lines)

(** Run an agent and stream its output to a Discord channel.
    Handles message creation/editing, typing indicators, and splitting.
    Returns Ok () on success, Error msg on failure. *)
let run ~sw ~env ~rest ~session ~(channel_id : Discord_types.channel_id)
    ~prompt ?(attachments=[]) ~author_name ~channel_name ~channel_type
    ?(wrap_width=Agent_process.desktop_width)
    ?(output_lines=Agent_process.default_output_lines)
    ?on_scroll_content ?on_pid ?on_session_id () =
  (* Download attachments and append paths to the prompt *)
  let downloaded = download_attachments ~rest
    ~working_dir:session.Session_store.working_dir attachments in
  let full_prompt = prompt ^ attachment_prompt_suffix downloaded in
  let context_prompt =
    build_context_header ~session ~author_name ~channel_name ~channel_type
    ^ full_prompt
  in
  (* Send typing indicator *)
  ignore (Discord_rest.send_typing rest ~channel_id ());
  Logs.info (fun m -> m "agent_runner: running %s for %s: %s"
    (Config.string_of_agent_kind session.Session_store.agent_kind)
    session.project_name
    (if String.length prompt > 80
     then String.sub prompt 0 80 ^ "..."
     else prompt));
  let result_buf = Buffer.create 4096 in
  let current_msg_id = ref None in
  let current_msg_buf = Buffer.create 1900 in
  let tool_status_lines = ref [] in
  let tool_status_msg_id = ref None in
  let in_tool_mode = ref false in
  let typing_active = ref true in
  let last_edit = ref (Unix.gettimeofday ()) in
  (* Codex emits fatal errors (turn.failed, top-level error) as JSON
     frames on stdout — its stderr is empty in those cases — so we
     accumulate them here. Appended to the displayed error message on
     nonzero exit; reported as a follow-up if the run still exited 0. *)
  let error_buf = Buffer.create 256 in
  let collect_error e =
    if Buffer.length error_buf > 0 then Buffer.add_char error_buf '\n';
    Buffer.add_string error_buf e
  in
  let collected_errors () =
    if Buffer.length error_buf = 0 then None
    else Some (Buffer.contents error_buf)
  in
  (* Background fiber: continuously send typing indicators while processing.
     Discord typing indicators expire after ~10s, so we refresh every 8s.
     Polls every 1s so the fiber stops within 1s of processing completing,
     preventing stale typing indicators after the checkmark appears. *)
  Eio.Fiber.fork ~sw (fun () ->
    let last_sent = ref (Unix.gettimeofday ()) in
    while !typing_active do
      Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;
      if !typing_active then begin
        let now = Unix.gettimeofday () in
        if now -. !last_sent >= typing_interval then begin
          ignore (Discord_rest.send_typing rest ~channel_id ());
          last_sent := now
        end
      end
    done
  );
  let send_single_message text =
    match !current_msg_id with
    | None ->
      (match Discord_rest.create_message rest ~channel_id ~content:text () with
       | Ok sent -> current_msg_id := Some sent.Discord_types.id
       | Error e -> Logs.warn (fun m -> m "agent_runner: send error: %s" e))
    | Some mid ->
      (match Discord_rest.edit_message rest ~channel_id ~message_id:mid ~content:text () with
       | Ok _ -> ()
       | Error e -> Logs.warn (fun m -> m "agent_runner: edit error: %s" e))
  in
  let flush_to_discord () =
    let text = Agent_process.reformat_tables ~max_width:wrap_width
      (Buffer.contents current_msg_buf) in
    let text = Agent_process.wrap_text ~max_width:wrap_width text in
    let text = Agent_process.escape_nested_fences text in
    if String.length text = 0 then ()
    else if String.length text <= Agent_process.discord_max_len then
      send_single_message text
    else
      (* Table wrapping expanded text beyond Discord's limit — split it.
         First chunk updates the existing message; overflow creates new ones. *)
      let chunks = Agent_process.split_message text in
      List.iteri (fun i chunk ->
        if i = 0 then send_single_message chunk
        else begin
          current_msg_id := None;
          send_single_message chunk
        end
      ) chunks
  in
  (* Flush the buffer as a single message, handling code block continuity. *)
  let flush_and_reset () =
    let buf_text = Buffer.contents current_msg_buf in
    let (in_code, lang) = Agent_process.scan_fences buf_text in
    if in_code then
      Buffer.add_string current_msg_buf "\n```";
    flush_to_discord ();
    Buffer.clear current_msg_buf;
    current_msg_id := None;
    if in_code then
      Buffer.add_string current_msg_buf ("```" ^ lang ^ "\n")
  in
  let start_new_message () =
    let buf_text = Buffer.contents current_msg_buf in
    (* Try to keep tables together: if the buffer ends mid-table,
       split before the table so it starts fresh in the next message. *)
    match Agent_process.find_trailing_table_start buf_text with
    | Some pos when pos > 0 ->
      let before = String.sub buf_text 0 pos in
      let after = String.sub buf_text pos (String.length buf_text - pos) in
      (* Flush the pre-table content *)
      Buffer.clear current_msg_buf;
      Buffer.add_string current_msg_buf before;
      let (in_code, lang) = Agent_process.scan_fences before in
      if in_code then
        Buffer.add_string current_msg_buf "\n```";
      flush_to_discord ();
      (* Start new message with the table content *)
      Buffer.clear current_msg_buf;
      current_msg_id := None;
      if in_code then
        Buffer.add_string current_msg_buf ("```" ^ lang ^ "\n");
      Buffer.add_string current_msg_buf after
    | _ ->
      (* No table boundary (or entire buffer is a table) — split normally *)
      flush_and_reset ()
  in
  (* Flush accumulated tool status lines to a single Discord message.
     Consecutive tool calls get batched into one message, edited in-place. *)
  let flush_tool_status () =
    match !tool_status_lines with
    | [] -> ()
    | lines ->
      let text = String.concat "\n" (List.rev lines) in
      (match !tool_status_msg_id with
       | None ->
         (match Discord_rest.create_message rest ~channel_id ~content:text () with
          | Ok sent -> tool_status_msg_id := Some sent.Discord_types.id
          | Error e -> Logs.warn (fun m -> m "agent_runner: tool status error: %s" e))
       | Some mid ->
         (match Discord_rest.edit_message rest ~channel_id ~message_id:mid ~content:text () with
          | Ok _ -> ()
          | Error e -> Logs.warn (fun m -> m "agent_runner: tool status edit error: %s" e)))
  in
  let on_event = function
    | Agent_process.Text_delta text ->
      (* Transitioning from tool status to text — clear tool state.
         The tool status message visually breaks continuity, so any
         code block reopening prefix left in the buffer from before
         the tool use would render the new text as monospace.
         Uses in_tool_mode flag since tool_status_lines may have been
         cleared by a multi-chunk flush. *)
      if !in_tool_mode then begin
        in_tool_mode := false;
        tool_status_lines := [];
        tool_status_msg_id := None;
        Buffer.clear current_msg_buf;
        current_msg_id := None
      end;
      Buffer.add_string result_buf text;
      (* Split at 1800-char boundaries, reserving space for closing ```
         if we might be inside a code block (worst case: 4 chars for "\n```") *)
      let text_len = String.length text in
      let pos = ref 0 in
      while !pos < text_len do
        (* Flush first if buffer is already at capacity (e.g. from a
           reopened code block prefix) to avoid zero-progress loops *)
        if Buffer.length current_msg_buf >= 1796 then
          start_new_message ();
        let remaining_capacity = 1796 - Buffer.length current_msg_buf in
        let chunk_len = min remaining_capacity (text_len - !pos) in
        Buffer.add_substring current_msg_buf text !pos chunk_len;
        pos := !pos + chunk_len;
        if Buffer.length current_msg_buf >= 1796 then
          start_new_message ()
      done;
      let now = Unix.gettimeofday () in
      if now -. !last_edit > 2.0 && Buffer.length current_msg_buf > 0 then begin
        flush_to_discord ();
        last_edit := now
      end
    | Agent_process.Result { text = _; session_id } ->
      (* Codex assigns session ids server-side (via thread.started);
         forward any new id so the caller can persist it for resume.
         Claude's final result also carries session_id but it matches
         the pre-assigned value, so the callback is a no-op there. *)
      (match session_id, on_session_id with
       | Some sid, Some cb -> cb sid
       | _ -> ());
      flush_tool_status ();
      flush_to_discord ()
    | Agent_process.Tool_use info ->
      Logs.debug (fun m -> m "agent_runner: tool: %s (%s)"
        info.tool_name info.tool_summary);
      in_tool_mode := true;
      (* Flush any pending text before showing tool status *)
      if Buffer.length current_msg_buf > 0 then
        start_new_message ();
      (* Accumulate tool status lines into a single batched message.
         Each new tool call appends a line and edits the message in-place.
         Start a new status message if we'd exceed Discord's 2000-char limit. *)
      let status = format_tool_status info in
      let projected_len =
        let existing = List.fold_left (fun acc l -> acc + String.length l + 1)
          0 !tool_status_lines in
        existing + String.length status
      in
      if projected_len > 1800 && !tool_status_lines <> [] then begin
        flush_tool_status ();
        tool_status_lines := [];
        tool_status_msg_id := None
      end;
      tool_status_lines := status :: !tool_status_lines;
      flush_tool_status ()
    | Agent_process.Tool_result { content } ->
      Logs.debug (fun m -> m "agent_runner: tool result: %d chars"
        (String.length content));
      (* Truncate output for display (both line count and char budget),
         storing full content for !scroll when truncated. *)
      if !in_tool_mode then begin
        (* Chunk long lines first so the inline display and the stored
           scroll state share a single line-indexing scheme. *)
        let chunks = Agent_process.split_into_chunks
          ~max_chars:Agent_process.max_output_display_chars content in
        let t = Agent_process.truncate_for_display
          ~max_lines:output_lines
          ~max_chars:Agent_process.max_output_display_chars
          chunks in
        (* Pass pre-computed chunks and actual-shown count so storage
           doesn't re-chunk and paging starts exactly where display left off. *)
        if t.was_truncated then
          Option.iter (fun cb -> cb chunks t.shown) on_scroll_content;
        let display_text =
          let text = String.concat "\n" t.display in
          if t.was_truncated then
            Printf.sprintf "%s\n*... (%d/%d lines \u{2014} use `!scroll` for more)*"
              text t.shown t.total
          else text in
        let output_block = Printf.sprintf "```\n%s\n```"
          (Agent_process.escape_code_fences display_text) in
        (* Size guard: if adding the output block would exceed Discord's
           limit, flush the existing status first so the output gets its
           own message. Same logic as the Tool_use overflow guard. *)
        let projected_len =
          let existing = List.fold_left (fun acc l -> acc + String.length l + 1)
            0 !tool_status_lines in
          existing + String.length output_block in
        if projected_len > 1800 && !tool_status_lines <> [] then begin
          flush_tool_status ();
          tool_status_lines := [];
          tool_status_msg_id := None
        end;
        tool_status_lines := output_block :: !tool_status_lines;
        flush_tool_status ()
      end
    | Agent_process.Error e ->
      Logs.warn (fun m -> m "agent_runner: error event: %s" e);
      collect_error e
    | Agent_process.Other line ->
      Logs.debug (fun m -> m "agent_runner: other event: %s"
        (Agent_process.truncate_inline ~max_chars:200 line))
  in
  let result =
    Fun.protect ~finally:(fun () -> typing_active := false)
      (fun () -> Agent_process.run_streaming ~sw ~env
          ~working_dir:session.working_dir
          ~kind:session.agent_kind
          ~session_id:session.session_id
          ~session_id_confirmed:session.session_id_confirmed
          ~message_count:session.message_count
          ?system_prompt:session.system_prompt
          ~prompt:context_prompt ~on_event ?on_pid ()) in
  (* All result-path messages route through here so they get split at
     Discord's 2000-char limit. Codex's turn.failed payloads can be
     very large JSON blobs; without splitting, Discord rejects the
     message and the user sees nothing. Empty input is a no-op so
     no caller can accidentally post a blank message. *)
  let send text =
    if text <> "" then
      List.iter (fun chunk ->
        ignore (Discord_rest.create_message rest ~channel_id ~content:chunk ())
      ) (Agent_process.split_message text)
  in
  match result with
  | Ok () ->
    flush_to_discord ();
    (match collected_errors () with
     | Some errs ->
       (* Codex sometimes reports errors mid-turn that don't fail the
          process; surface them so the user sees what went wrong. *)
       send (Printf.sprintf "Agent reported: %s" errs)
     | None when Buffer.length result_buf = 0 -> send "(no response)"
     | None -> ());
    Ok ()
  | Error e ->
    Logs.warn (fun m -> m "agent_runner: error: %s" e);
    (* Flush buffered assistant text before the error message so the
       last partial reply isn't dropped on the floor — the Ok branch
       does this and the symmetry was missing. *)
    flush_to_discord ();
    let combined = match collected_errors () with
      | None -> e
      | Some errs -> errs ^ "\n" ^ e in
    send (Printf.sprintf "Agent error: %s" combined);
    Error e
