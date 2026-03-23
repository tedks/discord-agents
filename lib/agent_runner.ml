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
  | _ -> "\xF0\x9F\x94\xA7", "Using " ^ name            (* 🔧 *)

(** Format a tool use event as a compact status line. *)
let format_tool_status (info : Agent_process.tool_info) =
  let emoji, verb = tool_display_info info.tool_name in
  if info.tool_summary = "" then
    Printf.sprintf "%s %s..." emoji verb
  else
    Printf.sprintf "%s %s `%s`..." emoji verb info.tool_summary

(** Run an agent and stream its output to a Discord channel.
    Handles message creation/editing, typing indicators, and splitting.
    Returns Ok () on success, Error msg on failure. *)
let run ~sw ~env ~rest ~session ~(channel_id : Discord_types.channel_id) ~prompt () =
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
  let last_typing = ref (Unix.gettimeofday ()) in
  let last_edit = ref (Unix.gettimeofday ()) in
  let flush_to_discord () =
    let text = Buffer.contents current_msg_buf in
    if String.length text = 0 then ()
    else match !current_msg_id with
    | None ->
      (match Discord_rest.create_message rest ~channel_id ~content:text () with
       | Ok sent -> current_msg_id := Some sent.Discord_types.id
       | Error e -> Logs.warn (fun m -> m "agent_runner: send error: %s" e))
    | Some mid ->
      (match Discord_rest.edit_message rest ~channel_id ~message_id:mid ~content:text () with
       | Ok _ -> ()
       | Error e -> Logs.warn (fun m -> m "agent_runner: edit error: %s" e))
  in
  let start_new_message () =
    (* Before flushing, check if buffer ends inside a code block.
       If so, close it in this message so Discord renders it properly,
       and remember to reopen it in the next message. *)
    let buf_text = Buffer.contents current_msg_buf in
    let (in_code, lang) = Agent_process.scan_fences buf_text in
    if in_code then
      Buffer.add_string current_msg_buf "\n```";
    flush_to_discord ();
    Buffer.clear current_msg_buf;
    current_msg_id := None;
    (* Reopen the code block in the next message *)
    if in_code then
      Buffer.add_string current_msg_buf ("```" ^ lang ^ "\n")
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
         No need to flush: the last Tool_use event already flushed. *)
      if !tool_status_lines <> [] then begin
        tool_status_lines := [];
        tool_status_msg_id := None
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
      end;
      if now -. !last_typing > typing_interval then begin
        ignore (Discord_rest.send_typing rest ~channel_id ());
        last_typing := now
      end
    | Agent_process.Result { text = _; session_id = _ } ->
      flush_tool_status ();
      flush_to_discord ()
    | Agent_process.Tool_use info ->
      Logs.debug (fun m -> m "agent_runner: tool: %s (%s)"
        info.tool_name info.tool_summary);
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
    | Agent_process.Error e ->
      Logs.warn (fun m -> m "agent_runner: error event: %s" e)
    | Agent_process.Other _ -> ()
  in
  match Agent_process.run_streaming ~sw ~env
          ~working_dir:session.working_dir
          ~kind:session.agent_kind
          ~session_id:session.session_id
          ~message_count:session.message_count
          ?system_prompt:session.system_prompt
          ~prompt ~on_event () with
  | Ok () ->
    flush_to_discord ();
    if Buffer.length result_buf = 0 then
      ignore (Discord_rest.create_message rest ~channel_id
        ~content:"(no response)" ());
    Ok ()
  | Error e ->
    Logs.warn (fun m -> m "agent_runner: error: %s" e);
    ignore (Discord_rest.create_message rest ~channel_id
      ~content:(Printf.sprintf "Agent error: %s" e) ());
    Error e
