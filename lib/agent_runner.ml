(** Agent session runner — manages the lifecycle of agent subprocesses.

    Handles spawning Claude/Codex/Gemini, streaming output to Discord,
    typing indicator refresh, and message splitting for Discord's 2000-char limit.

    Key design: one agent invocation at a time per session, enforced by
    the session's `processing` flag. Large text deltas are split at
    1800-char boundaries before being sent to Discord. *)

(** Typing indicator refresh interval in seconds. *)
let typing_interval = 8.0

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
    flush_to_discord ();
    Buffer.clear current_msg_buf;
    current_msg_id := None
  in
  let on_event = function
    | Agent_process.Text_delta text ->
      Buffer.add_string result_buf text;
      (* Split large deltas at 1800-char boundaries *)
      let text_len = String.length text in
      let pos = ref 0 in
      while !pos < text_len do
        let remaining_capacity = 1800 - Buffer.length current_msg_buf in
        let chunk_len = min remaining_capacity (text_len - !pos) in
        if chunk_len > 0 then
          Buffer.add_substring current_msg_buf text !pos chunk_len;
        pos := !pos + chunk_len;
        if Buffer.length current_msg_buf >= 1800 then
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
      flush_to_discord ()
    | Agent_process.Tool_use name ->
      Logs.debug (fun m -> m "agent_runner: tool: %s" name)
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
