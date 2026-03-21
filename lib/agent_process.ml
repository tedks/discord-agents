(** Agent subprocess management using Eio.

    Spawns Claude/Codex/Gemini as subprocesses with proper I/O handling.
    Claude and Gemini use stream-json output for real-time streaming.
    Codex uses plain text output. *)

type stream_event =
  | Text_delta of string   (** Incremental text from the agent *)
  | Result of { text: string; session_id: string option }
  | Tool_use of string     (** Agent is using a tool *)
  | Error of string
  | Other of string        (** Unrecognized event type *)

(** Parse a stream-json line from Claude's output.
    Key event types:
    - {"type":"assistant","message":{"content":[{"type":"text","text":"..."}]}}
    - {"type":"result","subtype":"success","result":"...","session_id":"..."} *)
let parse_stream_json_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let typ = json |> member "type" |> to_string_option in
    match typ with
    | Some "assistant" ->
      let msg = json |> member "message" in
      let content = msg |> member "content" in
      let texts = match content with
        | `List items ->
          List.filter_map (fun item ->
            match item |> member "type" |> to_string_option with
            | Some "text" -> item |> member "text" |> to_string_option
            | Some "tool_use" ->
              let name = item |> member "name" |> to_string_option in
              Some (Printf.sprintf "[tool: %s]" (Option.value ~default:"?" name))
            | _ -> None
          ) items
        | _ -> []
      in
      if texts = [] then Other line
      else Text_delta (String.concat "" texts)
    | Some "result" ->
      let result_text = json |> member "result" |> to_string_option
        |> Option.value ~default:"" in
      let session_id = json |> member "session_id" |> to_string_option in
      Result { text = result_text; session_id }
    | _ -> Other line
  with _ -> Other line

(** Build the command args for an agent invocation. *)
let claude_args ~session_id ~message_count ~prompt =
  let session_flag =
    if message_count = 0 then ["--session-id"; session_id]
    else ["--resume"; session_id]
  in
  ["claude"; "-p"; "--output-format"; "stream-json"] @ session_flag @ [prompt]

(** Spawn an agent and stream its output via a callback.
    The callback is called with each parsed event as it arrives.
    Returns when the process exits. *)
let run_streaming ~sw ~env ~working_dir ~kind ~session_id ~message_count
    ~prompt ~on_event =
  let mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Path.(fs / working_dir) in
  let args = match kind with
    | Config.Claude -> claude_args ~session_id ~message_count ~prompt
    | Config.Codex -> ["codex"; "exec"; "--json"; prompt]
    | Config.Gemini -> ["gemini"; "-p"; "-o"; "stream-json"; prompt]
  in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~cwd
    ~stdout:stdout_w ~stderr:stderr_w args in
  (* Close write ends so reads get EOF when process exits *)
  Eio.Resource.close stdout_w;
  Eio.Resource.close stderr_w;
  (* Read stdout line by line and parse events *)
  let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
  (try
    while true do
      let line = Eio.Buf_read.line reader in
      if String.length line > 0 then begin
        match kind with
        | Config.Claude | Config.Gemini ->
          let event = parse_stream_json_line line in
          on_event event
        | Config.Codex ->
          on_event (Text_delta line)
      end
    done
  with End_of_file -> ());
  Eio.Resource.close stdout_r;
  (* Read any stderr *)
  let stderr_text =
    try
      let sr = Eio.Buf_read.of_flow ~max_size:(64 * 1024) stderr_r in
      Eio.Buf_read.take_all sr
    with End_of_file -> ""
  in
  Eio.Resource.close stderr_r;
  (* Wait for process to finish *)
  let status = Eio.Process.await proc in
  ignore sw;
  match status with
  | `Exited 0 -> Ok ()
  | `Exited code ->
    Error (Printf.sprintf "agent exited with code %d: %s" code stderr_text)
  | `Signaled sig_ ->
    Error (Printf.sprintf "agent killed by signal %d" sig_)
