(** Agent subprocess management using Eio.

    Spawns Claude/Codex/Gemini as subprocesses with proper I/O handling.
    Claude and Gemini use stream-json output for real-time streaming.
    Codex uses plain text output. *)

type tool_info = {
  tool_name: string;        (** Tool name (e.g. "Read", "Edit", "Bash") *)
  tool_summary: string;     (** Human-readable summary of what the tool is doing *)
}

type stream_event =
  | Text_delta of string   (** Incremental text from the agent *)
  | Result of { text: string; session_id: string option }
  | Tool_use of tool_info  (** Agent is using a tool *)
  | Error of string
  | Other of string        (** Unrecognized event type *)

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
  match name with
  | "Read" | "Edit" | "Write" ->
    (match get "file_path" with
     | Some p -> basename p | None -> "")
  | "Bash" ->
    (match get "command" with
     | Some c -> clean 60 c | None -> "")
  | "Grep" ->
    (match get "pattern" with
     | Some pat ->
       let path = match get "path" with Some p -> " in " ^ basename p | None -> "" in
       clean 40 pat ^ path
     | None -> "")
  | "Glob" ->
    (match get "pattern" with
     | Some pat -> clean 60 pat | None -> "")
  | "Agent" | "Task" ->
    (match get "description" with
     | Some d -> clean 60 d | None -> "")
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
              Some (Tool_use { tool_name = name; tool_summary = summary })
            | _ -> None
          ) items
        | _ -> []
      in
      if events = [] then [Other line] else events
    | Some "result" ->
      let result_text = json |> member "result" |> to_string_option
        |> Option.value ~default:"" in
      let session_id = json |> member "session_id" |> to_string_option in
      [Result { text = result_text; session_id }]
    | _ -> [Other line]
  with _ -> [Other line]

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
    ?system_prompt ~prompt ~on_event () =
  let mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Path.(fs / working_dir) in
  (* Derive MCP config path from executable location *)
  let mcp_config =
    try
      let exe = Sys.executable_name in
      let exe = if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe in
      let rec find_root path =
        let candidate = Filename.concat path "scripts/mcp.json" in
        if Sys.file_exists candidate then candidate
        else
          let parent = Filename.dirname path in
          if parent = path then "scripts/mcp.json"
          else find_root parent
      in
      find_root (Filename.dirname exe)
    with _ -> "scripts/mcp.json"
  in
  let args = match kind with
    | Config.Claude ->
      let base = claude_args ~session_id ~message_count ~prompt in
      let base = match system_prompt with
        | Some sp ->
          (* Control channel sessions get MCP tools + system prompt *)
          base @ ["--append-system-prompt"; sp;
                  "--mcp-config"; mcp_config]
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
