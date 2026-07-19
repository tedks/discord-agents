module Control_api = Discord_agents.Control_api
module Control_client = Discord_agents.Control_client
module Mcp_formatter = Discord_agents.Mcp_formatter
module Mcp_handler = Discord_agents.Mcp_handler
module Mcp_server = Discord_agents.Mcp_server

let failf fmt = Format.kasprintf (fun message -> Alcotest.fail message) fmt

let rec canonical_json = function
  | `Assoc fields ->
    fields
    |> List.map (fun (key, value) -> key, canonical_json value)
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> fun fields -> `Assoc fields
  | `List values -> `List (List.map canonical_json values)
  | other -> other

let json_string json =
  Yojson.Safe.to_string (canonical_json json)

let check_json label expected actual =
  Alcotest.(check string) label (json_string expected) (json_string actual)

let repo_file path =
  let rec search dir =
    let candidate = Filename.concat dir path in
    if Sys.file_exists candidate then candidate
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then
        failf "could not find %s from %s" path (Sys.getcwd ())
      else
        search parent
  in
  search (Sys.getcwd ())

let read_process_output command =
  let ic = Unix.open_process_in command in
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  (try
     while true do
       match input ic chunk 0 (Bytes.length chunk) with
       | 0 -> raise End_of_file
       | n -> Buffer.add_subbytes buf chunk 0 n
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Buffer.contents buf
  | Unix.WEXITED n -> failf "command exited %d: %s" n command
  | Unix.WSIGNALED n -> failf "command signaled %d: %s" n command
  | Unix.WSTOPPED n -> failf "command stopped %d: %s" n command

let python_tool_output tool_name response =
  let script = repo_file "scripts/mcp-server.py" in
  let program =
    "import json, runpy, sys; "
    ^ "ns = runpy.run_path(sys.argv[1]); "
    ^ "response = json.loads(sys.argv[2]); "
    ^ "ns['handle_tool_call'].__globals__['control_request'] = "
    ^ "lambda method, params=None, timeout=60: response; "
    ^ "sys.stdout.write(ns['handle_tool_call'](sys.argv[3], {}))"
  in
  let command =
    Printf.sprintf "python3 -c %s %s %s %s"
      (Filename.quote program)
      (Filename.quote script)
      (Filename.quote (Yojson.Safe.to_string response))
      (Filename.quote tool_name)
  in
  read_process_output command

let python_list_projects_output response =
  python_tool_output "list_projects" response

let python_list_sessions_output response =
  python_tool_output "list_sessions" response

let project ?(is_bare=false) name path =
  `Assoc [
    ("name", `String name);
    ("path", `String path);
    ("is_bare", `Bool is_bare);
  ]

let list_projects_response projects =
  `Assoc [
    ("ok", `Bool true);
    ("projects", `List projects);
  ]

let session ?(session_id="session-1") project_name agent_kind message_count thread_id =
  `Assoc [
    ("project_name", `String project_name);
    ("agent_kind", `String agent_kind);
    ("message_count", `Int message_count);
    ("thread_id", `String thread_id);
    ("session_id", `String session_id);
  ]

let list_sessions_response sessions =
  `Assoc [
    ("ok", `Bool true);
    ("sessions", `List sessions);
  ]

let check_list_projects_parity label response =
  let expected = python_list_projects_output response in
  let actual =
    match Mcp_formatter.format_list_projects response with
    | Ok text -> text
    | Error message -> failf "%s: formatter error: %s" label message
  in
  Alcotest.(check string) label expected actual

let check_list_sessions_parity label response =
  let expected = python_list_sessions_output response in
  let actual =
    match Mcp_formatter.format_list_sessions response with
    | Ok text -> text
    | Error message -> failf "%s: formatter error: %s" label message
  in
  Alcotest.(check string) label expected actual

let test_format_list_projects_matches_python () =
  check_list_projects_parity "empty" (list_projects_response []);
  check_list_projects_parity "projects"
    (list_projects_response [
      project "alpha" "/tmp/alpha";
      project ~is_bare:true "beta" "/tmp/beta.git";
    ]);
  check_list_projects_parity "missing projects"
    (`Assoc [("ok", `Bool true)])

let test_format_list_projects_control_error () =
  Alcotest.(check (result string string))
    "control error"
    (Error "Bot is not running.")
    (Mcp_formatter.format_list_projects
       (`Assoc [("error", `String "Bot is not running.")]))

let test_format_list_sessions_matches_python () =
  check_list_sessions_parity "empty" (list_sessions_response []);
  check_list_sessions_parity "sessions"
    (list_sessions_response [
      session "alpha" "claude" 3 "111";
      session ~session_id:"session-2" "beta" "codex" 0 "222";
    ]);
  check_list_sessions_parity "missing sessions"
    (`Assoc [("ok", `Bool true)])

let test_format_list_sessions_control_error () =
  Alcotest.(check (result string string))
    "control error"
    (Error "Bot is not running.")
    (Mcp_formatter.format_list_sessions
       (`Assoc [("error", `String "Bot is not running.")]))

let test_handler_list_projects_requests_control_api () =
  let calls = ref [] in
  let response =
    list_projects_response [project ~is_bare:true "repo" "/src/repo.git"]
  in
  let control_client =
    Control_client.make ~request:(fun request ->
      calls := request :: !calls;
      Ok response)
  in
  let call = { Mcp_server.name = "list_projects"; arguments = `Assoc [] } in
  Alcotest.(check (result string string))
    "handler output"
    (Ok "1. **repo** — `/src/repo.git` [bare]")
    (Mcp_handler.handle_tool_call ~control_client call);
  match !calls with
  | [request] ->
    Alcotest.(check string) "method" "list_projects" request.method_name;
    Alcotest.(check int) "timeout" 60 request.timeout_s;
    Alcotest.(check bool) "params omitted" true
      (Option.is_none request.params)
  | calls -> failf "expected one control request, got %d" (List.length calls)

let test_handler_list_sessions_requests_control_api () =
  let calls = ref [] in
  let response =
    list_sessions_response [session "repo" "gemini" 12 "123"]
  in
  let control_client =
    Control_client.make ~request:(fun request ->
      calls := request :: !calls;
      Ok response)
  in
  let call = { Mcp_server.name = "list_sessions"; arguments = `Assoc [] } in
  Alcotest.(check (result string string))
    "handler output"
    (Ok "- **repo** / gemini — 12 messages (thread: <#123>)")
    (Mcp_handler.handle_tool_call ~control_client call);
  match !calls with
  | [request] ->
    Alcotest.(check string) "method" "list_sessions" request.method_name;
    Alcotest.(check int) "timeout" 60 request.timeout_s;
    Alcotest.(check bool) "params omitted" true
      (Option.is_none request.params)
  | calls -> failf "expected one control request, got %d" (List.length calls)

let test_handler_unsupported_tool () =
  let control_client =
    Control_client.make ~request:(fun _request ->
      failf "unsupported tool should not call control API")
  in
  let call = { Mcp_server.name = "send_message"; arguments = `Assoc [] } in
  Alcotest.(check (result string string))
    "unsupported"
    (Error "OCaml MCP tools/call is not wired yet for tool: send_message")
    (Mcp_handler.handle_tool_call ~control_client call)

let test_server_wraps_list_projects_result () =
  let control_client =
    Control_client.make ~request:(fun _request ->
      Ok (list_projects_response [project "alpha" "/tmp/alpha"]))
  in
  let line =
    {|{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"list_projects"}}|}
  in
  let actual =
    Mcp_server.handle_line
      ~handle_tool_call:(Mcp_handler.handle_tool_call ~control_client)
      line
  in
  let expected =
    Some (`Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int 9);
      ("result", `Assoc [
        ("content", `List [
          `Assoc [
            ("type", `String "text");
            ("text", `String "1. **alpha** — `/tmp/alpha`");
          ];
        ]);
      ]);
    ])
  in
  match actual, expected with
  | Some actual, Some expected -> check_json "MCP response" expected actual
  | _ -> failf "expected MCP response"

let test_server_wraps_list_projects_control_error () =
  let control_client =
    Control_client.make ~request:(fun _request ->
      Ok (`Assoc [("error", `String "Bot is not running.")]))
  in
  let line =
    {|{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"list_projects"}}|}
  in
  let actual =
    Mcp_server.handle_line
      ~handle_tool_call:(Mcp_handler.handle_tool_call ~control_client)
      line
  in
  let expected =
    Some (`Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int 9);
      ("result", `Assoc [
        ("content", `List [
          `Assoc [
            ("type", `String "text");
            ("text", `String "Bot is not running.");
          ];
        ]);
        ("isError", `Bool true);
      ]);
    ])
  in
  match actual, expected with
  | Some actual, Some expected ->
    check_json "MCP error response" expected actual
  | _ -> failf "expected MCP error response"

let test_server_wraps_list_sessions_result () =
  let control_client =
    Control_client.make ~request:(fun _request ->
      Ok (list_sessions_response [session "alpha" "claude" 7 "987"]))
  in
  let line =
    {|{"jsonrpc":"2.0","id":10,"method":"tools/call","params":{"name":"list_sessions"}}|}
  in
  let actual =
    Mcp_server.handle_line
      ~handle_tool_call:(Mcp_handler.handle_tool_call ~control_client)
      line
  in
  let expected =
    Some (`Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int 10);
      ("result", `Assoc [
        ("content", `List [
          `Assoc [
            ("type", `String "text");
            ("text",
             `String "- **alpha** / claude — 7 messages (thread: <#987>)");
          ];
        ]);
      ]);
    ])
  in
  match actual, expected with
  | Some actual, Some expected -> check_json "MCP response" expected actual
  | _ -> failf "expected MCP response"

let temp_dir_counter = ref 0

let make_short_temp_dir () =
  let rec loop attempts =
    if attempts > 100 then failf "could not create temporary socket dir";
    incr temp_dir_counter;
    let dir =
      Filename.concat "/tmp"
        (Printf.sprintf "da-mcp-%d-%d" (Unix.getpid ()) !temp_dir_counter)
    in
    match Unix.mkdir dir 0o700 with
    | () -> dir
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> loop (attempts + 1)
  in
  loop 0

let with_temp_socket f =
  let dir = make_short_temp_dir () in
  let socket_path = Filename.concat dir "control.sock" in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.unlink socket_path with Unix.Unix_error _ -> ());
      (try Unix.rmdir dir with Unix.Unix_error _ -> ()))
    (fun () -> f socket_path)

let write_all fd data =
  let length = String.length data in
  let rec loop offset =
    if offset < length then begin
      let written = Unix.write_substring fd data offset (length - offset) in
      if written = 0 then raise End_of_file;
      loop (offset + written)
    end
  in
  loop 0

let read_line_fd fd =
  let buffer = Buffer.create 256 in
  let byte = Bytes.create 1 in
  let rec loop () =
    match Unix.read fd byte 0 1 with
    | 0 -> Buffer.contents buffer
    | _ ->
      let ch = Bytes.get byte 0 in
      if Char.equal ch '\n' then Buffer.contents buffer
      else begin
        Buffer.add_char buffer ch;
        loop ()
      end
  in
  loop ()

let exit_child code =
  flush_all ();
  exit code

let serve_one_control_response server_fd response =
  try
    let client_fd, _ = Unix.accept server_fd in
    Fun.protect
      ~finally:(fun () -> Unix.close client_fd)
      (fun () ->
        let request = Yojson.Safe.from_string (read_line_fd client_fd) in
        let method_name =
          match request with
          | `Assoc fields ->
            (match List.assoc_opt "method" fields with
             | Some (`String method_name) -> method_name
             | _ -> "")
          | _ -> ""
        in
        let response =
          if String.equal method_name "list_projects" then response
          else `Assoc [("error", `String "bad method")]
        in
        write_all client_fd (Yojson.Safe.to_string response ^ "\n"));
    exit_child 0
  with _ ->
    exit_child 1

let wait_for_child pid =
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _, Unix.WEXITED code -> failf "child exited %d" code
  | _, Unix.WSIGNALED signal -> failf "child signaled %d" signal
  | _, Unix.WSTOPPED signal -> failf "child stopped %d" signal

let test_control_client_unix_roundtrip () =
  with_temp_socket (fun socket_path ->
    let server_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close server_fd)
      (fun () ->
        Unix.bind server_fd (Unix.ADDR_UNIX socket_path);
        Unix.listen server_fd 1;
        match Unix.fork () with
        | 0 ->
          serve_one_control_response server_fd
            (list_projects_response [project "alpha" "/tmp/alpha"])
        | pid ->
          let control_client = Control_client.unix ~socket_path () in
          let result =
            Control_client.request_method control_client
              Control_api.List_projects_id
          in
          wait_for_child pid;
          match result with
          | Error message -> failf "control client error: %s" message
          | Ok response ->
            check_json "control response"
              (list_projects_response [project "alpha" "/tmp/alpha"])
              response))

let test_control_client_missing_socket () =
  with_temp_socket (fun socket_path ->
    let control_client = Control_client.unix ~socket_path () in
    Alcotest.(check (result string string))
      "missing socket"
      (Error "Bot is not running (control socket not found).")
      (Control_client.request_method control_client
         Control_api.List_projects_id
       |> Result.map Yojson.Safe.to_string))

let () =
  Alcotest.run "mcp_handler" [
    ("formatter", [
      Alcotest.test_case "list_projects matches Python" `Quick
        test_format_list_projects_matches_python;
      Alcotest.test_case "list_projects control error" `Quick
        test_format_list_projects_control_error;
      Alcotest.test_case "list_sessions matches Python" `Quick
        test_format_list_sessions_matches_python;
      Alcotest.test_case "list_sessions control error" `Quick
        test_format_list_sessions_control_error;
    ]);
    ("handler", [
      Alcotest.test_case "list_projects requests control API" `Quick
        test_handler_list_projects_requests_control_api;
      Alcotest.test_case "list_sessions requests control API" `Quick
        test_handler_list_sessions_requests_control_api;
      Alcotest.test_case "unsupported tool" `Quick
        test_handler_unsupported_tool;
      Alcotest.test_case "server wraps list_projects result" `Quick
        test_server_wraps_list_projects_result;
      Alcotest.test_case "server wraps list_projects control error" `Quick
        test_server_wraps_list_projects_control_error;
      Alcotest.test_case "server wraps list_sessions result" `Quick
        test_server_wraps_list_sessions_result;
    ]);
    ("control client", [
      Alcotest.test_case "unix roundtrip" `Quick
        test_control_client_unix_roundtrip;
      Alcotest.test_case "missing socket" `Quick
        test_control_client_missing_socket;
    ]);
  ]
