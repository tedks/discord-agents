(** Bridge MCP tool calls to the bot control API. *)

let unsupported_tool name =
  Error (
    Printf.sprintf
      "OCaml MCP tools/call is not wired yet for tool: %s"
      name
  )

let request_and_format ?params control_client method_id formatter =
  match Control_client.request_method ?params control_client method_id with
  | Error _ as error -> error
  | Ok response -> formatter response

let handle_list_projects control_client =
  request_and_format control_client
    Control_api.List_projects_id
    Mcp_formatter.format_list_projects

let handle_list_sessions control_client =
  request_and_format control_client
    Control_api.List_sessions_id
    Mcp_formatter.format_list_sessions

let handle_list_claude_sessions control_client params =
  request_and_format ~params control_client
    Control_api.List_claude_sessions_id
    Mcp_formatter.format_list_claude_sessions

let handle_list_codex_sessions control_client params =
  request_and_format ~params control_client
    Control_api.List_codex_sessions_id
    Mcp_formatter.format_list_codex_sessions

let handle_list_gemini_sessions control_client params =
  request_and_format ~params control_client
    Control_api.List_gemini_sessions_id
    Mcp_formatter.format_list_gemini_sessions

let handle_tool_call ~control_client (call : Mcp_server.tool_call) =
  match call.name with
  | "list_projects" -> handle_list_projects control_client
  | "list_sessions" -> handle_list_sessions control_client
  | "list_claude_sessions" ->
    handle_list_claude_sessions control_client call.arguments
  | "list_codex_sessions" ->
    handle_list_codex_sessions control_client call.arguments
  | "list_gemini_sessions" ->
    handle_list_gemini_sessions control_client call.arguments
  | name -> unsupported_tool name
