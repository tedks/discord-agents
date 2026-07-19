(** Bridge MCP tool calls to the bot control API. *)

let unsupported_tool name =
  Error (
    Printf.sprintf
      "OCaml MCP tools/call is not wired yet for tool: %s"
      name
  )

let request_and_format control_client method_id formatter =
  match Control_client.request_method control_client method_id with
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

let handle_tool_call ~control_client (call : Mcp_server.tool_call) =
  match call.name with
  | "list_projects" -> handle_list_projects control_client
  | "list_sessions" -> handle_list_sessions control_client
  | name -> unsupported_tool name
