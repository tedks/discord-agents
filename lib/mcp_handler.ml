(** Bridge MCP tool calls to the bot control API. *)

let unsupported_tool name =
  Error (
    Printf.sprintf
      "OCaml MCP tools/call is not wired yet for tool: %s"
      name
  )

let handle_list_projects control_client =
  match Control_client.request_method control_client Control_api.List_projects_id with
  | Error _ as error -> error
  | Ok response -> Mcp_formatter.format_list_projects response

let handle_tool_call ~control_client (call : Mcp_server.tool_call) =
  match call.name with
  | "list_projects" -> handle_list_projects control_client
  | name -> unsupported_tool name
