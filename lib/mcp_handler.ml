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

(* Python matches the retry trigger with [in] on a lowercased message
   (scripts/mcp-server.py), so the comparison is case-insensitive on
   both sides. *)
let response_error_contains fragment = function
  | `Assoc fields ->
    (match List.assoc_opt "error" fields with
     | Some (`String message) ->
       Resource.contains_substring
         ~haystack:(String.lowercase_ascii message)
         ~needle:(String.lowercase_ascii fragment)
     | _ -> false)
  | _ -> false

let handle_list_projects control_client =
  request_and_format control_client
    Control_api.List_projects_id
    Mcp_formatter.format_list_projects

let handle_list_sessions control_client =
  request_and_format control_client
    Control_api.List_sessions_id
    Mcp_formatter.format_list_sessions

(* Wire-format note: Python's [control_request] drops falsy params, so a
   no-argument call goes out with no "params" key at all, while we always
   forward the arguments object — [{}] for the common no-argument case.
   [Control_api.hours_param] maps absent, empty and non-integer alike to
   the 24h default, so the two are equivalent; pinned by
   test_control_api's hours_param cases. *)
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

let handle_start_session control_client params =
  let request_start () =
    Control_client.request_method ~params control_client
      Control_api.Start_session_id
  in
  match request_start () with
  | Error _ as error -> error
  | Ok response when response_error_contains "no project matching" response ->
    ignore (Control_client.request_method control_client
              Control_api.Refresh_projects_id);
    (match request_start () with
     | Error _ as error -> error
     | Ok response -> Mcp_formatter.format_start_session response)
  | Ok response -> Mcp_formatter.format_start_session response

let handle_resume_session control_client params =
  request_and_format ~params control_client
    Control_api.Resume_session_id
    Mcp_formatter.format_resume_session

let handle_send_message control_client params =
  request_and_format ~params control_client
    Control_api.Send_message_id
    Mcp_formatter.format_send_message

let handle_stop_session control_client params =
  request_and_format ~params control_client
    Control_api.Stop_session_id
    Mcp_formatter.format_stop_session

let handle_default_agent control_client params =
  request_and_format ~params control_client
    Control_api.Default_agent_id
    (Mcp_formatter.format_default_agent ~arguments:params)

let handle_rescue_agent control_client params =
  request_and_format ~params control_client
    Control_api.Rescue_agent_id
    (Mcp_formatter.format_rescue_agent ~arguments:params)

let handle_get_agent_config control_client params =
  request_and_format ~params control_client
    Control_api.Get_agent_config_id
    Mcp_formatter.format_get_agent_config

let handle_set_model control_client params =
  request_and_format ~params control_client
    Control_api.Set_model_id
    Mcp_formatter.format_set_model

let handle_set_effort control_client params =
  request_and_format ~params control_client
    Control_api.Set_effort_id
    Mcp_formatter.format_set_effort

let handle_set_goal control_client params =
  request_and_format ~params control_client
    Control_api.Set_goal_id
    Mcp_formatter.format_set_goal

let handle_start_login_flow control_client params =
  request_and_format ~params control_client
    Control_api.Start_login_flow_id
    Mcp_formatter.format_start_login_flow

let handle_import_project control_client params =
  request_and_format ~params control_client
    Control_api.Import_project_id
    Mcp_formatter.format_import_project

let handle_restart_bot control_client =
  request_and_format control_client
    Control_api.Restart_id
    Mcp_formatter.format_restart_bot

let handle_rename_thread control_client params =
  request_and_format ~params control_client
    Control_api.Rename_thread_id
    Mcp_formatter.format_rename_thread

let handle_cleanup_channels control_client =
  request_and_format control_client
    Control_api.Cleanup_channels_id
    Mcp_formatter.format_cleanup_channels

let handle_refresh_projects control_client =
  request_and_format control_client
    Control_api.Refresh_projects_id
    Mcp_formatter.format_refresh_projects

let handle_tool_call ~control_client (call : Mcp_server.tool_call) =
  match call.name with
  | "start_session" -> handle_start_session control_client call.arguments
  | "list_projects" -> handle_list_projects control_client
  | "import_project" -> handle_import_project control_client call.arguments
  | "list_sessions" -> handle_list_sessions control_client
  | "send_message" -> handle_send_message control_client call.arguments
  | "stop_session" -> handle_stop_session control_client call.arguments
  | "list_claude_sessions" ->
    handle_list_claude_sessions control_client call.arguments
  | "list_codex_sessions" ->
    handle_list_codex_sessions control_client call.arguments
  | "list_gemini_sessions" ->
    handle_list_gemini_sessions control_client call.arguments
  | "default_agent" -> handle_default_agent control_client call.arguments
  | "rescue_agent" -> handle_rescue_agent control_client call.arguments
  | "get_agent_config" ->
    handle_get_agent_config control_client call.arguments
  | "set_model" -> handle_set_model control_client call.arguments
  | "set_effort" -> handle_set_effort control_client call.arguments
  | "set_goal" -> handle_set_goal control_client call.arguments
  | "start_login_flow" ->
    handle_start_login_flow control_client call.arguments
  | "resume_session" -> handle_resume_session control_client call.arguments
  | "restart_bot" -> handle_restart_bot control_client
  | "rename_thread" -> handle_rename_thread control_client call.arguments
  | "cleanup_channels" -> handle_cleanup_channels control_client
  | "refresh_projects" -> handle_refresh_projects control_client
  | name -> unsupported_tool name
