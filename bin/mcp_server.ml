let control_client =
  Discord_agents.Control_client.unix ()

let caller_thread_id =
  Sys.getenv_opt Discord_agents.Agent_process.session_thread_id_env

let handle_tool_call call =
  Discord_agents.Mcp_handler.handle_tool_call
    ~control_client ?caller_thread_id call

let write_response json =
  print_endline (Yojson.Safe.to_string json);
  flush stdout

let serve () =
  try
    while true do
      match input_line stdin with
      | line ->
        (match Discord_agents.Mcp_server.handle_line ~handle_tool_call line with
         | None -> ()
         | Some response -> write_response response)
    done
  with End_of_file -> ()

let () =
  match Array.to_list Sys.argv with
  | [_] -> serve ()
  | [_; "--check-control"; socket_path] ->
    (* An explicit socket prevents XDG/legacy fallback from checking a different
       bot when the intended writer's socket is absent. This mode does not read
       MCP requests or expose the health response's operational details. *)
    let client = Discord_agents.Control_client.unix ~socket_path () in
    if Discord_agents.Mcp_health.check client then begin
      print_string Discord_agents.Host_callback.reply;
      flush stdout
    end else begin
      prerr_endline "discord-agents-mcp: control endpoint unavailable";
      exit 1
    end
  | _ ->
    prerr_endline "usage: discord-agents-mcp [--check-control SOCKET]";
    exit 2
