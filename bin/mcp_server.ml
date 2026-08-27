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

let () =
  try
    while true do
      match input_line stdin with
      | line ->
        (match Discord_agents.Mcp_server.handle_line ~handle_tool_call line with
         | None -> ()
         | Some response -> write_response response)
    done
  with End_of_file -> ()
