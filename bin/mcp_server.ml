let handle_tool_call (call : Discord_agents.Mcp_server.tool_call) =
  Error (
    Printf.sprintf
      "OCaml MCP tools/call is not wired yet for tool: %s"
      call.name
  )

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
