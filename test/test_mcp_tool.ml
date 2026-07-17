module Control_api = Discord_agents.Control_api
module Mcp_tool = Discord_agents.Mcp_tool

let failf fmt = Format.kasprintf (fun message -> Alcotest.fail message) fmt

let tool_id_testable =
  Alcotest.testable
    (fun fmt id -> Format.fprintf fmt "%s" (Mcp_tool.string_of_id id))
    ( = )

let method_id_testable =
  Alcotest.testable
    (fun fmt id -> Format.fprintf fmt "%s" (Control_api.string_of_method_id id))
    ( = )

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
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Buffer.contents buf
  | Unix.WEXITED n -> failf "command exited %d: %s" n command
  | Unix.WSIGNALED n -> failf "command signaled %d: %s" n command
  | Unix.WSTOPPED n -> failf "command stopped %d: %s" n command

let python_tools_json () =
  let script = repo_file "scripts/mcp-server.py" in
  let program =
    "import json, runpy, sys; "
    ^ "ns = runpy.run_path(sys.argv[1]); "
    ^ "print(json.dumps(ns['TOOLS'], sort_keys=True))"
  in
  let command =
    Printf.sprintf "python3 -c %s %s"
      (Filename.quote program)
      (Filename.quote script)
  in
  read_process_output command
  |> Yojson.Safe.from_string

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

let unique values =
  let seen = Hashtbl.create 32 in
  values
  |> List.filter (fun value ->
    if Hashtbl.mem seen value then false
    else begin
      Hashtbl.add seen value ();
      true
    end)

let sorted_strings values =
  List.sort String.compare values

let expected_tool_ids = [
  Mcp_tool.Start_session;
  List_projects;
  Import_project;
  List_sessions;
  Send_message;
  Stop_session;
  List_claude_sessions;
  List_codex_sessions;
  List_gemini_sessions;
  Default_agent;
  Rescue_agent;
  Get_agent_config;
  Set_model;
  Set_effort;
  Set_goal;
  Start_login_flow;
  Resume_session;
  Restart_bot;
  Rename_thread;
  Cleanup_channels;
  Refresh_projects;
]

let test_specs_cover_tool_ids () =
  let actual = List.map Mcp_tool.spec_id Mcp_tool.all_specs in
  Alcotest.(check (list tool_id_testable))
    "MCP tool specs cover expected ids"
    expected_tool_ids
    actual

let test_tool_names_are_unique () =
  let seen = Hashtbl.create 32 in
  Mcp_tool.all_specs
  |> List.iter (fun spec ->
    let name = Mcp_tool.tool_name spec in
    if Hashtbl.mem seen name then
      failf "duplicate MCP tool name in specs: %s" name
    else
      Hashtbl.add seen name (Mcp_tool.spec_id spec))

let test_tool_definitions_match_python () =
  check_json
    "MCP tool definitions"
    (python_tools_json ())
    Mcp_tool.tool_definitions_json

let test_control_methods_are_exposed () =
  let tool_methods =
    Mcp_tool.all_specs
    |> List.map Mcp_tool.control_method
  in
  Alcotest.(check (list string))
    "MCP tools cover exposed control methods"
    (Control_api.mcp_control_method_names |> sorted_strings)
    (tool_methods
     |> List.map Control_api.string_of_method_id
     |> sorted_strings);
  let missing =
    tool_methods
    |> List.filter (fun id ->
      match Control_api.method_spec_of_id id with
      | Some spec -> not (Control_api.method_spec_mcp_exposed spec)
      | None -> true)
  in
  Alcotest.(check (list method_id_testable))
    "all tool control methods are MCP-exposed"
    []
    missing

let test_tool_names_match_python_order () =
  let python_names =
    match python_tools_json () with
    | `List tools ->
      tools
      |> List.map (function
        | `Assoc fields ->
          (match List.assoc_opt "name" fields with
           | Some (`String name) -> name
           | _ -> failf "Python tool without string name")
        | _ -> failf "Python tool entry is not an object")
    | _ -> failf "Python TOOLS is not a list"
  in
  let ocaml_names = List.map Mcp_tool.tool_name Mcp_tool.all_specs in
  Alcotest.(check (list string))
    "MCP tool names"
    python_names
    ocaml_names

let test_tool_control_methods_match_control_metadata () =
  Mcp_tool.all_specs
  |> List.iter (fun spec ->
    let control_method = Mcp_tool.control_method spec in
    match Control_api.method_spec_of_id control_method with
    | None ->
      failf "missing control method %s for tool %s"
        (Control_api.string_of_method_id control_method)
        (Mcp_tool.tool_name spec)
    | Some method_spec ->
      Alcotest.(check int)
        (Mcp_tool.tool_name spec)
        (Control_api.method_spec_timeout_s method_spec)
        (Mcp_tool.control_method_timeout_s spec));
  let tool_control_names =
    Mcp_tool.all_specs
    |> List.map Mcp_tool.control_method_name
    |> unique
    |> sorted_strings
  in
  Alcotest.(check (list string))
    "control method names"
    (sorted_strings Control_api.mcp_control_method_names)
    tool_control_names

let () =
  Alcotest.run "mcp_tool" [
    ("metadata", [
      Alcotest.test_case "specs cover tool ids" `Quick
        test_specs_cover_tool_ids;
      Alcotest.test_case "tool names are unique" `Quick
        test_tool_names_are_unique;
      Alcotest.test_case "tool definitions match Python" `Quick
        test_tool_definitions_match_python;
      Alcotest.test_case "tool names match Python order" `Quick
        test_tool_names_match_python_order;
      Alcotest.test_case "control methods are exposed" `Quick
        test_control_methods_are_exposed;
      Alcotest.test_case "tool control methods match control metadata"
        `Quick test_tool_control_methods_match_control_metadata;
    ]);
  ]
