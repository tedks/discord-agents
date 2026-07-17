module Control_api = Discord_agents.Control_api

let method_id_testable =
  Alcotest.testable
    (fun fmt id -> Format.fprintf fmt "%s" (Control_api.string_of_method_id id))
    ( = )

let mutability_testable =
  Alcotest.testable
    (fun fmt mutability ->
      Format.fprintf fmt "%s" (Control_api.string_of_mutability mutability))
    ( = )

let failf fmt = Format.kasprintf (fun message -> Alcotest.fail message) fmt

let expected_method_ids = [
  Control_api.Health_id;
  List_projects_id;
  List_sessions_id;
  Import_project_id;
  List_claude_sessions_id;
  List_codex_sessions_id;
  List_gemini_sessions_id;
  Start_session_id;
  Resume_session_id;
  Stop_session_id;
  Send_message_id;
  Default_agent_id;
  Rescue_agent_id;
  Get_agent_config_id;
  Set_model_id;
  Set_effort_id;
  Set_goal_id;
  Start_login_flow_id;
  Restart_id;
  Rename_thread_id;
  Cleanup_channels_id;
  Refresh_projects_id;
]

let test_specs_cover_method_ids () =
  let actual = List.map Control_api.method_spec_id Control_api.all_method_specs in
  Alcotest.(check (list method_id_testable))
    "control method specs cover expected ids"
    expected_method_ids
    actual

let test_spec_names_are_unique () =
  let seen = Hashtbl.create 32 in
  Control_api.all_method_specs
  |> List.iter (fun spec ->
    let name = Control_api.method_spec_name spec in
    if Hashtbl.mem seen name then
      failf "duplicate control method name in specs: %s" name
    else
      Hashtbl.add seen name (Control_api.method_spec_id spec))

let test_names_match_method_ids () =
  Control_api.all_method_specs
  |> List.iter (fun spec ->
    let id = Control_api.method_spec_id spec in
    Alcotest.(check string)
      (Control_api.string_of_method_id id)
      (Control_api.string_of_method_id id)
      (Control_api.method_spec_name spec))

let test_lookup_round_trips () =
  Control_api.all_method_specs
  |> List.iter (fun spec ->
    let name = Control_api.method_spec_name spec in
    match Control_api.method_spec_of_name name with
    | None -> failf "missing control method lookup for %s" name
    | Some found ->
      Alcotest.(check method_id_testable)
        name
        (Control_api.method_spec_id spec)
        (Control_api.method_spec_id found));
  expected_method_ids
  |> List.iter (fun id ->
    match Control_api.method_spec_of_id id with
    | None ->
      failf "missing control method spec for %s"
        (Control_api.string_of_method_id id)
    | Some spec ->
      Alcotest.(check string)
        (Control_api.string_of_method_id id)
        (Control_api.string_of_method_id id)
        (Control_api.method_spec_name spec))

let expected_mcp_control_method_names = [
  "list_projects";
  "list_sessions";
  "import_project";
  "list_claude_sessions";
  "list_codex_sessions";
  "list_gemini_sessions";
  "start_session";
  "resume_session";
  "stop_session";
  "send_message";
  "default_agent";
  "rescue_agent";
  "get_agent_config";
  "set_model";
  "set_effort";
  "set_goal";
  "start_login_flow";
  "restart";
  "rename_thread";
  "cleanup_channels";
  "refresh_projects";
]

let test_mcp_exposure_is_explicit () =
  Alcotest.(check (list string))
    "MCP-visible control method names"
    expected_mcp_control_method_names
    Control_api.mcp_control_method_names;
  Control_api.all_method_specs
  |> List.iter (fun spec ->
    let id = Control_api.method_spec_id spec in
    let expected = id <> Control_api.Health_id in
    Alcotest.(check bool)
      (Control_api.string_of_method_id id)
      expected
      (Control_api.method_spec_mcp_exposed spec))

let expected_timeout_s = function
  | Control_api.Import_project_id -> 300
  | Start_session_id
  | Resume_session_id -> 120
  | _ -> 60

let test_timeout_metadata_matches_current_mcp_client () =
  Control_api.all_method_specs
  |> List.iter (fun spec ->
    let id = Control_api.method_spec_id spec in
    Alcotest.(check int)
      (Control_api.string_of_method_id id)
      (expected_timeout_s id)
      (Control_api.method_spec_timeout_s spec))

let expected_mutability = function
  | Control_api.Health_id
  | List_projects_id
  | List_sessions_id
  | List_claude_sessions_id
  | List_codex_sessions_id
  | List_gemini_sessions_id
  | Get_agent_config_id
  | Start_login_flow_id -> Control_api.Read_only
  | Start_session_id
  | Resume_session_id
  | Stop_session_id
  | Send_message_id
  | Set_model_id
  | Set_effort_id
  | Set_goal_id -> Mutates_session
  | Default_agent_id
  | Rescue_agent_id -> Mutates_runtime
  | Import_project_id
  | Refresh_projects_id -> Mutates_projects
  | Rename_thread_id
  | Cleanup_channels_id -> Mutates_discord
  | Restart_id -> Restarts_process

let test_mutability_metadata_is_stable () =
  Control_api.all_method_specs
  |> List.iter (fun spec ->
    let id = Control_api.method_spec_id spec in
    Alcotest.(check mutability_testable)
      (Control_api.string_of_method_id id)
      (expected_mutability id)
      (Control_api.method_spec_mutability spec))

let () =
  Alcotest.run "control_api" [
    ("metadata", [
      Alcotest.test_case "specs cover method ids" `Quick
        test_specs_cover_method_ids;
      Alcotest.test_case "spec names are unique" `Quick
        test_spec_names_are_unique;
      Alcotest.test_case "names match method ids" `Quick
        test_names_match_method_ids;
      Alcotest.test_case "lookup round trips" `Quick
        test_lookup_round_trips;
      Alcotest.test_case "MCP exposure is explicit" `Quick
        test_mcp_exposure_is_explicit;
      Alcotest.test_case "timeouts match current MCP client" `Quick
        test_timeout_metadata_matches_current_mcp_client;
      Alcotest.test_case "mutability metadata is stable" `Quick
        test_mutability_metadata_is_stable;
    ]);
  ]
