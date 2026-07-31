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

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let n = in_channel_length ic in
    really_input_string ic n)

let collect_matches regexp group text =
  let rec loop pos acc =
    match Str.search_forward regexp text pos with
    | exception Not_found -> List.rev acc
    | _ ->
      loop (Str.match_end ())
        (Str.matched_group group text :: acc)
  in
  loop 0 []

let unique_preserving_order values =
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

let mcp_server_py =
  lazy (read_file (repo_file "scripts/mcp-server.py"))

let python_control_methods () =
  collect_matches
    (Str.regexp "control_request(\"\\([^\"]+\\)\"")
    1
    (Lazy.force mcp_server_py)
  |> unique_preserving_order

let python_default_timeout_s () =
  let regexp =
    Str.regexp "def control_request(method, params=None, timeout=\\([0-9]+\\))"
  in
  match Str.search_forward regexp (Lazy.force mcp_server_py) 0 with
  | exception Not_found -> failf "could not find Python control_request timeout"
  | _ -> int_of_string (Str.matched_group 1 (Lazy.force mcp_server_py))

let python_explicit_timeouts () =
  let regexp =
    Str.regexp "control_request(\"\\([^\"]+\\)\"[^\n]*timeout=\\([0-9]+\\)"
  in
  let text = Lazy.force mcp_server_py in
  let rec loop pos acc =
    match Str.search_forward regexp text pos with
    | exception Not_found -> acc
    | _ ->
      let method_name = Str.matched_group 1 text in
      let timeout_s = int_of_string (Str.matched_group 2 text) in
      let acc =
        match List.assoc_opt method_name acc with
        | None -> (method_name, timeout_s) :: acc
        | Some existing when existing = timeout_s -> acc
        | Some existing ->
          failf "conflicting Python timeout for %s: %d and %d"
            method_name existing timeout_s
      in
      loop (Str.match_end ()) acc
  in
  loop 0 []

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
      Alcotest.(check method_id_testable)
        (Control_api.string_of_method_id id)
        id
        (Control_api.method_spec_id spec))

(* These lists intentionally duplicate the descriptor table. Changing
   control methods should require a conscious update to the metadata tests. *)
let expected_mcp_exposed_method_ids = [
  Control_api.List_projects_id;
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

let test_mcp_exposure_is_explicit () =
  let actual =
    Control_api.mcp_exposed_method_specs
    |> List.map Control_api.method_spec_id
  in
  Alcotest.(check (list method_id_testable))
    "MCP-exposed method ids"
    expected_mcp_exposed_method_ids
    actual;
  Alcotest.(check (list string))
    "MCP-visible control method names"
    (List.map Control_api.string_of_method_id expected_mcp_exposed_method_ids)
    Control_api.mcp_control_method_names

let test_python_mcp_control_methods_match_descriptors () =
  Alcotest.(check (list string))
    "Python MCP control methods"
    (sorted_strings Control_api.mcp_control_method_names)
    (sorted_strings (python_control_methods ()))

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

let test_timeout_metadata_matches_python_mcp_client () =
  let default_timeout_s = python_default_timeout_s () in
  let explicit_timeouts = python_explicit_timeouts () in
  Control_api.mcp_exposed_method_specs
  |> List.iter (fun spec ->
    let name = Control_api.method_spec_name spec in
    let expected =
      Option.value (List.assoc_opt name explicit_timeouts)
        ~default:default_timeout_s
    in
    Alcotest.(check int)
      name
      expected
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

(* The OCaml MCP handler forwards an empty params object for a
   no-argument recent-session call, where Python's control_request omits
   "params" entirely. Both must land on the same 24h default, or the two
   MCP implementations would disagree on the most common call there is. *)
let test_hours_param_defaults () =
  Alcotest.(check int) "absent params" 24 (Control_api.hours_param None);
  Alcotest.(check int) "empty params" 24
    (Control_api.hours_param (Some (`Assoc [])));
  Alcotest.(check int) "explicit hours" 6
    (Control_api.hours_param (Some (`Assoc [("hours", `Int 6)])));
  Alcotest.(check int) "non-integer hours" 24
    (Control_api.hours_param (Some (`Assoc [("hours", `String "6")])));
  Alcotest.(check int) "non-object params" 24
    (Control_api.hours_param (Some (`List [])))

let () =
  Alcotest.run "control_api" [
    ("params", [
      Alcotest.test_case "hours_param defaults" `Quick
        test_hours_param_defaults;
    ]);
    ("metadata", [
      Alcotest.test_case "specs cover method ids" `Quick
        test_specs_cover_method_ids;
      Alcotest.test_case "spec names are unique" `Quick
        test_spec_names_are_unique;
      Alcotest.test_case "lookup round trips" `Quick
        test_lookup_round_trips;
      Alcotest.test_case "MCP exposure is explicit" `Quick
        test_mcp_exposure_is_explicit;
      Alcotest.test_case "Python MCP methods match descriptors" `Quick
        test_python_mcp_control_methods_match_descriptors;
      Alcotest.test_case "timeouts match expected snapshot" `Quick
        test_timeout_metadata_matches_current_mcp_client;
      Alcotest.test_case "timeouts match Python MCP client" `Quick
        test_timeout_metadata_matches_python_mcp_client;
      Alcotest.test_case "mutability metadata is stable" `Quick
        test_mutability_metadata_is_stable;
    ]);
  ]
