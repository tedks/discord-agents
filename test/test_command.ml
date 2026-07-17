module Command = Discord_agents.Command

let id_testable =
  Alcotest.testable
    (fun fmt id -> Format.fprintf fmt "%s" (Command.string_of_id id))
    ( = )

let failf fmt = Format.kasprintf (fun message -> Alcotest.fail message) fmt

let check_parse_id raw expected =
  let actual = Command.(id_of (parse raw)) in
  Alcotest.(check id_testable) raw expected actual

let test_spec_names_are_unique () =
  let seen = Hashtbl.create 32 in
  Command.all_specs
  |> List.iter (fun spec ->
    Command.names spec
    |> List.iter (fun name ->
      if Hashtbl.mem seen name then
        failf "duplicate command name in specs: %s" name
      else
        Hashtbl.add seen name (Command.spec_id spec)))

let test_no_arg_spec_names_parse_to_spec_id () =
  Command.all_specs
  |> List.iter (fun spec ->
    match Command.no_arg_command spec with
    | None -> ()
    | Some expected ->
      let expected_id = Command.id_of expected in
      Command.names spec
      |> List.iter (fun name -> check_parse_id ("!" ^ name) expected_id))

let test_start_without_project_keeps_project_list_shortcut () =
  check_parse_id "!start" Command.List_projects_id

let require_spec id =
  match Command.spec_of_id id with
  | Some spec -> spec
  | None ->
    failf "missing command spec for %s" (Command.string_of_id id)

let check_all_spec_names_parse ~id ~raw_of_name =
  let spec = require_spec id in
  Command.names spec
  |> List.iter (fun name -> check_parse_id (raw_of_name name) id)

let test_argument_spec_names_parse_to_spec_id () =
  [
    Command.Start_agent_id, (fun name -> Printf.sprintf "!%s demo" name);
    Start_agent_id, (fun name -> Printf.sprintf "!%s demo codex" name);
    Import_project_id,
    (fun name -> Printf.sprintf "!%s https://github.com/tedks/demo.git" name);
    Import_project_id,
    (fun name -> Printf.sprintf
       "!%s https://github.com/tedks/demo.git local-demo" name);
    Default_agent_id, (fun name -> Printf.sprintf "!%s codex" name);
    Rescue_agent_id, (fun name -> Printf.sprintf "!%s codex" name);
    Rescue_agent_id, (fun name -> Printf.sprintf "!%s off" name);
    Session_agent_id, (fun name -> Printf.sprintf "!%s codex" name);
    Resume_session_id, (fun name -> Printf.sprintf "!%s session-1" name);
    Resume_session_id, (fun name -> Printf.sprintf "!%s codex session-1" name);
    Fork_session_id, (fun name -> Printf.sprintf "!%s --move" name);
    Fork_session_id, (fun name -> Printf.sprintf "!%s --no-move" name);
    Stop_session_id, (fun name -> Printf.sprintf "!%s 123456789012" name);
    Rename_thread_id, (fun name -> Printf.sprintf "!%s new-name" name);
    Wrapping_id, (fun name -> Printf.sprintf "!%s 80" name);
    Lines_id, (fun name -> Printf.sprintf "!%s 80" name);
    Scroll_id, (fun name -> Printf.sprintf "!%s 2" name);
  ]
  |> List.iter (fun (id, raw_of_name) ->
    check_all_spec_names_parse ~id ~raw_of_name)

let test_drain_policy_matches_existing_read_only_set () =
  let allowed = [
    Command.List_projects;
    List_sessions;
    List_claude_sessions;
    List_codex_sessions;
    List_gemini_sessions;
    Status;
    Help;
  ] in
  allowed
  |> List.iter (fun cmd ->
    Alcotest.(check bool)
      (Command.string_of_id (Command.id_of cmd))
      true
      (Command.is_allowed_during_drain cmd));
  let blocked = [
    Command.Start_agent { project = "p"; kind = None };
    Import_project { url = "https://github.com/owner/repo.git"; name = None };
    Resume_session { session_id = "abc"; kind = None };
    Fork_session { move = None };
    Reset_session;
    Stop_session { thread_id = "123" };
    Interrupt_session;
    Cleanup_channels;
    Default_agent None;
    Rescue_agent None;
    Session_agent None;
    Restart;
    Refresh;
    Rename_thread { thread_id = None; name = "x" };
    Desktop;
    Mobile;
    Wrapping None;
    Lines None;
    Scroll None;
    Unknown "!bogus";
  ] in
  blocked
  |> List.iter (fun cmd ->
    Alcotest.(check bool)
      (Command.string_of_id (Command.id_of cmd))
      false
      (Command.is_allowed_during_drain cmd))

let expected_spec_ids = [
  Command.List_projects_id;
  List_sessions_id;
  List_claude_sessions_id;
  List_codex_sessions_id;
  List_gemini_sessions_id;
  Start_agent_id;
  Import_project_id;
  Default_agent_id;
  Rescue_agent_id;
  Session_agent_id;
  Resume_session_id;
  Fork_session_id;
  Reset_session_id;
  Stop_session_id;
  Interrupt_session_id;
  Rename_thread_id;
  Status_id;
  Refresh_id;
  Cleanup_channels_id;
  Restart_id;
  Desktop_id;
  Mobile_id;
  Wrapping_id;
  Lines_id;
  Scroll_id;
  Help_id;
]

let test_specs_cover_command_ids () =
  let actual = List.map Command.spec_id Command.all_specs in
  Alcotest.(check (list id_testable))
    "command specs cover expected ids"
    expected_spec_ids
    actual

let () =
  Alcotest.run "command" [
    ("metadata", [
      Alcotest.test_case "spec names are unique" `Quick
        test_spec_names_are_unique;
      Alcotest.test_case "no-arg spec names parse to spec id" `Quick
        test_no_arg_spec_names_parse_to_spec_id;
      Alcotest.test_case "start without project remains project list shortcut"
        `Quick test_start_without_project_keeps_project_list_shortcut;
      Alcotest.test_case "argument spec names parse to spec id" `Quick
        test_argument_spec_names_parse_to_spec_id;
      Alcotest.test_case "drain policy matches existing read-only set" `Quick
        test_drain_policy_matches_existing_read_only_set;
      Alcotest.test_case "specs cover command ids" `Quick
        test_specs_cover_command_ids;
    ]);
  ]
