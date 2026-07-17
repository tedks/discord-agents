let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_rf (Filename.concat path name));
    Unix.rmdir path
  | _ ->
    Unix.unlink path

let restore_env name = function
  | Some value -> Unix.putenv name value
  | None -> Unix.putenv name ""

let with_tmp_home f =
  let home = Filename.temp_dir "discord_agents_config_" "" in
  let old_home = Sys.getenv_opt "HOME" in
  let old_xdg_config_home = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Fun.protect
    ~finally:(fun () ->
      restore_env "HOME" old_home;
      restore_env "XDG_CONFIG_HOME" old_xdg_config_home;
      rm_rf home)
    (fun () ->
      Unix.putenv "HOME" home;
      Unix.putenv "XDG_CONFIG_HOME" "";
      f home)

let config_path home =
  Filename.concat home ".config/discord-agents/config.json"

let test_load_accepts_basic_schema_alias () =
  with_tmp_home (fun home ->
    let path = config_path home in
    Discord_agents.Resource.ensure_parent_dir path;
    Discord_agents.Resource.write_file_atomic path
      {|{
  "discord_token": "test-token",
  "guild_id": "guild-1",
  "base_dirs": ["~/Projects"]
}|};
    let config = Discord_agents.Config.load () in
    Alcotest.(check string) "token" "test-token" config.discord_token;
    Alcotest.(check string) "guild" "guild-1" config.guild_id;
    Alcotest.(check (list string)) "base dirs"
      ["~/Projects"] config.base_directories;
    Alcotest.(check (option string)) "control channel default"
      None config.control_channel_id;
    Alcotest.(check int) "projects default" 0 (List.length config.projects))

let test_load_prefers_canonical_base_directories () =
  with_tmp_home (fun home ->
    let path = config_path home in
    Discord_agents.Resource.ensure_parent_dir path;
    Discord_agents.Resource.write_file_atomic path
      {|{
  "discord_token": "test-token",
  "guild_id": "guild-1",
  "base_dirs": ["~/Old"],
  "base_directories": ["~/Projects"]
}|};
    let config = Discord_agents.Config.load () in
    Alcotest.(check (list string)) "canonical base directories"
      ["~/Projects"] config.base_directories)

let test_load_accepts_project_without_channel_id () =
  with_tmp_home (fun home ->
    let path = config_path home in
    Discord_agents.Resource.ensure_parent_dir path;
    Discord_agents.Resource.write_file_atomic path
      {|{
  "discord_token": "test-token",
  "guild_id": "guild-1",
  "projects": [{"name": "repo", "path": "/srv/repo"}]
}|};
    let config = Discord_agents.Config.load () in
    match config.projects with
    | [project] ->
      Alcotest.(check string) "project name" "repo" project.name;
      Alcotest.(check string) "project path" "/srv/repo" project.path;
      Alcotest.(check (option string)) "channel id default"
        None project.channel_id
    | _ -> Alcotest.fail "expected one project")

let test_load_env_token_replaces_blank_config_token () =
  with_tmp_home (fun home ->
    let old_token = Sys.getenv_opt "DISCORD_BOT_TOKEN" in
    Fun.protect
      ~finally:(fun () -> restore_env "DISCORD_BOT_TOKEN" old_token)
      (fun () ->
        let path = config_path home in
        Discord_agents.Resource.ensure_parent_dir path;
        Discord_agents.Resource.write_file_atomic path
          {|{
  "discord_token": "   ",
  "guild_id": "guild-1",
  "base_directories": ["~/Projects"]
}|};
        Unix.putenv "DISCORD_BOT_TOKEN" "env-token";
        let config = Discord_agents.Config.load () in
        Alcotest.(check string) "env token"
          "env-token" config.discord_token))

let has_error_substring needle errors =
  List.exists (fun err ->
    try ignore (Str.search_forward (Str.regexp_string needle) err 0); true
    with Not_found -> false
  ) errors

let test_validate_reports_required_fields () =
  let config = {
    Discord_agents.Config.default with
    base_directories = ["~/Projects"; ""];
    control_channel_id = Some " ";
    projects = [
      { name = ""; path = "/tmp/project"; channel_id = None };
      { name = "missing-path"; path = ""; channel_id = Some "" };
    ];
  } in
  match Discord_agents.Config.validate config with
  | Ok () -> Alcotest.fail "expected config validation errors"
  | Error errors ->
    Alcotest.(check bool) "requires token"
      true (has_error_substring "discord_token" errors);
    Alcotest.(check bool) "requires guild"
      true (has_error_substring "guild_id" errors);
    Alcotest.(check bool) "rejects empty base dir"
      true (has_error_substring "base_directories[1]" errors);
    Alcotest.(check bool) "rejects empty control channel"
      true (has_error_substring "control_channel_id" errors);
    Alcotest.(check bool) "rejects empty project name"
      true (has_error_substring "projects[0].name" errors);
    Alcotest.(check bool) "rejects empty project path"
      true (has_error_substring "projects[1].path" errors);
    Alcotest.(check bool) "rejects empty project channel"
      true (has_error_substring "projects[1].channel_id" errors)

let test_validate_smoke_test_allows_missing_guild () =
  let config = {
    Discord_agents.Config.default with
    discord_token = "test-token";
  } in
  match Discord_agents.Config.validate ~require_guild_id:false config with
  | Ok () -> ()
  | Error errors ->
    Alcotest.failf "expected smoke-test config to validate, got: %s"
      (String.concat "; " errors)

let test_load_result_reports_parse_errors () =
  with_tmp_home (fun home ->
    let path = config_path home in
    Discord_agents.Resource.ensure_parent_dir path;
    Discord_agents.Resource.write_file_atomic path
      {|{"discord_token": }|};
    match Discord_agents.Config.load_result () with
    | Ok _ -> Alcotest.fail "expected parse error"
    | Error errors ->
      Alcotest.(check bool) "reports invalid JSON"
        true (has_error_substring "invalid JSON" errors))

let test_load_result_reports_schema_errors () =
  with_tmp_home (fun home ->
    let path = config_path home in
    Discord_agents.Resource.ensure_parent_dir path;
    Discord_agents.Resource.write_file_atomic path
      {|{"discord_token": 42}|};
    match Discord_agents.Config.load_result () with
    | Ok _ -> Alcotest.fail "expected schema error"
    | Error errors ->
      Alcotest.(check bool) "reports field type error"
        true (has_error_substring "discord_token: expected string" errors))

let test_load_result_reports_unknown_fields () =
  with_tmp_home (fun home ->
    let path = config_path home in
    Discord_agents.Resource.ensure_parent_dir path;
    Discord_agents.Resource.write_file_atomic path
      {|{
  "discord_token": "test-token",
  "guild_id": "guild-1",
  "base_directores": ["~/Projects"]
}|};
    match Discord_agents.Config.load_result () with
    | Ok _ -> Alcotest.fail "expected unknown field error"
    | Error errors ->
      Alcotest.(check bool) "reports unknown field"
        true (has_error_substring "unknown config field: base_directores" errors))

let test_save_reaps_stale_atomic_temp () =
  with_tmp_home (fun home ->
    let path = config_path home in
    Discord_agents.Resource.ensure_parent_dir path;
    let stale = Filename.concat (Filename.dirname path)
      "config.json.tmp.stale" in
    let oc = open_out stale in
    output_string oc "stale";
    close_out oc;
    Discord_agents.Config.save {
      Discord_agents.Config.default with
      discord_token = "test-token";
      guild_id = "guild";
    };
    Alcotest.(check bool) "config written" true (Sys.file_exists path);
    Alcotest.(check bool) "stale temp removed" false (Sys.file_exists stale);
    let mode = (Unix.stat path).Unix.st_perm land 0o777 in
    Alcotest.(check int) "config mode remains private" 0o600 mode)

let test_save_load_roundtrip () =
  with_tmp_home (fun _home ->
    let config = {
      Discord_agents.Config.discord_token = "test-token";
      guild_id = "guild";
      base_directories = ["~/Projects"; "/srv/src"];
      control_channel_id = Some "control";
      projects = [
        { name = "repo"; path = "/srv/src/repo"; channel_id = Some "chan" };
      ];
    } in
    Discord_agents.Config.save config;
    let loaded = Discord_agents.Config.load () in
    Alcotest.(check string) "token"
      config.discord_token loaded.discord_token;
    Alcotest.(check string) "guild"
      config.guild_id loaded.guild_id;
    Alcotest.(check (list string)) "base directories"
      config.base_directories loaded.base_directories;
    Alcotest.(check (option string)) "control channel"
      config.control_channel_id loaded.control_channel_id;
    Alcotest.(check int) "project count"
      1 (List.length loaded.projects);
    match loaded.projects with
    | [project] ->
      Alcotest.(check string) "project name" "repo" project.name;
      Alcotest.(check string) "project path" "/srv/src/repo" project.path;
      Alcotest.(check (option string)) "project channel"
        (Some "chan") project.channel_id
    | _ -> Alcotest.fail "expected one project")

let test_reasoning_effort_values_by_agent () =
  let values agent =
    Discord_agents.Config.reasoning_effort_strings_for_agent agent
  in
  Alcotest.(check (list string)) "claude efforts"
    ["low"; "medium"; "high"; "xhigh"; "max"]
    (values Discord_agents.Config.Claude);
  Alcotest.(check (list string)) "codex efforts"
    ["low"; "medium"; "high"; "xhigh"]
    (values Discord_agents.Config.Codex);
  Alcotest.(check (list string)) "gemini efforts"
    [] (values Discord_agents.Config.Gemini)

let test_validate_reasoning_effort_for_agent () =
  let validate = Discord_agents.Config.validate_reasoning_effort_for_agent in
  let all_efforts = [
    Discord_agents.Config.Low;
    Discord_agents.Config.Medium;
    Discord_agents.Config.High;
    Discord_agents.Config.Xhigh;
    Discord_agents.Config.Max;
  ] in
  List.iter (fun agent ->
    let supported = Discord_agents.Config.reasoning_efforts_for_agent agent in
    List.iter (fun effort ->
      let result = validate agent (Some effort) in
      let expected =
        List.exists (Discord_agents.Config.equal_reasoning_effort effort)
          supported
      in
      Alcotest.(check bool)
        (Printf.sprintf "%s %s validation follows supported list"
           (Discord_agents.Config.string_of_agent_kind agent)
           (Discord_agents.Config.string_of_reasoning_effort effort))
        expected
        (match result with Ok () -> true | Error _ -> false)
    ) all_efforts
  ) [
    Discord_agents.Config.Claude;
    Discord_agents.Config.Codex;
    Discord_agents.Config.Gemini;
  ];
  (match validate Discord_agents.Config.Codex (Some Discord_agents.Config.Max) with
   | Ok () -> Alcotest.fail "expected Codex max effort to be rejected"
   | Error msg ->
     Alcotest.(check bool) "codex max names max as Claude-only"
       true (has_error_substring "max is Claude-only" [msg]));
  (match validate Discord_agents.Config.Gemini (Some Discord_agents.Config.High) with
   | Ok () -> Alcotest.fail "expected Gemini effort to be rejected"
   | Error msg ->
     Alcotest.(check bool) "gemini unsupported"
       true (has_error_substring "Gemini CLI does not expose" [msg]));
  Alcotest.(check bool) "clearing effort accepted for Gemini"
    true
    (match validate Discord_agents.Config.Gemini None with
     | Ok () -> true
     | Error _ -> false);
  Alcotest.(check bool) "xhigh accepted for Codex"
    true
    (match validate Discord_agents.Config.Codex
             (Some Discord_agents.Config.Xhigh) with
     | Ok () -> true
     | Error _ -> false)

let () =
  Alcotest.run "config" [
    ("schema", [
      Alcotest.test_case "load accepts base_dirs alias" `Quick
        test_load_accepts_basic_schema_alias;
      Alcotest.test_case "canonical base_directories wins" `Quick
        test_load_prefers_canonical_base_directories;
      Alcotest.test_case "load accepts project without channel_id" `Quick
        test_load_accepts_project_without_channel_id;
      Alcotest.test_case "env token replaces blank config token" `Quick
        test_load_env_token_replaces_blank_config_token;
      Alcotest.test_case "validation reports required fields" `Quick
        test_validate_reports_required_fields;
      Alcotest.test_case "smoke-test validation allows missing guild" `Quick
        test_validate_smoke_test_allows_missing_guild;
      Alcotest.test_case "load_result reports parse errors" `Quick
        test_load_result_reports_parse_errors;
      Alcotest.test_case "load_result reports schema errors" `Quick
        test_load_result_reports_schema_errors;
      Alcotest.test_case "load_result reports unknown fields" `Quick
        test_load_result_reports_unknown_fields;
    ]);
    ("agent capabilities", [
      Alcotest.test_case "reasoning effort values by agent" `Quick
        test_reasoning_effort_values_by_agent;
      Alcotest.test_case "validate reasoning effort for agent" `Quick
        test_validate_reasoning_effort_for_agent;
    ]);
    ("persistence", [
      Alcotest.test_case "save reaps stale atomic temp" `Quick
        test_save_reaps_stale_atomic_temp;
      Alcotest.test_case "save/load roundtrip" `Quick
        test_save_load_roundtrip;
    ]);
  ]
