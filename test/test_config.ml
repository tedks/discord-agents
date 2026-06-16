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

let has_error_substring needle errors =
  List.exists (fun err ->
    try ignore (Str.search_forward (Str.regexp_string needle) err 0); true
    with Not_found -> false
  ) errors

let test_validate_reports_required_fields () =
  let config = {
    Discord_agents.Config.default with
    base_directories = ["~/Projects"; ""];
    projects = [
      { name = ""; path = "/tmp/project"; channel_id = None };
      { name = "missing-path"; path = ""; channel_id = None };
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
    Alcotest.(check bool) "rejects empty project name"
      true (has_error_substring "projects[0].name" errors);
    Alcotest.(check bool) "rejects empty project path"
      true (has_error_substring "projects[1].path" errors)

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

let () =
  Alcotest.run "config" [
    ("schema", [
      Alcotest.test_case "load accepts base_dirs alias" `Quick
        test_load_accepts_basic_schema_alias;
      Alcotest.test_case "canonical base_directories wins" `Quick
        test_load_prefers_canonical_base_directories;
      Alcotest.test_case "validation reports required fields" `Quick
        test_validate_reports_required_fields;
      Alcotest.test_case "smoke-test validation allows missing guild" `Quick
        test_validate_smoke_test_allows_missing_guild;
    ]);
    ("persistence", [
      Alcotest.test_case "save reaps stale atomic temp" `Quick
        test_save_reaps_stale_atomic_temp;
    ]);
  ]
