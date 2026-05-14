(** Tests for persisted runtime settings. *)

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_rf (Filename.concat path name));
    Unix.rmdir path
  | _ ->
    Unix.unlink path

let make_tmp_dir prefix =
  let base = Filename.temp_file prefix "" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  base

let restore_env name = function
  | Some value -> Unix.putenv name value
  | None -> Unix.putenv name ""

let with_tmp_env ?home ?xdg_config_home f =
  let old_home = Sys.getenv_opt "HOME" in
  let old_xdg_config_home = Sys.getenv_opt "XDG_CONFIG_HOME" in
  (match home with
   | Some value -> Unix.putenv "HOME" value
   | None -> Unix.putenv "HOME" "");
  (match xdg_config_home with
   | Some value -> Unix.putenv "XDG_CONFIG_HOME" value
   | None -> Unix.putenv "XDG_CONFIG_HOME" "");
  Fun.protect
    ~finally:(fun () ->
      restore_env "HOME" old_home;
      restore_env "XDG_CONFIG_HOME" old_xdg_config_home)
    f

let with_tmp_home f =
  let base = make_tmp_dir "discord_agents_home_" in
  Fun.protect
    ~finally:(fun () -> rm_rf base)
    (fun () ->
      with_tmp_env ~home:base ~xdg_config_home:"" (fun () -> f base))

let test_load_defaults_to_claude () =
  with_tmp_home (fun _home ->
    let settings = Discord_agents.Runtime_settings.load () in
    Alcotest.(check string) "default agent"
      "claude"
      (Discord_agents.Config.string_of_agent_kind settings.default_agent))

let test_save_and_reload_roundtrip () =
  with_tmp_home (fun _home ->
    let settings = Discord_agents.Runtime_settings.load () in
    match Discord_agents.Runtime_settings.set_default_agent
            settings Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "save failed: %s" err
    | Ok () ->
      let reloaded = Discord_agents.Runtime_settings.load () in
      Alcotest.(check string) "saved default agent"
        "codex"
        (Discord_agents.Config.string_of_agent_kind reloaded.default_agent))

let test_load_uses_backup_when_primary_corrupt () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    match Discord_agents.Runtime_settings.set_default_agent
            settings Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "save failed: %s" err
    | Ok () ->
      let settings_path =
        Filename.concat home ".config/discord-agents/settings.json"
      in
      let oc = open_out settings_path in
      output_string oc "{ definitely not json";
      close_out oc;
      let recovered = Discord_agents.Runtime_settings.load () in
      Alcotest.(check string) "recovered default agent"
        "codex"
        (Discord_agents.Config.string_of_agent_kind recovered.default_agent))

let test_xdg_config_home_without_home () =
  let xdg_root = make_tmp_dir "discord_agents_xdg_" in
  Fun.protect
    ~finally:(fun () -> rm_rf xdg_root)
    (fun () ->
      with_tmp_env ~home:"" ~xdg_config_home:xdg_root (fun () ->
        let settings = Discord_agents.Runtime_settings.load () in
        match Discord_agents.Runtime_settings.set_default_agent
                settings Discord_agents.Config.Gemini with
        | Error err -> Alcotest.failf "save failed: %s" err
        | Ok () ->
          let reloaded = Discord_agents.Runtime_settings.load () in
          Alcotest.(check string) "saved default agent via xdg"
            "gemini"
            (Discord_agents.Config.string_of_agent_kind
               reloaded.default_agent)))

let () =
  Alcotest.run "runtime_settings" [
    ("settings", [
      Alcotest.test_case "load defaults to claude" `Quick
        test_load_defaults_to_claude;
      Alcotest.test_case "save and reload roundtrip" `Quick
        test_save_and_reload_roundtrip;
      Alcotest.test_case "load uses backup when primary corrupt" `Quick
        test_load_uses_backup_when_primary_corrupt;
      Alcotest.test_case "xdg config home without home" `Quick
        test_xdg_config_home_without_home;
    ]);
  ]
