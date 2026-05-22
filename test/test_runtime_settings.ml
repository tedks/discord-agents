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
  Filename.temp_dir prefix ""

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

let settings_path home =
  Filename.concat home ".config/discord-agents/settings.json"

let backup_path home = settings_path home ^ ".bak"
let stale_temp_path home = settings_path home ^ ".tmp.stale"

let test_load_defaults_to_claude () =
  with_tmp_home (fun _home ->
    let settings = Discord_agents.Runtime_settings.load () in
    Alcotest.(check string) "default agent"
      "claude"
      (Discord_agents.Config.string_of_agent_kind settings.default_agent);
    Alcotest.(check (option string)) "rescue agent defaults to none"
      None
      (Option.map Discord_agents.Config.string_of_agent_kind settings.rescue_agent);
    Alcotest.(check bool) "policy sync defaults clean"
      false settings.policy_sync_pending)

let test_save_and_reload_roundtrip () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    match Discord_agents.Runtime_settings.set_default_agent
            settings Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "default save failed: %s" err
    | Ok () ->
      (match Discord_agents.Runtime_settings.set_rescue_agent
               settings (Some Discord_agents.Config.Gemini) with
       | Error err -> Alcotest.failf "rescue save failed: %s" err
       | Ok () -> ());
      (match Discord_agents.Runtime_settings.set_policy_sync_pending settings true with
       | Error err -> Alcotest.failf "pending save failed: %s" err
       | Ok () -> ());
      Alcotest.(check bool) "backup exists"
        true (Sys.file_exists (backup_path home));
      Alcotest.(check string) "backup mirrors primary"
        (Discord_agents.Resource.read_file (settings_path home))
        (Discord_agents.Resource.read_file (backup_path home));
      let reloaded = Discord_agents.Runtime_settings.load () in
      Alcotest.(check string) "saved default agent"
        "codex"
        (Discord_agents.Config.string_of_agent_kind reloaded.default_agent);
      Alcotest.(check (option string)) "saved rescue agent"
        (Some "gemini")
        (Option.map
           Discord_agents.Config.string_of_agent_kind
           reloaded.rescue_agent);
      Alcotest.(check bool) "saved pending policy flag"
        true reloaded.policy_sync_pending)

let test_load_uses_backup_when_primary_corrupt () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    match Discord_agents.Runtime_settings.set_default_agent
            settings Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "save failed: %s" err
    | Ok () ->
      let oc = open_out (settings_path home) in
      output_string oc "{ definitely not json";
      close_out oc;
      let backup_stat = Unix.stat (backup_path home) in
      Unix.utimes
        (settings_path home)
        backup_stat.Unix.st_atime
        backup_stat.Unix.st_mtime;
      let recovered = Discord_agents.Runtime_settings.load () in
      Alcotest.(check string) "recovered default agent"
        "codex"
        (Discord_agents.Config.string_of_agent_kind recovered.default_agent))

let test_save_with_visible_but_unconfirmed_primary_updates_backup () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    settings.default_agent <- Discord_agents.Config.Codex;
    settings.rescue_agent <- Some Discord_agents.Config.Gemini;
    let write_file path content =
      if String.equal path (settings_path home) then
        Discord_agents.Resource.write_file_atomic
          ~fsync_parent:(fun dir ->
            raise (Unix.Unix_error (Unix.EINVAL, "fsync", dir)))
          path content
      else
        Discord_agents.Resource.write_file_atomic path content
    in
    match Discord_agents.Runtime_settings.save_with ~write_file settings with
    | Error err -> Alcotest.failf "save_with failed: %s" err
    | Ok () ->
      Alcotest.(check string) "backup mirrors primary after warning"
        (Discord_agents.Resource.read_file (settings_path home))
        (Discord_agents.Resource.read_file (backup_path home));
      let recovered = Discord_agents.Runtime_settings.load () in
      Alcotest.(check string) "reloaded default agent"
        "codex"
        (Discord_agents.Config.string_of_agent_kind recovered.default_agent))

let test_save_marks_primary_newer_when_backup_update_fails () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    match Discord_agents.Runtime_settings.set_default_agent
            settings Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "initial save failed: %s" err
    | Ok () ->
      settings.default_agent <- Discord_agents.Config.Gemini;
      let write_file path content =
        if String.equal path (backup_path home) then
          failwith "backup write failed"
        else
          Discord_agents.Resource.write_file_atomic path content
      in
      (match Discord_agents.Runtime_settings.save_with ~write_file settings with
       | Error err -> Alcotest.failf "save_with failed: %s" err
       | Ok () -> ());
      let primary_stat = Unix.stat (settings_path home) in
      let backup_stat = Unix.stat (backup_path home) in
      Alcotest.(check bool) "primary stamped newer than stale backup"
        true (primary_stat.Unix.st_mtime > backup_stat.Unix.st_mtime))

let test_set_top_level_policy_failure_leaves_settings_unchanged () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    Discord_agents.Resource.ensure_parent_dir (settings_path home);
    Unix.mkdir (settings_path home) 0o700;
    match Discord_agents.Runtime_settings.set_top_level_policy settings
            ~default_agent:Discord_agents.Config.Codex
            ~rescue_agent:(Some Discord_agents.Config.Gemini)
            ~policy_sync_pending:true with
    | Ok () ->
      Alcotest.fail "set_top_level_policy unexpectedly succeeded"
    | Error _ ->
      Alcotest.(check string) "default agent unchanged"
        "claude"
        (Discord_agents.Config.string_of_agent_kind settings.default_agent);
      Alcotest.(check (option string)) "rescue agent unchanged"
        None
        (Option.map
           Discord_agents.Config.string_of_agent_kind
           settings.rescue_agent);
      Alcotest.(check bool) "policy sync unchanged"
        false settings.policy_sync_pending)

let test_load_ignores_stale_backup_when_primary_is_newer_and_corrupt () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    match Discord_agents.Runtime_settings.set_default_agent
            settings Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "initial save failed: %s" err
    | Ok () ->
      settings.default_agent <- Discord_agents.Config.Gemini;
      let write_file path content =
        if String.equal path (backup_path home) then
          failwith "backup write failed"
        else
          Discord_agents.Resource.write_file_atomic path content
      in
      (match Discord_agents.Runtime_settings.save_with ~write_file settings with
       | Error err -> Alcotest.failf "save_with failed: %s" err
       | Ok () -> ());
      let oc = open_out (settings_path home) in
      output_string oc "{ definitely not json";
      close_out oc;
      let backup_stat = Unix.stat (backup_path home) in
      Unix.utimes
        (settings_path home)
        backup_stat.Unix.st_atime
        (backup_stat.Unix.st_mtime +. 10.0);
      let recovered = Discord_agents.Runtime_settings.load () in
      Alcotest.(check string) "stale backup ignored"
        "claude"
        (Discord_agents.Config.string_of_agent_kind recovered.default_agent);
      Alcotest.(check bool) "pending policy defaults clean after stale backup"
        false recovered.policy_sync_pending)

let test_save_reaps_stale_atomic_write_temps () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    Discord_agents.Resource.ensure_parent_dir (stale_temp_path home);
    let oc = open_out (stale_temp_path home) in
    output_string oc "stale";
    close_out oc;
    match Discord_agents.Runtime_settings.set_default_agent
            settings Discord_agents.Config.Gemini with
    | Error err -> Alcotest.failf "save failed: %s" err
    | Ok () ->
      Alcotest.(check bool) "stale temp removed"
        false (Sys.file_exists (stale_temp_path home)))

let test_save_refuses_read_only_preflight () =
  with_tmp_home (fun home ->
    let settings = Discord_agents.Runtime_settings.load () in
    settings.default_agent <- Discord_agents.Config.Codex;
    settings.rescue_agent <- Some Discord_agents.Config.Gemini;
    let write_attempted = ref false in
    let write_file _path _content =
      write_attempted := true;
      failwith "write should not be attempted"
    in
    match Discord_agents.Runtime_settings.save_with
            ~preflight_write:(fun _ -> Error "disk is read-only")
            ~write_file settings with
    | Ok () -> Alcotest.fail "save_with unexpectedly succeeded"
    | Error err ->
      Alcotest.(check string) "preflight error" "disk is read-only" err;
      Alcotest.(check bool) "write skipped" false !write_attempted;
      Alcotest.(check bool) "primary absent"
        false (Sys.file_exists (settings_path home)))

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

let test_xdg_falls_back_to_existing_legacy_home () =
  let home = make_tmp_dir "discord_agents_home_" in
  let xdg_root = make_tmp_dir "discord_agents_xdg_" in
  Fun.protect
    ~finally:(fun () ->
      rm_rf xdg_root;
      rm_rf home)
    (fun () ->
      with_tmp_env ~home ~xdg_config_home:xdg_root (fun () ->
        let legacy_dir =
          Filename.concat home ".config/discord-agents"
        in
        Unix.mkdir (Filename.concat xdg_root "discord-agents") 0o755;
        let legacy_settings =
          Filename.concat legacy_dir "settings.json"
        in
        Discord_agents.Resource.write_file_atomic legacy_settings
          {|{"default_agent":"codex"}|};
        Alcotest.(check string) "config dir"
          legacy_dir
          (Discord_agents.Resource.app_config_dir ());
        let settings = Discord_agents.Runtime_settings.load () in
        Alcotest.(check string) "legacy default agent"
          "codex"
          (Discord_agents.Config.string_of_agent_kind
             settings.default_agent)))

let () =
  Alcotest.run "runtime_settings" [
    ("settings", [
      Alcotest.test_case "load defaults to claude" `Quick
        test_load_defaults_to_claude;
      Alcotest.test_case "save and reload roundtrip" `Quick
        test_save_and_reload_roundtrip;
      Alcotest.test_case "load uses backup when primary corrupt" `Quick
        test_load_uses_backup_when_primary_corrupt;
      Alcotest.test_case "visible but unconfirmed primary still updates backup" `Quick
        test_save_with_visible_but_unconfirmed_primary_updates_backup;
      Alcotest.test_case "failed backup leaves primary newer" `Quick
        test_save_marks_primary_newer_when_backup_update_fails;
      Alcotest.test_case "set_top_level_policy failure leaves settings unchanged" `Quick
        test_set_top_level_policy_failure_leaves_settings_unchanged;
      Alcotest.test_case "load ignores stale backup when primary is newer and corrupt" `Quick
        test_load_ignores_stale_backup_when_primary_is_newer_and_corrupt;
      Alcotest.test_case "save reaps stale atomic write temps" `Quick
        test_save_reaps_stale_atomic_write_temps;
      Alcotest.test_case "save refuses read-only preflight" `Quick
        test_save_refuses_read_only_preflight;
      Alcotest.test_case "xdg config home without home" `Quick
        test_xdg_config_home_without_home;
      Alcotest.test_case "xdg falls back to existing legacy home" `Quick
        test_xdg_falls_back_to_existing_legacy_home;
    ]);
  ]
