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
    ("persistence", [
      Alcotest.test_case "save reaps stale atomic temp" `Quick
        test_save_reaps_stale_atomic_temp;
    ]);
  ]
