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

let with_tmp_home f =
  let base = Filename.temp_file "discord_agents_home_" "" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  let old_home = Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" base;
  Fun.protect
    ~finally:(fun () ->
      (match old_home with
       | Some home -> Unix.putenv "HOME" home
       | None -> Unix.putenv "HOME" "");
      rm_rf base)
    (fun () -> f base)

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

let () =
  Alcotest.run "runtime_settings" [
    ("settings", [
      Alcotest.test_case "load defaults to claude" `Quick
        test_load_defaults_to_claude;
      Alcotest.test_case "save and reload roundtrip" `Quick
        test_save_and_reload_roundtrip;
    ]);
  ]
