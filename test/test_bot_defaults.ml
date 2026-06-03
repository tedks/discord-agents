(** Behavioral tests for default-agent and session-agent session transitions. *)

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

let with_tmp_home f =
  let base = make_tmp_dir "discord_agents_bot_" in
  let old_home = Sys.getenv_opt "HOME" in
  let old_xdg_config_home = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Fun.protect
    ~finally:(fun () ->
      restore_env "HOME" old_home;
      restore_env "XDG_CONFIG_HOME" old_xdg_config_home;
      rm_rf base)
    (fun () ->
      Unix.putenv "HOME" base;
      Unix.putenv "XDG_CONFIG_HOME" "";
      f ())

let with_test_bot f =
  with_tmp_home (fun () ->
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
      let settings : Discord_agents.Runtime_settings.t = {
        default_agent = Discord_agents.Config.Claude;
      } in
      let config : Discord_agents.Config.t = {
        Discord_agents.Config.default with
        discord_token = "test-token";
        control_channel_id = Some "control";
      } in
      let project_state : Discord_agents.Bot.project_state = {
        projects = [];
        channels = Discord_agents.Channel_manager.create ();
      } in
      let bot : Discord_agents.Bot.t = {
        config;
        settings;
        rest = Discord_agents.Discord_rest.create ~sw ~env ~token:"test-token";
        gateway = Discord_agents.Discord_gateway.create
          ~token:"test-token"
          ~intents:Discord_agents.Discord_gateway.default_intents
          ~handler:(fun _ -> ());
        project_state;
        sessions = Discord_agents.Session_store.create ();
        env;
        sw;
        started_at = Unix.gettimeofday ();
        draining = false;
        child_pids = (ref Discord_agents.Bot.Pid_set.empty, Mutex.create ());
        wrap_width = Discord_agents.Agent_process.desktop_width;
        refreshing = false;
        output_lines = Discord_agents.Agent_process.default_output_lines;
        scroll_states = Hashtbl.create 8;
      } in
      f bot)

let kind_string = Discord_agents.Config.string_of_agent_kind

let make_session ?(processing=false) ?session_override_kind
    ?pending_agent_change agent_kind =
  let session = Discord_agents.Session_store.make_session
    ~project_name:"control"
    ~working_dir:"/tmp/project"
    ~agent_kind
    ?session_override_kind
    ~session_id:"session-1"
    ~thread_id:"control"
    ~system_prompt:(Some "prompt")
    ~initial_prompt:None
    ()
  in
  session.processing <- processing;
  session.pending_agent_change <- pending_agent_change;
  session

let expect_pending session ~kind ~origin =
  match session.Discord_agents.Session_store.pending_agent_change with
  | Some pending ->
    Alcotest.(check string) "pending kind"
      (kind_string kind) (kind_string pending.kind);
    let actual_origin =
      Discord_agents.Session_store.string_of_pending_agent_origin pending.origin
    in
    let expected_origin =
      Discord_agents.Session_store.string_of_pending_agent_origin origin
    in
    Alcotest.(check string) "pending origin" expected_origin actual_origin
  | None ->
    Alcotest.fail "expected a pending agent change"

let find_control_session bot =
  match Discord_agents.Session_store.find_opt bot.Discord_agents.Bot.sessions
          ~thread_id:"control" with
  | Some session -> session
  | None -> Alcotest.fail "expected a control-channel session"

let test_set_default_agent_defers_busy_control_session () =
  with_test_bot (fun bot ->
    let session = make_session ~processing:true Discord_agents.Config.Claude in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.set_default_agent bot
            ~current_channel_id:(Some "control")
            Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "set_default_agent failed: %s" err
    | Ok rotation ->
      Alcotest.(check int) "reset count" 0 rotation.reset_count;
      Alcotest.(check int) "busy count" 1 rotation.busy_count;
      (match rotation.current_busy_kind with
       | Some kind ->
         Alcotest.(check string) "current busy kind"
           "claude" (kind_string kind)
       | None -> Alcotest.fail "expected current busy kind");
      Alcotest.(check string) "default agent persisted"
        "codex" (kind_string bot.settings.default_agent);
      let saved = find_control_session bot in
      Alcotest.(check string) "session stays on current agent"
        "claude" (kind_string saved.agent_kind);
      expect_pending saved
        ~kind:Discord_agents.Config.Codex
        ~origin:Discord_agents.Session_store.Default_rotation)

let test_set_default_agent_preserves_explicit_session_override () =
  with_test_bot (fun bot ->
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Gemini;
      origin = Session_override;
    } in
    let session =
      make_session ~processing:true ~pending_agent_change:pending
        Discord_agents.Config.Claude
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.set_default_agent bot
            ~current_channel_id:(Some "control")
            Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "set_default_agent failed: %s" err
    | Ok rotation ->
      Alcotest.(check int) "busy count unchanged" 0 rotation.busy_count;
      (match rotation.current_override_kind with
       | Some kind ->
         Alcotest.(check string) "current override kind"
           "gemini" (kind_string kind)
       | None -> Alcotest.fail "expected current override kind");
      let saved = find_control_session bot in
      expect_pending saved
        ~kind:Discord_agents.Config.Gemini
        ~origin:Discord_agents.Session_store.Session_override)

let test_set_default_agent_clears_completed_default_rotation () =
  with_test_bot (fun bot ->
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let session =
      make_session ~pending_agent_change:pending Discord_agents.Config.Codex
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.set_default_agent bot
            Discord_agents.Config.Codex with
    | Error err -> Alcotest.failf "set_default_agent failed: %s" err
    | Ok rotation ->
      Alcotest.(check int) "reset count" 0 rotation.reset_count;
      Alcotest.(check int) "busy count" 0 rotation.busy_count;
      let saved = find_control_session bot in
      Alcotest.(check (option string)) "pending cleared"
        None
        (Option.map
           (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
           saved.pending_agent_change))

let test_apply_pending_session_override_when_idle () =
  with_test_bot (fun bot ->
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Gemini;
      origin = Session_override;
    } in
    let session =
      make_session ~pending_agent_change:pending Discord_agents.Config.Claude
    in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.maybe_apply_pending_session_agent_change bot session;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent switched" "gemini" (kind_string saved.agent_kind);
    Alcotest.(check (option string)) "session override persisted"
      (Some "gemini")
      (Option.map kind_string saved.session_override_kind);
    Alcotest.(check bool) "fresh session id allocated"
      true (saved.session_id <> original_session_id);
    Alcotest.(check (option string)) "pending cleared"
      None
      (Option.map
         (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
         saved.pending_agent_change))

let test_apply_pending_same_kind_session_override_starts_fresh_session () =
  with_test_bot (fun bot ->
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Session_override;
    } in
    let session =
      make_session ~pending_agent_change:pending Discord_agents.Config.Codex
    in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.maybe_apply_pending_session_agent_change bot session;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent unchanged" "codex" (kind_string saved.agent_kind);
    Alcotest.(check (option string)) "session override persisted"
      (Some "codex")
      (Option.map kind_string saved.session_override_kind);
    Alcotest.(check bool) "fresh session id allocated"
      true (saved.session_id <> original_session_id);
    Alcotest.(check (option string)) "pending cleared"
      None
      (Option.map
         (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
         saved.pending_agent_change))

let test_apply_pending_same_kind_clears_pending () =
  with_test_bot (fun bot ->
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let session =
      make_session ~pending_agent_change:pending Discord_agents.Config.Codex
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.maybe_apply_pending_session_agent_change bot session;
    let saved = find_control_session bot in
    Alcotest.(check (option string)) "pending cleared"
      None
      (Option.map
         (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
         saved.pending_agent_change))

let test_apply_pending_busy_session_leaves_pending_intact () =
  with_test_bot (fun bot ->
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Gemini;
      origin = Session_override;
    } in
    let session =
      make_session ~processing:true ~pending_agent_change:pending
        Discord_agents.Config.Claude
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.maybe_apply_pending_session_agent_change bot session;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent unchanged" "claude" (kind_string saved.agent_kind);
    expect_pending saved
      ~kind:Discord_agents.Config.Gemini
      ~origin:Discord_agents.Session_store.Session_override)

let test_reconcile_preserves_idle_session_override () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    let session =
      make_session
        ~session_override_kind:(Some Discord_agents.Config.Gemini)
        Discord_agents.Config.Gemini
    in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
    let saved = find_control_session bot in
    Alcotest.(check string) "override agent preserved"
      "gemini" (kind_string saved.agent_kind);
    Alcotest.(check (option string)) "override still marked"
      (Some "gemini")
      (Option.map kind_string saved.session_override_kind);
    Alcotest.(check string) "session id unchanged"
      original_session_id saved.session_id)

let test_reconcile_rotates_idle_session_to_default_agent () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    let session = make_session Discord_agents.Config.Claude in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent rotated" "codex" (kind_string saved.agent_kind);
    Alcotest.(check bool) "fresh session id allocated"
      true (saved.session_id <> original_session_id))

let test_reconcile_applies_persisted_pending_default_rotation () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let session =
      make_session ~pending_agent_change:pending Discord_agents.Config.Claude
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent rotated" "codex" (kind_string saved.agent_kind);
    Alcotest.(check (option string)) "pending cleared"
      None
      (Option.map
         (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
         saved.pending_agent_change))

let () =
  Alcotest.run "bot_defaults" [
    ("default agent", [
      Alcotest.test_case "busy control session defers default rotation" `Quick
        test_set_default_agent_defers_busy_control_session;
      Alcotest.test_case "explicit session override survives default rotation" `Quick
        test_set_default_agent_preserves_explicit_session_override;
      Alcotest.test_case "completed default rotation gets cleared" `Quick
        test_set_default_agent_clears_completed_default_rotation;
      Alcotest.test_case "idle pending session override starts fresh session" `Quick
        test_apply_pending_session_override_when_idle;
      Alcotest.test_case "same-kind pending clears" `Quick
        test_apply_pending_same_kind_clears_pending;
      Alcotest.test_case "same-kind session override starts fresh session" `Quick
        test_apply_pending_same_kind_session_override_starts_fresh_session;
      Alcotest.test_case "busy pending change stays pending" `Quick
        test_apply_pending_busy_session_leaves_pending_intact;
      Alcotest.test_case "reconcile preserves idle session override" `Quick
        test_reconcile_preserves_idle_session_override;
      Alcotest.test_case "reconcile rotates idle session to default" `Quick
        test_reconcile_rotates_idle_session_to_default_agent;
      Alcotest.test_case "reconcile applies persisted default rotation" `Quick
        test_reconcile_applies_persisted_pending_default_rotation;
    ]);
  ]
