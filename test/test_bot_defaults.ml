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
  Filename.temp_dir prefix ""

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
      Discord_agents.Disk_health.For_testing.reset ();
      let settings : Discord_agents.Runtime_settings.t = {
        default_agent = Discord_agents.Config.Claude;
        rescue_agent = None;
        policy_sync_pending = false;
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
        policy_sync_clear_last_warning = None;
        scroll_states = Hashtbl.create 8;
      } in
      Fun.protect
        ~finally:(fun () -> Discord_agents.Disk_health.For_testing.reset ())
        (fun () -> f bot))

let kind_string = Discord_agents.Config.string_of_agent_kind
let policy_sync_state_string bot =
  Discord_agents.Bot.top_level_policy_sync_state bot
  |> Discord_agents.Bot.string_of_top_level_policy_sync_state

let set_disk_warning_mode () =
  Discord_agents.Disk_health.For_testing.set_probe_available_bytes
    (fun _path -> Discord_agents.Disk_health.For_testing.mib 96)

let set_disk_read_only_mode () =
  Discord_agents.Disk_health.For_testing.set_probe_available_bytes
    (fun _path -> Discord_agents.Disk_health.For_testing.mib 32)

let set_disk_healthy_mode () =
  Discord_agents.Disk_health.For_testing.set_probe_available_bytes
    (fun _path -> Discord_agents.Disk_health.For_testing.mib 512)

let make_project ~name ~path =
  Discord_agents.Project.{
    name;
    path;
    is_bare = false;
    remote_url = None;
  }

let set_projects bot projects =
  bot.Discord_agents.Bot.project_state <-
    { bot.Discord_agents.Bot.project_state with projects }

let make_session ?(project_name="control") ?(working_dir="/tmp/project")
    ?(thread_id="control") ?(processing=false) ?session_override_kind
    ?pending_agent_change ?(system_prompt=Some "prompt") agent_kind =
  let session = Discord_agents.Session_store.make_session
    ~project_name
    ~working_dir
    ~agent_kind
    ?session_override_kind
    ~session_id:"session-1"
    ~thread_id
    ~system_prompt
    ~initial_prompt:None
    ()
  in
  session.processing <- processing;
  session.pending_agent_change <- pending_agent_change;
  session

let make_message ?(message_id="message-1") ?(channel_id="control") content =
  let author : Discord_agents.Discord_types.user = {
    id = "user-1";
    username = "tester";
    bot = Some false;
  } in
  {
    Discord_agents.Discord_types.id = message_id;
    channel_id;
    author;
    content;
    timestamp = "2026-01-01T00:00:00.000000+00:00";
    guild_id = Some "guild-1";
    attachments = [];
    referenced_message = None;
  }

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

let find_session bot thread_id =
  match Discord_agents.Session_store.find_opt bot.Discord_agents.Bot.sessions
          ~thread_id with
  | Some session -> session
  | None -> Alcotest.failf "expected session %s" thread_id

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
      Alcotest.(check bool) "policy sync completed"
        false bot.settings.policy_sync_pending;
      let persisted = Discord_agents.Runtime_settings.load () in
      Alcotest.(check bool) "persisted policy sync completed"
        false persisted.policy_sync_pending;
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

let test_apply_pending_same_kind_session_override_pins_existing_session () =
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
    Alcotest.(check string) "agent stays codex" "codex" (kind_string saved.agent_kind);
    Alcotest.(check (option string)) "session override persisted"
      (Some "codex")
      (Option.map kind_string saved.session_override_kind);
    Alcotest.(check string) "session id unchanged"
      original_session_id saved.session_id;
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

let test_finalize_pending_default_rotation_uses_current_policy_after_pressure_clears () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    set_disk_warning_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let session =
      make_session ~processing:true ~pending_agent_change:pending
        Discord_agents.Config.Claude
    in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    set_disk_healthy_mode ();
    Discord_agents.Bot.finalize_session_run ~notify_stopped:false bot session;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent follows recovered default policy"
      "claude" (kind_string saved.agent_kind);
    Alcotest.(check string) "session id unchanged"
      original_session_id saved.session_id;
    Alcotest.(check (option string)) "stale pending cleared"
      None
      (Option.map
         (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
         saved.pending_agent_change))

let test_finalize_pending_default_rotation_uses_current_rescue_policy () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Gemini;
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Gemini;
      origin = Default_rotation;
    } in
    let session =
      make_session ~processing:true ~pending_agent_change:pending
        Discord_agents.Config.Claude
    in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    set_disk_warning_mode ();
    Discord_agents.Bot.finalize_session_run ~notify_stopped:false bot session;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent follows current rescue policy"
      "codex" (kind_string saved.agent_kind);
    Alcotest.(check bool) "fresh session id allocated"
      true (saved.session_id <> original_session_id);
    Alcotest.(check (option string)) "pending cleared"
      None
      (Option.map
         (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
         saved.pending_agent_change))

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
    (match Discord_agents.Runtime_settings.set_top_level_policy bot.settings
             ~default_agent:Discord_agents.Config.Codex
             ~rescue_agent:None
             ~policy_sync_pending:true with
     | Error err -> Alcotest.failf "set_top_level_policy failed: %s" err
     | Ok () -> ());
    let session = make_session Discord_agents.Config.Claude in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent rotated" "codex" (kind_string saved.agent_kind);
    Alcotest.(check bool) "policy sync cleared"
      false bot.settings.policy_sync_pending;
    let persisted = Discord_agents.Runtime_settings.load () in
    Alcotest.(check bool) "persisted policy sync cleared"
      false persisted.policy_sync_pending;
    Alcotest.(check bool) "fresh session id allocated"
      true (saved.session_id <> original_session_id))

let test_best_effort_policy_sync_does_not_raise_in_read_only_mode () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    let session = make_session Discord_agents.Config.Claude in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    set_disk_read_only_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    Discord_agents.Bot.sync_top_level_agent_policy_best_effort bot;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent unchanged after failed best-effort sync"
      "claude" (kind_string saved.agent_kind);
    Alcotest.(check string) "session id unchanged"
      original_session_id saved.session_id)

let test_reconcile_clears_stale_rescue_rotation_after_pressure_clears () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    set_disk_healthy_mode ();
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let session =
      make_session ~pending_agent_change:pending Discord_agents.Config.Claude
    in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent follows recovered default policy"
      "claude" (kind_string saved.agent_kind);
    Alcotest.(check string) "session id unchanged"
      original_session_id saved.session_id;
    Alcotest.(check (option string)) "stale rescue rotation cleared"
      None
      (Option.map
         (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
         saved.pending_agent_change))

let test_reconcile_keeps_rescue_when_persistent_workdir_still_under_pressure () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    let project_dir = make_tmp_dir "discord_agents_low_space_project_" in
    Fun.protect
      ~finally:(fun () -> rm_rf project_dir)
      (fun () ->
        Discord_agents.Disk_health.For_testing.set_probe_available_bytes
          (fun path ->
             if String.equal path project_dir then
               Discord_agents.Disk_health.For_testing.mib 96
             else
               Discord_agents.Disk_health.For_testing.mib 512);
        let session =
          make_session
            ~working_dir:project_dir
            Discord_agents.Config.Codex
        in
        let original_session_id = session.session_id in
        Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
        Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
        let saved = find_control_session bot in
        Alcotest.(check string) "effective policy observes workdir pressure"
          "codex"
          (kind_string (Discord_agents.Bot.effective_top_level_agent bot));
        Alcotest.(check string) "agent stays on rescue"
          "codex" (kind_string saved.agent_kind);
        Alcotest.(check string) "session id unchanged"
          original_session_id saved.session_id))

let test_reconcile_rotates_project_session_without_channel_map () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    let project_dir = make_tmp_dir "discord_agents_project_session_" in
    Fun.protect
      ~finally:(fun () -> rm_rf project_dir)
      (fun () ->
        set_projects bot [make_project ~name:"demo" ~path:project_dir];
        let session =
          make_session
            ~project_name:"demo"
            ~working_dir:project_dir
            ~thread_id:"project-channel"
            Discord_agents.Config.Claude
        in
        let original_session_id = session.session_id in
        Discord_agents.Session_store.add bot.sessions
          ~thread_id:"project-channel" session;
        Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
        let saved = find_session bot "project-channel" in
        Alcotest.(check string) "project session rotated"
          "codex" (kind_string saved.agent_kind);
        Alcotest.(check bool) "fresh session id allocated"
          true (saved.session_id <> original_session_id)))

let test_reconcile_does_not_rotate_thread_session_without_channel_map () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    let project_dir = make_tmp_dir "discord_agents_thread_session_" in
    Fun.protect
      ~finally:(fun () -> rm_rf project_dir)
      (fun () ->
        set_projects bot [make_project ~name:"demo" ~path:project_dir];
        let session =
          make_session
            ~project_name:"demo"
            ~working_dir:project_dir
            ~thread_id:"thread-1"
            ~system_prompt:None
            Discord_agents.Config.Claude
        in
        let original_session_id = session.session_id in
        Discord_agents.Session_store.add bot.sessions
          ~thread_id:"thread-1" session;
        Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
        let saved = find_session bot "thread-1" in
        Alcotest.(check string) "thread session agent preserved"
          "claude" (kind_string saved.agent_kind);
        Alcotest.(check string) "session id unchanged"
          original_session_id saved.session_id))

let test_best_effort_sync_observes_project_workdir_pressure () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    let project_dir = make_tmp_dir "discord_agents_sync_project_pressure_" in
    Fun.protect
      ~finally:(fun () -> rm_rf project_dir)
      (fun () ->
        set_projects bot [make_project ~name:"demo" ~path:project_dir];
        Discord_agents.Disk_health.For_testing.set_probe_available_bytes
          (fun path ->
             if String.equal path project_dir then
               Discord_agents.Disk_health.For_testing.mib 96
             else
               Discord_agents.Disk_health.For_testing.mib 512);
        let session =
          make_session
            ~project_name:"demo"
            ~working_dir:project_dir
            ~thread_id:"project-channel"
            Discord_agents.Config.Claude
        in
        let original_session_id = session.session_id in
        Discord_agents.Session_store.add bot.sessions
          ~thread_id:"project-channel" session;
        Discord_agents.Bot.sync_top_level_agent_policy_best_effort bot;
        let saved = find_session bot "project-channel" in
        Alcotest.(check string) "effective policy observes workdir pressure"
          "codex"
          (kind_string (Discord_agents.Bot.effective_top_level_agent bot));
        Alcotest.(check string) "session rotated to rescue"
          "codex" (kind_string saved.agent_kind);
        Alcotest.(check bool) "fresh session id allocated"
          true (saved.session_id <> original_session_id)))

let test_policy_sync_state_is_marker_clear_pending_after_converged_rotation () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    bot.settings.policy_sync_pending <- true;
    let session = make_session Discord_agents.Config.Codex in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Alcotest.(check string) "policy sync state"
      "marker-clear-pending" (policy_sync_state_string bot))

let test_policy_sync_state_is_rotation_pending_for_deferred_default_rotation () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    bot.settings.policy_sync_pending <- true;
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let session =
      make_session ~processing:true ~pending_agent_change:pending
        Discord_agents.Config.Claude
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Alcotest.(check string) "policy sync state"
      "rotation-pending" (policy_sync_state_string bot))

let test_policy_sync_state_is_rotation_pending_after_marker_cleared () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let session =
      make_session ~processing:true ~pending_agent_change:pending
        Discord_agents.Config.Claude
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Alcotest.(check string) "policy sync state"
      "rotation-pending" (policy_sync_state_string bot))

let test_policy_sync_clear_warning_is_suppressed_for_repeat_state_and_error () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    bot.settings.policy_sync_pending <- true;
    let session = make_session Discord_agents.Config.Codex in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Alcotest.(check bool) "first warning allowed"
      true
      (Discord_agents.Bot.should_log_policy_sync_clear_failure bot "disk full");
    Alcotest.(check bool) "repeat warning suppressed"
      false
      (Discord_agents.Bot.should_log_policy_sync_clear_failure bot "disk full"))

let test_policy_sync_clear_warning_logs_again_after_error_or_state_change () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    bot.settings.policy_sync_pending <- true;
    let converged = make_session Discord_agents.Config.Codex in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" converged;
    ignore (Discord_agents.Bot.should_log_policy_sync_clear_failure bot "disk full");
    Alcotest.(check bool) "different error re-logs"
      true
      (Discord_agents.Bot.should_log_policy_sync_clear_failure bot "io timeout");
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let deferred =
      make_session ~processing:true ~pending_agent_change:pending
        Discord_agents.Config.Claude
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" deferred;
    Alcotest.(check string) "policy sync state changed"
      "rotation-pending" (policy_sync_state_string bot);
    Alcotest.(check bool) "state change re-logs"
      true
      (Discord_agents.Bot.should_log_policy_sync_clear_failure bot "io timeout"))

let test_policy_sync_clear_success_rearms_warning () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    bot.settings.policy_sync_pending <- true;
    let session = make_session Discord_agents.Config.Codex in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    ignore (Discord_agents.Bot.should_log_policy_sync_clear_failure bot "disk full");
    Discord_agents.Bot.note_policy_sync_clear_success bot;
    Alcotest.(check bool) "warning re-armed after success"
      true
      (Discord_agents.Bot.should_log_policy_sync_clear_failure bot "disk full"))

let test_policy_sync_state_ignores_nonpersistent_sessions () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Codex;
    bot.settings.policy_sync_pending <- true;
    let control = make_session Discord_agents.Config.Codex in
    let ephemeral =
      make_session ~project_name:"thread" ~thread_id:"thread-1"
        Discord_agents.Config.Claude
    in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" control;
    Discord_agents.Session_store.add bot.sessions ~thread_id:"thread-1" ephemeral;
    Alcotest.(check string) "policy sync state"
      "marker-clear-pending" (policy_sync_state_string bot))

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

let test_effective_top_level_agent_uses_rescue_under_pressure () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    set_disk_warning_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    Alcotest.(check string) "effective top-level agent"
      "codex"
      (kind_string (Discord_agents.Bot.effective_top_level_agent bot)))

let test_set_rescue_agent_rotates_idle_control_session_under_pressure () =
  with_test_bot (fun bot ->
    set_disk_warning_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    let session = make_session Discord_agents.Config.Claude in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.set_rescue_agent bot
            ~current_channel_id:(Some "control")
            (Some Discord_agents.Config.Codex) with
    | Error err -> Alcotest.failf "set_rescue_agent failed: %s" err
    | Ok rotation ->
      Alcotest.(check int) "idle reset count" 1 rotation.reset_count;
      Alcotest.(check bool) "policy sync completed"
        false bot.settings.policy_sync_pending;
      let persisted = Discord_agents.Runtime_settings.load () in
      Alcotest.(check bool) "persisted policy sync completed"
        false persisted.policy_sync_pending;
      let saved = find_control_session bot in
      Alcotest.(check string) "agent rotated to rescue"
        "codex" (kind_string saved.agent_kind);
      Alcotest.(check bool) "fresh session id allocated"
        true (saved.session_id <> original_session_id))

let test_set_default_agent_rolls_back_pending_rotation_when_session_save_fails () =
  with_test_bot (fun bot ->
    let session = make_session ~processing:true Discord_agents.Config.Claude in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    let sessions_path = Discord_agents.Session_store.sessions_file () in
    Sys.remove sessions_path;
    Unix.mkdir sessions_path 0o700;
    match Discord_agents.Bot.set_default_agent bot
            ~current_channel_id:(Some "control")
            Discord_agents.Config.Codex with
    | Ok _ ->
      Alcotest.fail "set_default_agent unexpectedly succeeded"
    | Error _ ->
      Alcotest.(check string) "staged default agent persisted"
        "codex" (kind_string bot.settings.default_agent);
      Alcotest.(check bool) "policy sync left pending"
        true bot.settings.policy_sync_pending;
      let persisted = Discord_agents.Runtime_settings.load () in
      Alcotest.(check string) "persisted default agent"
        "codex"
        (Discord_agents.Config.string_of_agent_kind persisted.default_agent);
      Alcotest.(check bool) "persisted policy sync pending"
        true persisted.policy_sync_pending;
      let saved = find_control_session bot in
      Alcotest.(check string) "session agent rolled back"
        "claude" (kind_string saved.agent_kind);
      Alcotest.(check (option string)) "pending change rolled back"
        None
        (Option.map
           (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
           saved.pending_agent_change))

let test_align_rolls_back_in_memory_mutations_when_replacement_raises () =
  with_test_bot (fun bot ->
    let busy = make_session ~processing:true Discord_agents.Config.Claude in
    let idle =
      make_session ~project_name:"proj" ~thread_id:"project-thread"
        Discord_agents.Config.Claude
    in
    Discord_agents.Channel_manager.add
      bot.project_state.channels
      ~project_name:"proj"
      ~channel_id:"project-thread";
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" busy;
    Discord_agents.Session_store.add
      bot.sessions ~thread_id:"project-thread" idle;
    let saw_busy_pending_before_failure = ref false in
    let replacement_session _t (session : Discord_agents.Session_store.session)
        ~agent_kind:_ ~session_override_kind:_ =
      if String.equal session.thread_id "project-thread" then
        (saw_busy_pending_before_failure :=
           Option.is_some busy.pending_agent_change;
        failwith "simulated replacement failure"
        )
      else
        session
    in
    match Discord_agents.Bot.align_persistent_sessions_to_agent
            ~replacement_session
            bot ~current_channel_id:(Some "control")
            ~new_agent:Discord_agents.Config.Codex with
    | Ok _ ->
      Alcotest.fail "align_persistent_sessions_to_agent unexpectedly succeeded"
    | Error _ ->
      Alcotest.(check bool) "busy mutation happened before failure"
        true !saw_busy_pending_before_failure;
      let saved_busy = find_control_session bot in
      Alcotest.(check string) "busy session agent unchanged"
        "claude" (kind_string saved_busy.agent_kind);
      Alcotest.(check (option string)) "busy pending rolled back"
        None
        (Option.map
           (fun pending -> kind_string pending.Discord_agents.Session_store.kind)
           saved_busy.pending_agent_change);
      match Discord_agents.Session_store.find_opt bot.sessions
              ~thread_id:"project-thread" with
      | None -> Alcotest.fail "expected project-thread session"
      | Some saved_idle ->
        Alcotest.(check string) "idle session unchanged"
          "claude" (kind_string saved_idle.agent_kind))

let test_align_reraises_fatal_replacement_exception () =
  with_test_bot (fun bot ->
    let session = make_session Discord_agents.Config.Claude in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    let replacement_session _t (_session : Discord_agents.Session_store.session)
        ~agent_kind:_ ~session_override_kind:_ =
      assert false
    in
    try
      ignore (Discord_agents.Bot.align_persistent_sessions_to_agent
        ~replacement_session
        bot ~current_channel_id:(Some "control")
        ~new_agent:Discord_agents.Config.Codex);
      Alcotest.fail "expected Assert_failure"
    with
    | Assert_failure _ -> ())

let test_align_rolls_back_cleared_pending_rotation_when_replacement_raises () =
  with_test_bot (fun bot ->
    let pending = Discord_agents.Session_store.{
      kind = Discord_agents.Config.Codex;
      origin = Default_rotation;
    } in
    let control =
      make_session ~pending_agent_change:pending Discord_agents.Config.Codex
    in
    let idle =
      make_session ~project_name:"proj" ~thread_id:"project-thread"
        Discord_agents.Config.Claude
    in
    Discord_agents.Channel_manager.add
      bot.project_state.channels
      ~project_name:"proj"
      ~channel_id:"project-thread";
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" control;
    Discord_agents.Session_store.add
      bot.sessions ~thread_id:"project-thread" idle;
    let replacement_session _t (session : Discord_agents.Session_store.session)
        ~agent_kind:_ ~session_override_kind:_ =
      if String.equal session.thread_id "project-thread" then
        failwith "simulated replacement failure"
      else
        session
    in
    match Discord_agents.Bot.align_persistent_sessions_to_agent
            ~replacement_session
            bot ~current_channel_id:None
            ~new_agent:Discord_agents.Config.Codex with
    | Ok _ ->
      Alcotest.fail "align_persistent_sessions_to_agent unexpectedly succeeded"
    | Error _ ->
      let saved = find_control_session bot in
      expect_pending saved
        ~kind:Discord_agents.Config.Codex
        ~origin:Discord_agents.Session_store.Default_rotation)

let test_set_rescue_agent_preserves_idle_session_override_under_pressure () =
  with_test_bot (fun bot ->
    set_disk_warning_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    let session =
      make_session
        ~session_override_kind:(Some Discord_agents.Config.Gemini)
        Discord_agents.Config.Gemini
    in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.set_rescue_agent bot
            ~current_channel_id:(Some "control")
            (Some Discord_agents.Config.Codex) with
    | Error err -> Alcotest.failf "set_rescue_agent failed: %s" err
    | Ok rotation ->
      Alcotest.(check int) "override reset count" 0 rotation.reset_count;
      Alcotest.(check (option string)) "current override kind"
        (Some "gemini")
        (Option.map kind_string rotation.current_override_kind);
      let saved = find_control_session bot in
      Alcotest.(check string) "session override agent preserved"
        "gemini" (kind_string saved.agent_kind);
      Alcotest.(check string) "session id unchanged"
        original_session_id saved.session_id)

let test_disable_rescue_agent_rotates_idle_session_to_default_under_pressure () =
  with_test_bot (fun bot ->
    bot.settings.default_agent <- Discord_agents.Config.Claude;
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    set_disk_warning_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    let session = make_session Discord_agents.Config.Codex in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.set_rescue_agent bot
            ~current_channel_id:(Some "control")
            None with
    | Error err -> Alcotest.failf "disable rescue agent failed: %s" err
    | Ok rotation ->
      Alcotest.(check int) "idle reset count" 1 rotation.reset_count;
      let saved = find_control_session bot in
      Alcotest.(check string) "agent rotated to default"
        "claude" (kind_string saved.agent_kind);
      Alcotest.(check bool) "fresh session id allocated"
        true (saved.session_id <> original_session_id))

let test_set_default_agent_under_active_rescue_preserves_rescue_target () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    set_disk_warning_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    let session = make_session Discord_agents.Config.Claude in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.set_default_agent bot
            ~current_channel_id:(Some "control")
            Discord_agents.Config.Gemini with
    | Error err -> Alcotest.failf "set_default_agent failed: %s" err
    | Ok rotation ->
      Alcotest.(check int) "idle reset count" 1 rotation.reset_count;
      Alcotest.(check string) "default persisted"
        "gemini" (kind_string bot.settings.default_agent);
      let saved = find_control_session bot in
      Alcotest.(check string) "session still uses rescue agent"
        "codex" (kind_string saved.agent_kind))

let test_reconcile_rotates_idle_session_to_rescue_agent_under_pressure () =
  with_test_bot (fun bot ->
    bot.settings.rescue_agent <- Some Discord_agents.Config.Codex;
    set_disk_warning_mode ();
    ignore (Discord_agents.Disk_health.preflight_state_mutation ());
    let session = make_session Discord_agents.Config.Claude in
    let original_session_id = session.session_id in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.reconcile_persisted_pending_agent_changes bot;
    let saved = find_control_session bot in
    Alcotest.(check string) "agent rotated" "codex" (kind_string saved.agent_kind);
    Alcotest.(check bool) "fresh session id allocated"
      true (saved.session_id <> original_session_id))

let test_stop_idle_session_removes_it () =
  with_test_bot (fun bot ->
    let session = make_session Discord_agents.Config.Claude in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.stop_session bot ~thread_id:"control" with
    | Discord_agents.Bot.Session_stopped { project_name; dropped_count } ->
      Alcotest.(check string) "project name" "control" project_name;
      Alcotest.(check int) "no dropped queue" 0 dropped_count;
      Alcotest.(check bool) "session removed"
        true
        (Option.is_none
           (Discord_agents.Session_store.find_opt bot.sessions ~thread_id:"control"))
    | _ -> Alcotest.fail "expected idle session to stop immediately")

let test_stop_idle_queued_session_clears_and_removes_it () =
  with_test_bot (fun bot ->
    let session = make_session Discord_agents.Config.Claude in
    Queue.add
      Discord_agents.Session_store.{
        msg = make_message "queued while idle";
        channel_info = None;
      }
      session.pending_queue;
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.stop_session bot ~thread_id:"control" with
    | Discord_agents.Bot.Session_stopped { project_name; dropped_count } ->
      Alcotest.(check string) "project name" "control" project_name;
      Alcotest.(check int) "dropped queued message" 1 dropped_count;
      Alcotest.(check int) "queue cleared" 0 (Queue.length session.pending_queue);
      Alcotest.(check bool) "session removed"
        true
        (Option.is_none
           (Discord_agents.Session_store.find_opt bot.sessions ~thread_id:"control"))
    | _ -> Alcotest.fail "expected idle queued session to stop immediately")

let test_stop_busy_session_requests_stop () =
  with_test_bot (fun bot ->
    let session = make_session ~processing:true Discord_agents.Config.Claude in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.stop_session bot ~thread_id:"control" with
    | Discord_agents.Bot.Session_stopping stop ->
      Alcotest.(check string) "project name" "control" stop.project_name;
      Alcotest.(check bool) "no pid yet" false stop.had_running_process;
      Alcotest.(check int) "no dropped queue" 0 stop.dropped_count;
      Alcotest.(check bool) "stop requested latched" true session.stop_requested;
      Alcotest.(check bool) "session retained while stopping"
        true
        (Option.is_some
           (Discord_agents.Session_store.find_opt bot.sessions ~thread_id:"control"))
    | _ -> Alcotest.fail "expected busy session to enter stopping state")

let test_stop_busy_session_is_idempotent () =
  with_test_bot (fun bot ->
    let session = make_session ~processing:true Discord_agents.Config.Claude in
    session.stop_requested <- true;
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.stop_session bot ~thread_id:"control" with
    | Discord_agents.Bot.Session_already_stopping { project_name } ->
      Alcotest.(check string) "project name" "control" project_name
    | _ -> Alcotest.fail "expected repeated stop to be idempotent")

let test_stop_idle_stopping_session_retries_removal () =
  with_test_bot (fun bot ->
    let session = make_session Discord_agents.Config.Claude in
    session.stop_requested <- true;
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    match Discord_agents.Bot.stop_session bot ~thread_id:"control" with
    | Discord_agents.Bot.Session_stopped { project_name; dropped_count } ->
      Alcotest.(check string) "project name" "control" project_name;
      Alcotest.(check int) "no dropped queue" 0 dropped_count;
      Alcotest.(check bool) "session removed"
        true
        (Option.is_none
           (Discord_agents.Session_store.find_opt bot.sessions ~thread_id:"control"))
    | _ -> Alcotest.fail "expected idle stopping session to be removed")

let test_finalize_session_run_removes_stopped_session () =
  with_test_bot (fun bot ->
    let session = make_session ~processing:true Discord_agents.Config.Claude in
    session.stop_requested <- true;
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    Discord_agents.Bot.finalize_session_run ~notify_stopped:false bot session;
    Alcotest.(check bool) "session removed"
      true
      (Option.is_none
         (Discord_agents.Session_store.find_opt bot.sessions ~thread_id:"control"));
    Alcotest.(check bool) "processing cleared" false session.processing)

let test_ppid_of_proc_stat_line_handles_spaces_and_parens () =
  let line = "12345 (codex exec (worker 1)) S 6789 12345 12345 0 -1 4194304 0 0 0 0 0 0 0 0 20 0 1 0 123 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" in
  Alcotest.(check (option int)) "parsed ppid"
    (Some 6789)
    (Discord_agents.Bot.ppid_of_proc_stat_line line)

let test_stop_requested_roundtrips_through_disk () =
  with_tmp_home (fun () ->
    let store = Discord_agents.Session_store.create () in
    let session = make_session Discord_agents.Config.Claude in
    Discord_agents.Session_store.add store ~thread_id:"control" session;
    (match Discord_agents.Session_store.set_stop_requested store session true with
     | Ok () -> ()
     | Error err -> Alcotest.failf "set_stop_requested failed: %s" err);
    let reloaded = Discord_agents.Session_store.create () in
    match Discord_agents.Session_store.find_opt reloaded ~thread_id:"control" with
    | Some saved ->
      Alcotest.(check bool) "stop requested persisted" true saved.stop_requested
    | None -> Alcotest.fail "expected reloaded session")

let test_reconcile_persisted_stop_requests_removes_session () =
  with_test_bot (fun bot ->
    let session = make_session Discord_agents.Config.Claude in
    Discord_agents.Session_store.add bot.sessions ~thread_id:"control" session;
    (match Discord_agents.Session_store.set_stop_requested bot.sessions session true with
     | Ok () -> ()
     | Error err -> Alcotest.failf "set_stop_requested failed: %s" err);
    Discord_agents.Bot.reconcile_persisted_stop_requests bot;
    Alcotest.(check bool) "session removed"
      true
      (Option.is_none
         (Discord_agents.Session_store.find_opt bot.sessions ~thread_id:"control")))

let () =
  Alcotest.run "bot_defaults" [
    ("default agent", [
      Alcotest.test_case "busy control session defers default rotation" `Quick
        test_set_default_agent_defers_busy_control_session;
      Alcotest.test_case "default rotation rollback restores session state on save failure" `Quick
        test_set_default_agent_rolls_back_pending_rotation_when_session_save_fails;
      Alcotest.test_case "alignment rollback restores in-memory mutations on replacement failure" `Quick
        test_align_rolls_back_in_memory_mutations_when_replacement_raises;
      Alcotest.test_case "alignment rollback restores cleared pending rotation on replacement failure" `Quick
        test_align_rolls_back_cleared_pending_rotation_when_replacement_raises;
      Alcotest.test_case "alignment reraises fatal replacement exceptions" `Quick
        test_align_reraises_fatal_replacement_exception;
      Alcotest.test_case "explicit session override survives default rotation" `Quick
        test_set_default_agent_preserves_explicit_session_override;
      Alcotest.test_case "completed default rotation gets cleared" `Quick
        test_set_default_agent_clears_completed_default_rotation;
      Alcotest.test_case "idle pending session override starts fresh session" `Quick
        test_apply_pending_session_override_when_idle;
      Alcotest.test_case "same-kind pending clears" `Quick
        test_apply_pending_same_kind_clears_pending;
      Alcotest.test_case "same-kind session override pins existing session" `Quick
        test_apply_pending_same_kind_session_override_pins_existing_session;
      Alcotest.test_case "busy pending change stays pending" `Quick
        test_apply_pending_busy_session_leaves_pending_intact;
      Alcotest.test_case "default rotation rechecks policy after pressure clears" `Quick
        test_finalize_pending_default_rotation_uses_current_policy_after_pressure_clears;
      Alcotest.test_case "default rotation rechecks active rescue policy" `Quick
        test_finalize_pending_default_rotation_uses_current_rescue_policy;
      Alcotest.test_case "policy sync state marks converged uncleared marker as marker-clear-pending" `Quick
        test_policy_sync_state_is_marker_clear_pending_after_converged_rotation;
      Alcotest.test_case "policy sync state marks deferred default rotation as rotation-pending" `Quick
        test_policy_sync_state_is_rotation_pending_for_deferred_default_rotation;
      Alcotest.test_case "policy sync state stays rotation-pending after marker clears with busy rotation" `Quick
        test_policy_sync_state_is_rotation_pending_after_marker_cleared;
      Alcotest.test_case "policy sync clear warnings suppress repeated identical state+error pairs" `Quick
        test_policy_sync_clear_warning_is_suppressed_for_repeat_state_and_error;
      Alcotest.test_case "policy sync clear warnings re-log after error or state changes" `Quick
        test_policy_sync_clear_warning_logs_again_after_error_or_state_change;
      Alcotest.test_case "policy sync clear success re-arms warning logging" `Quick
        test_policy_sync_clear_success_rearms_warning;
      Alcotest.test_case "policy sync state ignores nonpersistent sessions" `Quick
        test_policy_sync_state_ignores_nonpersistent_sessions;
      Alcotest.test_case "reconcile preserves idle session override" `Quick
        test_reconcile_preserves_idle_session_override;
      Alcotest.test_case "reconcile rotates idle session to default" `Quick
        test_reconcile_rotates_idle_session_to_default_agent;
      Alcotest.test_case "best-effort policy sync does not raise in read-only mode" `Quick
        test_best_effort_policy_sync_does_not_raise_in_read_only_mode;
      Alcotest.test_case "startup reconcile clears stale rescue rotation after pressure clears" `Quick
        test_reconcile_clears_stale_rescue_rotation_after_pressure_clears;
      Alcotest.test_case "startup reconcile probes persistent workdir pressure before failback" `Quick
        test_reconcile_keeps_rescue_when_persistent_workdir_still_under_pressure;
      Alcotest.test_case "startup reconcile rotates project session without channel map" `Quick
        test_reconcile_rotates_project_session_without_channel_map;
      Alcotest.test_case "startup reconcile ignores thread session without channel map" `Quick
        test_reconcile_does_not_rotate_thread_session_without_channel_map;
      Alcotest.test_case "best-effort sync observes project workdir pressure" `Quick
        test_best_effort_sync_observes_project_workdir_pressure;
      Alcotest.test_case "reconcile applies persisted default rotation" `Quick
        test_reconcile_applies_persisted_pending_default_rotation;
      Alcotest.test_case "effective top-level agent uses rescue under pressure" `Quick
        test_effective_top_level_agent_uses_rescue_under_pressure;
      Alcotest.test_case "set rescue agent rotates idle top-level session under pressure" `Quick
        test_set_rescue_agent_rotates_idle_control_session_under_pressure;
      Alcotest.test_case "set rescue agent preserves idle session override under pressure" `Quick
        test_set_rescue_agent_preserves_idle_session_override_under_pressure;
      Alcotest.test_case "disable rescue agent rotates idle session to default under pressure" `Quick
        test_disable_rescue_agent_rotates_idle_session_to_default_under_pressure;
      Alcotest.test_case "set default agent under active rescue preserves rescue target" `Quick
        test_set_default_agent_under_active_rescue_preserves_rescue_target;
      Alcotest.test_case "reconcile rotates idle session to rescue under pressure" `Quick
        test_reconcile_rotates_idle_session_to_rescue_agent_under_pressure;
      Alcotest.test_case "stop idle session removes it" `Quick
        test_stop_idle_session_removes_it;
      Alcotest.test_case "stop idle queued session clears and removes it" `Quick
        test_stop_idle_queued_session_clears_and_removes_it;
      Alcotest.test_case "stop busy session requests stop" `Quick
        test_stop_busy_session_requests_stop;
      Alcotest.test_case "stop busy session is idempotent" `Quick
        test_stop_busy_session_is_idempotent;
      Alcotest.test_case "stop idle stopping session retries removal" `Quick
        test_stop_idle_stopping_session_retries_removal;
      Alcotest.test_case "finalize removes stopped session" `Quick
        test_finalize_session_run_removes_stopped_session;
      Alcotest.test_case "proc stat parser handles spaces and parens" `Quick
        test_ppid_of_proc_stat_line_handles_spaces_and_parens;
      Alcotest.test_case "stop_requested roundtrips through disk" `Quick
        test_stop_requested_roundtrips_through_disk;
      Alcotest.test_case "startup reconcile removes persisted stopping sessions" `Quick
        test_reconcile_persisted_stop_requests_removes_session;
    ]);
  ]
