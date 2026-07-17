(** Unix domain socket control API — JSON-RPC-style server.

    Runs as a daemon fiber inside the bot's main switch. Accepts
    connections on a Unix socket, reads one JSON request per connection,
    dispatches to bot operations, and writes one JSON response.

    Protocol: line-delimited JSON over Unix domain socket.
    Request:  {"method": "...", "params": {...}}
    Response: {"ok": true, ...} or {"error": "..."}

    Replaces the MCP server's direct session file / Discord REST access. *)

let socket_path () =
  Filename.concat (Resource.app_config_dir ()) "control.sock"

let raise_if_cancelled exn =
  match exn with
  | Eio.Cancel.Cancelled _
  | Out_of_memory
  | Stack_overflow
  | Sys.Break -> raise exn
  | _ -> ()

let max_request_size = 1_000_000

(** Read one line from a buffered reader, up to a size limit. *)
let read_line_limited reader =
  try
    let line = Eio.Buf_read.line reader in
    if String.length line > max_request_size then
      Error "request too large"
    else
      Ok line
  with
  | End_of_file -> Error "empty request"
  | Eio.Buf_read.Buffer_limit_exceeded -> Error "request too large"
  | exn ->
    raise_if_cancelled exn;
    Error (Printexc.to_string exn)

(** Send a JSON response and close. *)
let send_response flow json =
  let data = Yojson.Safe.to_string json ^ "\n" in
  try Eio.Flow.copy_string data flow
  with exn ->
    raise_if_cancelled exn;
    ()

let ok_response body =
  `Assoc (("ok", `Bool true) :: body)

let error_response msg =
  `Assoc [("error", `String msg)]

let json_of_int64 n =
  `Intlit (Int64.to_string n)

let cleanup_orphan_thread rest ~thread_id ~context =
  match Discord_rest.delete_channel rest ~channel_id:thread_id () with
  | Ok _ -> ()
  | Error err ->
    Logs.warn (fun m ->
      m "control_api: failed to clean up orphan thread %s after %s: %s"
        thread_id context err)

let cleanup_orphan_worktree ~project ~branch_info ~working_dir ~context =
  match branch_info with
  | None -> ()
  | Some branch_name ->
    (match Project.remove_worktree project ~branch_name
             ~worktree_path:working_dir with
     | Ok () -> ()
     | Error err ->
       Logs.warn (fun m ->
         m "control_api: failed to clean up orphan worktree %s after %s: %s"
           working_dir context err))

let remove_persisted_session_for_cleanup (bot : Bot.t) ~thread_id ~context =
  match Session_store.find_opt bot.sessions ~thread_id with
  | None -> true
  | Some _ ->
    try
      Session_store.remove bot.sessions ~thread_id;
      true
    with exn ->
      Logs.warn (fun m ->
        m "control_api: failed to remove persisted session %s during %s cleanup: %s"
          thread_id context (Printexc.to_string exn));
      false

let cleanup_start_artifacts (bot : Bot.t) ~project ~branch_info ~working_dir
    ~thread_id ~session_persisted ~context =
  let session_removed =
    (not session_persisted)
    || remove_persisted_session_for_cleanup bot ~thread_id ~context
  in
  if session_removed then begin
    cleanup_orphan_thread bot.rest ~thread_id ~context;
    cleanup_orphan_worktree ~project ~branch_info ~working_dir ~context
  end;
  session_removed

let cleanup_thread_session_artifacts (bot : Bot.t)
    ~thread_id ~session_persisted ~context =
  let session_removed =
    (not session_persisted)
    || remove_persisted_session_for_cleanup bot ~thread_id ~context
  in
  if session_removed then
    cleanup_orphan_thread bot.rest ~thread_id ~context;
  session_removed

(* ── Handlers ──────────────────────────────────────────────────── *)

let handle_health (bot : Bot.t) =
  ignore (Eio_unix.run_in_systhread (fun () ->
    Disk_health.preflight_state_mutation ()));
  let uptime = int_of_float (Unix.gettimeofday () -. bot.started_at) in
  let disk = Disk_health.snapshot () in
  let effective_top_level_agent =
    Bot.effective_top_level_agent_from_snapshot bot.settings disk
  in
  let rest_failures = Discord_rest.consecutive_rest_failures bot.rest in
  let transport_failures =
    Discord_rest.consecutive_transport_failures bot.rest
  in
  let rest_fields = [
    ("rest_degraded", `Bool (Discord_rest.rest_degraded bot.rest));
    ("rest_consecutive_failures", `Int rest_failures);
    ("rest_transport_degraded",
     `Bool (Discord_rest.transport_degraded bot.rest));
    ("rest_consecutive_transport_failures", `Int transport_failures);
    ("rest_retry_delay_ms",
     `Int (int_of_float (1000.0 *. Discord_rest.rest_retry_delay_s bot.rest)));
  ] in
  let optional_error_field key err =
    [
      (key, `String (Resource.truncate_utf8 ~max_bytes:1000
        (Resource.sanitize_utf8 (Resource.single_line err))));
    ]
  in
  let optional_rest_fields =
    match Discord_rest.last_rest_error bot.rest with
    | Some err -> optional_error_field "last_rest_error" err
    | None -> []
  in
  let optional_transport_fields =
    match Discord_rest.last_transport_error bot.rest with
    | Some err -> optional_error_field "last_rest_transport_error" err
    | None -> []
  in
  let gateway_fields = [
    ("gateway_connected", `Bool (Option.is_some bot.gateway.ws));
    ("gateway_resuming", `Bool bot.gateway.resuming);
    ("gateway_sequence",
     match bot.gateway.sequence with Some s -> `Int s | None -> `Null);
    ("gateway_supervisor_restarts", `Int bot.gateway_supervisor_restarts);
    ("control_api_restarts", `Int bot.control_api_restarts);
  ] in
  let optional_gateway_fields =
    (match bot.last_gateway_supervisor_error with
     | Some err ->
       [("last_gateway_supervisor_error",
         `String (Resource.truncate_utf8 ~max_bytes:1000
           (Resource.single_line err)))]
     | None -> [])
    @
    (match bot.last_control_api_error with
     | Some err ->
       [("last_control_api_error",
         `String (Resource.truncate_utf8 ~max_bytes:1000
           (Resource.single_line err)))]
     | None -> [])
    @
    (match bot.gateway.last_error with
     | Some err ->
       [("last_gateway_error",
         `String (Resource.truncate_utf8 ~max_bytes:1000
           (Resource.sanitize_utf8 (Resource.single_line err))))]
     | None -> [])
    @
    (match bot.gateway.last_payload_summary with
     | Some summary -> [("last_gateway_payload", `String summary)]
     | None -> [])
  in
  let disk_fields = [
    ("disk_mode", `String (Disk_health.string_of_mode disk.mode));
    ("disk_pressure", `Bool (Disk_health.pressure disk));
    ("disk_degraded", `Bool (disk.mode = Disk_health.Read_only));
    ("disk_warning_threshold_bytes",
     json_of_int64 disk.warning_threshold_bytes);
    ("disk_read_only_threshold_bytes",
     json_of_int64 disk.read_only_threshold_bytes);
  ] in
  let optional_disk_fields =
    (match disk.available_bytes with
     | Some available ->
       [("disk_free_bytes", json_of_int64 available)]
     | None -> [])
    @
    (match disk.checked_path with
     | Some path -> [("disk_path", `String path)]
     | None -> [])
    @
    (match disk.last_error with
     | Some err ->
       [("last_disk_error",
         `String (Resource.truncate_utf8 ~max_bytes:1000
           (Resource.single_line err)))]
     | None -> [])
  in
  ok_response ([
    ("uptime_seconds", `Int uptime);
    ("default_agent",
     `String (Config.string_of_agent_kind bot.settings.default_agent));
    ("effective_top_level_agent",
     `String (Config.string_of_agent_kind effective_top_level_agent));
    ("top_level_policy_sync_pending",
     `Bool (Bot.policy_sync_pending bot));
    ("top_level_policy_sync_state",
     `String (Bot.string_of_top_level_policy_sync_state
       (Bot.top_level_policy_sync_state_from_snapshot bot disk)));
    ("disk_rescue_active",
     `Bool (Bot.rescue_mode_active_from_snapshot bot.settings disk));
    ("sessions", `Int (Session_store.count bot.sessions));
    ("projects", `Int (List.length (Bot.projects bot)));
    ("channels", `Int (Channel_manager.count (Bot.channels bot)));
  ] @ rest_fields @ optional_rest_fields
    @ optional_transport_fields
    @ gateway_fields @ optional_gateway_fields
    @ disk_fields @ optional_disk_fields
    @
    match bot.settings.rescue_agent with
    | Some kind ->
      [("rescue_agent", `String (Config.string_of_agent_kind kind))]
    | None -> [])

let handle_list_projects (bot : Bot.t) =
  let projects = List.map (fun (p : Project.t) ->
    `Assoc [
      ("name", `String p.name);
      ("path", `String p.path);
      ("is_bare", `Bool p.is_bare);
    ]
  ) (Bot.projects bot) in
  ok_response [("projects", `List projects)]

let handle_list_sessions (bot : Bot.t) =
  let entries = Session_store.bindings bot.sessions in
  let sessions = List.map (fun (_tid, (s : Session_store.session)) ->
    (* single_line on project_name: an MCP client formats this back
       into Discord markdown bullets (scripts/mcp-server.py:234), so
       a literal newline in a project name would split the bullet
       just like it did for session summaries before 25d3546. *)
    `Assoc [
      ("project_name", `String (Resource.single_line s.project_name));
      ("agent_kind", `String (Config.string_of_agent_kind s.agent_kind));
      ("message_count", `Int s.message_count);
      ("thread_id", `String s.thread_id);
      ("session_id", `String s.session_id);
    ]
  ) entries in
  ok_response [("sessions", `List sessions)]

let handle_import_project (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let url = match params |> member "url" |> to_string_option with
    | Some url -> url
    | None -> failwith "missing url" in
  let name = params |> member "name" |> to_string_option in
  match Bot.import_project bot ~url ?name () with
  | Error err -> error_response err
  | Ok result ->
    let project = result.Bot.imported_project in
    ok_response [
      ("project_name", `String project.name);
      ("project_path", `String project.path);
      ("is_bare", `Bool project.is_bare);
      ("remote_url", match project.remote_url with
        | Some url -> `String url | None -> `Null);
      ("channel_id", `String result.imported_channel_id);
      ("working_dir", `String result.imported_working_dir);
      ("existing", `Bool result.imported_existing);
    ]

(** Extract [hours] from the optional params object, defaulting to 24. *)
let hours_param params =
  match params with
  | Some (`Assoc l) ->
    (match List.assoc_opt "hours" l with
     | Some (`Int h) -> h | _ -> 24)
  | _ -> 24

let handle_list_claude_sessions _bot params =
  let sessions = Claude_sessions.discover ~hours:(hours_param params) () in
  let items = List.map (fun (s : Claude_sessions.info) ->
    let sid_short = Resource.short_id s.session_id in
    let age_min = int_of_float ((Unix.gettimeofday () -. s.mtime) /. 60.0) in
    `Assoc [
      ("session_id", `String s.session_id);
      ("session_id_short", `String sid_short);
      ("project_dir", `String s.project_dir);
      ("working_dir", `String s.working_dir);
      ("summary", `String s.summary);
      ("age_minutes", `Int age_min);
    ]
  ) sessions in
  ok_response [("sessions", `List items)]

let handle_list_codex_sessions _bot params =
  let sessions = Codex_sessions.discover ~hours:(hours_param params) () in
  let items = List.map (fun (s : Codex_sessions.info) ->
    let sid_short = Resource.short_id s.session_id in
    let age_min = int_of_float ((Unix.gettimeofday () -. s.mtime) /. 60.0) in
    `Assoc [
      ("session_id", `String s.session_id);
      ("session_id_short", `String sid_short);
      ("working_dir", `String s.working_dir);
      ("summary", `String s.summary);
      ("age_minutes", `Int age_min);
    ]
  ) sessions in
  ok_response [("sessions", `List items)]

let handle_list_gemini_sessions _bot params =
  let sessions = Gemini_sessions.discover ~hours:(hours_param params) () in
  let items = List.map (fun (s : Gemini_sessions.info) ->
    let sid_short = Resource.short_id s.session_id in
    let age_min = int_of_float ((Unix.gettimeofday () -. s.mtime) /. 60.0) in
    `Assoc [
      ("session_id", `String s.session_id);
      ("session_id_short", `String sid_short);
      ("working_dir", `String s.working_dir);
      ("summary", `String s.summary);
      ("age_minutes", `Int age_min);
    ]
  ) sessions in
  ok_response [("sessions", `List items)]

let handle_start_session (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  (* Refuse new sessions during a graceful restart: the bot is
     waiting for in-flight session.processing flags to clear before
     exec'ing the new build, so accepting a new session here either
     delays the restart (fork holds the flag) or races with handoff.
     Mirrors the warning [handle_thread_message] posts to in-flight
     threads while draining; for start_session there's no thread to
     warn in yet, so we just refuse. *)
  if bot.draining then
    error_response "Bot is restarting; try again shortly."
  else
    match Disk_health.preflight_state_mutation () with
    | Error err -> error_response err
    | Ok () ->
      let project_str = params |> member "project" |> to_string in
      let kind_str = match params |> member "agent" |> to_string_option with
        | Some s -> s
        | None ->
          Config.string_of_agent_kind (Bot.effective_top_level_agent bot)
      in
      let kind = match Config.agent_kind_of_string kind_str with
        | Ok k -> k | Error _ -> failwith ("unknown agent: " ^ kind_str) in
      let thread_name = params |> member "thread_name" |> to_string_option in
      let initial_prompt = params |> member "initial_prompt" |> to_string_option in
      let initial_prompt = match initial_prompt with
        | Some s ->
          let s = String.trim s in
          (* Cap below Discord's 2000-byte message limit so the prompt
             posts as a single message; we use that message as the
             reaction anchor for the auto-triggered agent run.
             [Resource.truncate_utf8] is codepoint-aware (a raw
             [String.sub] would split a multi-byte character, and the
             send-path sanitization in PR #33 would then "repair" the
             cut byte to U+FFFD, silently corrupting the last character).
             We DON'T use [normalize_summary] here because its
             [single_line] pass collapses \\n / \\r / \\t and would
             flatten a structured prompt (code blocks, bullets,
             paragraph breaks) into one line — and the prompt is posted
             as a standalone Discord message, not embedded in a markdown
             list, so there are no sibling bullets to defend against.
             Cap is 1900 *bytes*; the MCP schema description matches. *)
          let s = Resource.truncate_utf8 ~max_bytes:1900 s in
          if s = "" then None else Some s
        | None -> None in
      match Command.find_project_fuzzy (Bot.projects bot) project_str with
      | None ->
        (* Sanitize the MCP-supplied project string before echoing it
           back: the MCP client renders the error into Discord, where a
           literal newline in the input would let the rest of the error
           land at column 0 and parse as a sibling bullet. Same defense
           Bot.handle_command applies for the Discord !start path. *)
        error_response (Printf.sprintf "No project matching '%s'."
          (Resource.single_line project_str))
      | Some p ->
        let kind_str = Config.string_of_agent_kind kind in
        let branch_name = Printf.sprintf "agent/%s-%s"
          kind_str (String.sub (Resource.generate_uuid ()) 0 8) in
        let working_dir, branch_info =
          match Project.create_worktree p ~branch_name with
          | Ok wt -> wt, Some branch_name
          | Error e ->
            Logs.warn (fun m -> m "control_api: worktree failed: %s" e);
            (match Bot.working_dir_of_project p with
             | Ok wd -> wd, None
             | Error _ -> "", None)
        in
        if working_dir = "" then begin
          cleanup_orphan_worktree ~project:p ~branch_info ~working_dir
            ~context:"working directory resolution failure";
          error_response "No working directory available."
        end else
          let thread_display_name = match thread_name with
            | Some n when String.length (String.trim n) > 0 ->
              let n = String.trim n in
              if String.length n > 80 then
                Resource.truncate_utf8 ~max_bytes:80 n
              else
                n
            | _ -> Printf.sprintf "%s / %s" kind_str p.name
          in
          let thread_parent =
            match Channel_manager.find_or_create ~rest:bot.rest
                    ~guild_id:bot.config.guild_id ~project:p (Bot.channels bot) with
            | Some ch_id -> ch_id
            | None ->
              (match bot.config.control_channel_id with
               | Some ctl -> ctl | None -> "")
          in
          if thread_parent = "" then begin
            cleanup_orphan_worktree ~project:p ~branch_info ~working_dir
              ~context:"thread parent resolution failure";
            error_response "No channel found for thread creation."
          end
          else
            match Discord_rest.create_thread_no_message bot.rest
                    ~channel_id:thread_parent ~name:thread_display_name () with
            | Error e ->
              cleanup_orphan_worktree ~project:p ~branch_info ~working_dir
                ~context:"thread creation failure";
              error_response (Printf.sprintf "Failed to create thread: %s" e)
            | Ok thread_ch ->
          let session_id = Resource.generate_uuid () in
          let branch_str = match branch_info with
            | Some b -> Printf.sprintf "\nBranch: `%s`" b | None -> "" in
          let starter_text = match initial_prompt with
            | Some _ ->
              "Working on the prompt below \u{2014} send a message any \
               time to add to the conversation."
            | None -> "Send a message to interact." in
          (* Build the session struct with [initial_prompt:None] —
             when an initial_prompt was supplied, we post it as a
             visible Discord message below and feed that message to
             the agent runner directly. Stashing it in
             [session.initial_prompt] would silently prepend it to
             whatever the user sends next instead, so the user never
             sees what context the agent received. *)
          let session = Session_store.make_session
            ~project_name:p.name ~working_dir ~agent_kind:kind
            ~session_id ~thread_id:thread_ch.Discord_types.id
            ~system_prompt:None ~initial_prompt:None () in
          (* Race-safe ordering for the auto-trigger.

             When an initial_prompt is set, we MUST publish the
             session to the store with [processing = true] *before*
             posting any visible Discord message. Any of those posts
             (announcement OR prompt) yields, and a user typing
             into the freshly-created thread during that yield will
             reach the gateway before us. If we hadn't added the
             session yet, [Bot.handle_thread_message] would
             [find_opt -> None] and silently drop the message; if
             we'd added it with [processing = false], the user's
             message would run *first* and our auto-trigger would
             queue behind it. Setting [processing <- true] first
             and then [Session_store.add] makes any racing user
             message queue correctly into [pending_queue]. The
             [fork_initial_prompt_run] fiber drains that queue when
             its agent run finishes.

             For the no-prompt path we add normally (no
             [processing] lock); the user's first message goes
             through the standard [handle_thread_message] flow. *)
          match initial_prompt with
          | None ->
            let session_persisted = ref false in
            (try
               Session_store.add bot.sessions ~thread_id:thread_ch.id session;
               session_persisted := true;
               ignore (Discord_rest.create_message bot.rest
                 ~channel_id:thread_ch.id
                 ~content:(Printf.sprintf
                   "**%s** session started for **%s**%s\nWorking in: `%s`\n%s"
                   kind_str p.name branch_str working_dir starter_text) ());
               ok_response [
                 ("thread_id", `String thread_ch.id);
                 ("working_dir", `String working_dir);
                 ("branch", match branch_info with
                   | Some b -> `String b | None -> `Null);
                 ("project_name", `String p.name);
                 ("session_id", `String session_id);
               ]
             with exn ->
               ignore (cleanup_start_artifacts bot ~project:p ~branch_info ~working_dir
                 ~thread_id:thread_ch.id
                 ~session_persisted:!session_persisted
                 ~context:"session startup failure");
               error_response (Printf.sprintf "Failed to persist session: %s"
                 (Printexc.to_string exn)))
          | Some prompt ->
            session.processing <- true;
            let session_persisted = ref false in
            (try
               Session_store.add bot.sessions ~thread_id:thread_ch.id session;
               session_persisted := true;
               ignore (Discord_rest.create_message bot.rest
                 ~channel_id:thread_ch.id
                 ~content:(Printf.sprintf
                   "**%s** session started for **%s**%s\nWorking in: `%s`\n%s"
                   kind_str p.name branch_str working_dir starter_text) ());
               match Discord_rest.create_message bot.rest
                       ~channel_id:thread_ch.id ~content:prompt () with
               | Error e ->
                 (* Roll back: the session is half-published (in store
                    but [processing] locked, no agent fiber will fire),
                    and the thread holds an orphan announcement. Remove
                    the session, delete the thread, and surface the
                    error to the MCP caller. *)
                 Logs.warn (fun m -> m
                   "control_api: failed to post initial_prompt: %s" e);
                 session.processing <- false;
                 let cleaned =
                   cleanup_start_artifacts bot ~project:p ~branch_info
                     ~working_dir ~thread_id:thread_ch.id
                     ~session_persisted:true
                     ~context:"prompt-post failure"
                 in
                 let suffix =
                   if cleaned then ""
                   else " The session was persisted and could not be rolled back; send a message in the new thread or stop it manually."
                 in
                 error_response (Printf.sprintf
                   "Failed to post initial_prompt: %s%s" e suffix)
               | Ok prompt_msg ->
                 Bot.fork_initial_prompt_run bot
                   ~session ~msg:prompt_msg;
                 ok_response [
                   ("thread_id", `String thread_ch.id);
                   ("working_dir", `String working_dir);
                   ("branch", match branch_info with
                     | Some b -> `String b | None -> `Null);
                   ("project_name", `String p.name);
                   ("session_id", `String session_id);
                 ]
             with exn ->
               session.processing <- false;
               ignore (cleanup_start_artifacts bot ~project:p ~branch_info ~working_dir
                 ~thread_id:thread_ch.id
                 ~session_persisted:!session_persisted
                 ~context:"session startup failure");
               error_response (Printf.sprintf "Failed to persist session: %s"
                 (Printexc.to_string exn)))

let handle_resume_session (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  (* Same drain refusal as handle_start_session: resume creates a
     thread + persistent session, and accepting one mid-restart
     either delays the restart or leaves a half-set-up session
     stranded. The MCP caller can retry after restart. *)
  if bot.draining then
    error_response "Bot is restarting; try again shortly."
  else
    match Disk_health.preflight_state_mutation () with
    | Error err -> error_response err
    | Ok () ->
      let sid_prefix = params |> member "session_id" |> to_string in
      let kind = match params |> member "kind" |> to_string_option with
        | None -> None
        | Some s ->
          (match Config.agent_kind_of_string (String.lowercase_ascii s) with
           | Ok k -> Some k | Error _ -> None)
      in
      (* Mirror Bot.handle_command's Resume_session dispatch: explicit
         kind looks up its own store; None tries the current effective
         top-level agent first, then the remaining stores. *)
      let try_claude () =
        match Claude_sessions.find_by_prefix sid_prefix with
        | Some (sid, wd) -> Some (Config.Claude, sid, wd) | None -> None
      in
      let try_codex () =
        match Codex_sessions.find_by_prefix sid_prefix with
        | Some (sid, wd) -> Some (Config.Codex, sid, wd) | None -> None
      in
      let try_gemini () =
        match Gemini_sessions.find_by_prefix sid_prefix with
        | Some (sid, wd) -> Some (Config.Gemini, sid, wd) | None -> None
      in
      let try_kind = function
        | Config.Claude -> try_claude ()
        | Config.Codex -> try_codex ()
        | Config.Gemini -> try_gemini ()
      in
      let found = match kind with
        | Some k -> try_kind k
        | None ->
          Config.find_with_preferred_agent
            (Bot.effective_top_level_agent bot) try_kind
      in
      match found with
      | None ->
        error_response (Bot.resume_not_found_message ~kind ~sid_prefix)
      | Some (_, full_sid, "") ->
    (* See Bot.handle_command Resume_session for the rationale —
       Gemini sessions with unresolvable projectHash arrive here
       with an empty working_dir; running gemini with an empty cwd
       writes settings.json into the bot's directory. *)
        error_response (Printf.sprintf
          "Cannot resume session '%s': its working directory could not \
           be resolved." (Resource.short_id full_sid))
      | Some (resolved_kind, full_sid, raw_working_dir) ->
        let kind_label = Config.string_of_agent_kind resolved_kind in
        let kind_title = String.capitalize_ascii kind_label in
        let sid_short = Resource.short_id full_sid in
        let fallback_channel = match bot.config.control_channel_id with
          | Some ctl -> ctl | None -> ""
        in
        let { Bot.thread_parent; working_dir; project_name } =
          Bot.resolve_resume_target bot
            ~raw_working_dir ~kind_label ~fallback_channel
        in
        if thread_parent = "" then
          error_response "No channel found for thread creation."
        else
          let thread_name =
            Printf.sprintf "resume %s / %s" kind_label sid_short in
          (match Discord_rest.create_thread_no_message bot.rest
                  ~channel_id:thread_parent ~name:thread_name () with
          | Error e -> error_response (Printf.sprintf "Failed to create thread: %s" e)
          | Ok thread_ch ->
        (* session_id_confirmed:true is critical: see Bot.handle_command
           Resume_session for the rationale — without it, Gemini resumes
           start fresh chats instead of resuming. *)
        let session = Session_store.make_session
          ~project_name ~working_dir ~agent_kind:resolved_kind
          ~session_id:full_sid ~session_id_confirmed:true
          ~message_count:1
          ~thread_id:thread_ch.Discord_types.id
          ~system_prompt:None ~initial_prompt:None () in
        let session_persisted = ref false in
        (try
           Session_store.add bot.sessions ~thread_id:thread_ch.id session;
           session_persisted := true;
           ignore (Discord_rest.create_message bot.rest ~channel_id:thread_ch.id
             ~content:(Printf.sprintf
               "**Resumed** %s session `%s`\nWorking in: `%s`\nSend a message to continue."
               kind_title sid_short working_dir) ());
           ok_response [
             ("thread_id", `String thread_ch.id);
             ("working_dir", `String working_dir);
             ("session_id", `String full_sid);
             ("project_name", `String project_name);
             ("agent_kind", `String kind_label);
           ]
         with exn ->
           ignore (cleanup_thread_session_artifacts bot ~thread_id:thread_ch.id
             ~session_persisted:!session_persisted
             ~context:"resume startup failure");
           error_response (Printf.sprintf
             "Failed to persist resumed session: %s"
             (Printexc.to_string exn))))

let handle_default_agent (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  ignore (Disk_health.preflight_state_mutation ());
  let requested =
    match params with
    | Some p ->
      (match p |> member "agent" |> to_string_option with
       | None -> None
       | Some s ->
         (match Config.agent_kind_of_string (String.lowercase_ascii s) with
          | Ok kind -> Some kind
          | Error msg -> failwith msg))
    | None -> None
  in
  match requested with
  | None ->
    let disk = Disk_health.snapshot () in
    ok_response ([
      ("agent",
       `String (Config.string_of_agent_kind bot.settings.default_agent));
      ("effective_top_level_agent",
       `String (Config.string_of_agent_kind
         (Bot.effective_top_level_agent_from_snapshot bot.settings disk)));
      ("top_level_policy_sync_pending",
       `Bool (Bot.policy_sync_pending bot));
      ("top_level_policy_sync_state",
       `String (Bot.string_of_top_level_policy_sync_state
         (Bot.top_level_policy_sync_state_from_snapshot bot disk)));
      ("disk_rescue_active",
       `Bool (Bot.rescue_mode_active_from_snapshot bot.settings disk));
    ] @
    match bot.settings.rescue_agent with
    | Some kind ->
      [("rescue_agent", `String (Config.string_of_agent_kind kind))]
    | None -> [])
  | Some kind ->
    (match Bot.set_default_agent bot kind with
     | Error err -> error_response err
     | Ok rotation ->
       let disk = Disk_health.snapshot () in
       ok_response ([
         ("agent", `String (Config.string_of_agent_kind kind));
         ("effective_top_level_agent",
          `String (Config.string_of_agent_kind
            (Bot.effective_top_level_agent_from_snapshot bot.settings disk)));
         ("top_level_policy_sync_pending",
          `Bool (Bot.policy_sync_pending bot));
         ("top_level_policy_sync_state",
          `String (Bot.string_of_top_level_policy_sync_state
            (Bot.top_level_policy_sync_state_from_snapshot bot disk)));
         ("disk_rescue_active",
          `Bool (Bot.rescue_mode_active_from_snapshot bot.settings disk));
         ("reset_count", `Int rotation.Bot.reset_count);
         ("busy_count", `Int rotation.Bot.busy_count);
       ] @
       match bot.settings.rescue_agent with
       | Some rescue ->
         [("rescue_agent", `String (Config.string_of_agent_kind rescue))]
       | None -> []))

let handle_rescue_agent (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  ignore (Disk_health.preflight_state_mutation ());
  let requested =
    match params with
    | Some p ->
      (match p |> member "agent" |> to_string_option with
       | None -> None
       | Some s when String.equal (String.lowercase_ascii s) "off" -> Some None
       | Some s ->
         (match Config.agent_kind_of_string (String.lowercase_ascii s) with
          | Ok kind -> Some (Some kind)
          | Error msg -> failwith msg))
    | None -> None
  in
  match requested with
  | None ->
    let disk = Disk_health.snapshot () in
    ok_response ([
      ("effective_top_level_agent",
       `String (Config.string_of_agent_kind
         (Bot.effective_top_level_agent_from_snapshot bot.settings disk)));
      ("top_level_policy_sync_pending",
       `Bool (Bot.policy_sync_pending bot));
      ("top_level_policy_sync_state",
       `String (Bot.string_of_top_level_policy_sync_state
         (Bot.top_level_policy_sync_state_from_snapshot bot disk)));
      ("disk_rescue_active",
       `Bool (Bot.rescue_mode_active_from_snapshot bot.settings disk));
    ] @
    match bot.settings.rescue_agent with
    | Some kind ->
      [("agent", `String (Config.string_of_agent_kind kind))]
    | None -> [("agent", `Null)])
  | Some kind ->
    (match Bot.set_rescue_agent bot kind with
     | Error err -> error_response err
     | Ok rotation ->
       let disk = Disk_health.snapshot () in
       ok_response [
         ("agent",
          match kind with
          | Some kind -> `String (Config.string_of_agent_kind kind)
          | None -> `Null);
         ("effective_top_level_agent",
          `String (Config.string_of_agent_kind
            (Bot.effective_top_level_agent_from_snapshot bot.settings disk)));
         ("top_level_policy_sync_pending",
          `Bool (Bot.policy_sync_pending bot));
         ("top_level_policy_sync_state",
          `String (Bot.string_of_top_level_policy_sync_state
            (Bot.top_level_policy_sync_state_from_snapshot bot disk)));
         ("disk_rescue_active",
          `Bool (Bot.rescue_mode_active_from_snapshot bot.settings disk));
         ("reset_count", `Int rotation.Bot.reset_count);
         ("busy_count", `Int rotation.Bot.busy_count);
       ])

let session_not_found_response =
  error_response "Session not found."

let model_field (session : Session_store.session) =
  match session.model with
  | Some model -> ("model", `String model)
  | None -> ("model", `Null)

let effort_field (session : Session_store.session) =
  match session.reasoning_effort with
  | Some effort ->
    ("effort", `String (Config.string_of_reasoning_effort effort))
  | None -> ("effort", `Null)

let goal_json (goal : Session_store.session_goal) =
  `Assoc ([
    ("objective", `String goal.objective);
    ("status", `String (Session_store.string_of_goal_status goal.status));
  ] @
  match goal.token_budget with
  | Some n -> [("token_budget", `Int n)]
  | None -> [])

let goal_field (session : Session_store.session) =
  match session.goal with
  | Some goal -> ("goal", goal_json goal)
  | None -> ("goal", `Null)

let login_help_json agent =
  let command, note =
    match agent with
    | Config.Claude ->
      "claude auth login",
      "Run this on the host where discord-agents runs. If your Claude CLI uses a different auth command, run that equivalent login locally."
    | Config.Codex ->
      "codex login",
      "Run this on the host where discord-agents runs. For trusted automation, CODEX_ACCESS_TOKEN can be piped to `codex login --with-access-token`."
    | Config.Gemini ->
      "gcloud auth application-default login",
      "Run this on the host where discord-agents runs. Gemini CLI does not expose a non-interactive login command in this install; if your Gemini setup uses another provider, run that provider's local login flow."
  in
  `Assoc [
    ("agent", `String (Config.string_of_agent_kind agent));
    ("command", `String command);
    ("note", `String note);
  ]

let string_list_json values =
  `List (List.map (fun value -> `String value) values)

let clear_values_json =
  `List [`String "default"; `String ""; `Null]

let effort_notes agent =
  if Config.supports_reasoning_effort agent then
    "Supported effort values are listed in values and validated for this thread's agent."
  else
    "This agent does not expose a reasoning effort flag in this integration."

let configuration_options_json agent =
  `Assoc [
    ("agent_kind", `Assoc [
      ("values", string_list_json ["claude"; "codex"; "gemini"]);
      ("current_thread_value", `String (Config.string_of_agent_kind agent));
      ("mutable_for_current_session", `Bool false);
      ("set_with", `String "start_session agent for new sessions, or Discord session-agent outside this MCP config surface");
    ]);
    ("model", `Assoc [
      ("values", `String "any non-empty model string accepted by the selected agent CLI");
      ("max_bytes", `Int 200);
      ("clear_values", clear_values_json);
    ]);
    ("effort", `Assoc [
      ("supported", `Bool (Config.supports_reasoning_effort agent));
      ("values", string_list_json
        (Config.reasoning_effort_strings_for_agent agent));
      ("clear_values", clear_values_json);
      ("notes", `String (effort_notes agent));
    ]);
    ("goal", `Assoc [
      ("supported", `Bool (Config.equal_agent_kind agent Config.Codex));
      ("objective", `Assoc [
        ("values", `String "any non-empty string");
        ("max_bytes", `Int 4000);
      ]);
      ("status_values", string_list_json
        ["active"; "paused"; "blocked"; "usageLimited"; "budgetLimited"; "complete"]);
      ("token_budget", `Assoc [
        ("values", `String "positive integer or null");
      ]);
      ("clear_values", `String "clear=true");
      ("mechanism", `String (match agent with
        | Config.Codex -> "bot_prompt_context; native /goal requires codex app-server"
        | _ -> "unsupported"));
    ]);
    ("login", `Assoc [
      ("repair_tool", `String "start_login_flow");
      ("agent_values", string_list_json ["claude"; "codex"; "gemini"]);
    ]);
  ]

let command_briefing session =
  let effort_text =
    if not (Config.supports_reasoning_effort session.Session_store.agent_kind)
    then
      "Effort is unsupported for this thread's agent."
    else
      "Set effort with set_effort."
  in
  Printf.sprintf
    "Single command: get_agent_config {\"thread_id\":\"%s\"}. Agent kind is read-only here after session creation. Set model with set_model. %s Set or update Codex goals with set_goal, and get login repair instructions with start_login_flow."
    session.Session_store.thread_id effort_text

let handle_get_agent_config (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let thread_id = params |> member "thread_id" |> to_string in
  match Session_store.find_opt bot.sessions ~thread_id with
  | None -> session_not_found_response
  | Some session ->
    ok_response [
      ("thread_id", `String session.thread_id);
      ("agent_kind", `String (Config.string_of_agent_kind session.agent_kind));
      model_field session;
      effort_field session;
      goal_field session;
      ("login_help", login_help_json session.agent_kind);
      ("goal_mechanism",
       `String (match session.agent_kind with
         | Config.Codex -> "bot_prompt_context; native /goal requires codex app-server"
         | _ -> "unsupported"));
      ("configuration_options", configuration_options_json session.agent_kind);
      ("command_briefing", `String (command_briefing session));
    ]

let string_param_opt params name =
  let open Yojson.Safe.Util in
  match params |> member name with
  | `Null -> None
  | json -> to_string_option json

let json_field params name =
  match params with
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let handle_set_model (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let thread_id = params |> member "thread_id" |> to_string in
  match Session_store.find_opt bot.sessions ~thread_id with
  | None -> session_not_found_response
  | Some session ->
    let model =
      match json_field params "model" with
      | None ->
        failwith "model is required; use null, empty string, or default to clear"
      | Some `Null -> None
      | Some (`String s) ->
        let s = String.trim s in
        if s = "" || String.equal (String.lowercase_ascii s) "default"
        then None
        else Some (Resource.truncate_utf8 ~max_bytes:200 s)
      | Some _ -> failwith "model must be a string or null"
    in
    (match Session_store.set_model bot.sessions session model with
     | Error err -> error_response err
     | Ok () ->
       ok_response [
         ("thread_id", `String thread_id);
         ("agent_kind", `String (Config.string_of_agent_kind session.agent_kind));
         model_field session;
       ])

let handle_set_effort (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let thread_id = params |> member "thread_id" |> to_string in
  match Session_store.find_opt bot.sessions ~thread_id with
  | None -> session_not_found_response
  | Some session ->
    let effort =
      match json_field params "effort" with
      | None ->
        failwith "effort is required; use null, empty string, or default to clear"
      | Some `Null -> None
      | Some (`String s) ->
        let s = String.trim (String.lowercase_ascii s) in
        if s = "" || String.equal s "default" then None
        else
          (match Config.reasoning_effort_of_string s with
           | Ok effort -> Some effort
           | Error msg -> failwith msg)
      | Some _ -> failwith "effort must be a string or null"
    in
    (match Config.validate_reasoning_effort_for_agent
             session.agent_kind effort with
     | Error err -> error_response err
     | Ok () ->
       (match Session_store.set_reasoning_effort bot.sessions session effort with
        | Error err -> error_response err
        | Ok () ->
          ok_response [
            ("thread_id", `String thread_id);
            ("agent_kind", `String (Config.string_of_agent_kind session.agent_kind));
            effort_field session;
          ]))

let handle_set_goal (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let thread_id = params |> member "thread_id" |> to_string in
  match Session_store.find_opt bot.sessions ~thread_id with
  | None -> session_not_found_response
  | Some session ->
    if not (Config.equal_agent_kind session.agent_kind Config.Codex) then
      error_response
        "Goal config is currently supported only for Codex sessions."
    else
      let clear =
        match json_field params "clear" with
        | Some (`Bool b) -> b
        | _ -> false
      in
      let parse_status = function
        | None | Some `Null -> None
        | Some (`String s) ->
          let s = String.trim s in
          if s = "" then None
          else
            (match Session_store.goal_status_of_string s with
             | Ok status -> Some status
             | Error msg -> failwith msg)
        | Some _ -> failwith "status must be a string"
      in
      let parse_token_budget current = function
        | None -> current
        | Some `Null -> None
        | Some (`Int n) when n > 0 -> Some n
        | Some (`Intlit s) ->
          (match int_of_string_opt s with
           | Some n when n > 0 -> Some n
           | _ -> failwith "token_budget must be positive")
        | Some (`Int _) -> failwith "token_budget must be positive"
        | Some _ -> failwith "token_budget must be a positive integer or null"
      in
      let goal =
        if clear then None
        else
          let current = session.goal in
          let objective =
            match json_field params "objective", current with
            | Some (`String objective), _ ->
              let objective = String.trim objective in
              if objective = "" then
                failwith "objective must be non-empty"
              else
                Resource.truncate_utf8 ~max_bytes:4000 objective
            | Some `Null, Some goal
            | None, Some goal -> goal.objective
            | Some `Null, None
            | None, None ->
              failwith "objective is required when setting a new goal"
            | Some _, _ -> failwith "objective must be a string"
          in
          let status =
            match parse_status (json_field params "status"), current with
            | Some status, _ -> status
            | None, Some goal -> goal.status
            | None, None -> Session_store.Goal_active
          in
          let current_budget = Option.bind current (fun goal ->
            goal.token_budget)
          in
          let token_budget = parse_token_budget current_budget
            (json_field params "token_budget") in
          Some { Session_store.objective = objective; status; token_budget }
      in
      (match Session_store.set_goal bot.sessions session goal with
       | Error err -> error_response err
       | Ok () ->
         ok_response [
           ("thread_id", `String thread_id);
           goal_field session;
           ("goal_mechanism",
            `String "bot_prompt_context; native /goal requires codex app-server");
         ])

let handle_start_login_flow (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let resolve_requested_agent () =
    match params with
    | Some p ->
      (match p |> member "thread_id" |> to_string_option with
       | Some thread_id ->
         (match Session_store.find_opt bot.sessions ~thread_id with
          | Some session -> Some session.agent_kind
          | None -> raise Not_found)
       | None ->
         (match p |> member "agent" |> to_string_option with
          | Some s ->
            (match Config.agent_kind_of_string (String.lowercase_ascii s) with
             | Ok agent -> Some agent
             | Error msg -> failwith msg)
          | None -> None))
    | None -> None
  in
  match resolve_requested_agent () with
  | exception Not_found -> session_not_found_response
  | requested_agent ->
    let agent = Option.value requested_agent
      ~default:(Bot.effective_top_level_agent bot) in
    ok_response [
      ("login", login_help_json agent);
      ("message",
       `String "Login is handled by the local agent CLI, not by discord-agents OAuth. Run the command on the bot host, then retry the session turn.");
    ]

let handle_stop_session (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let thread_id = params |> member "thread_id" |> to_string in
  let dropped_text dropped_count =
    if dropped_count = 0 then ""
    else Printf.sprintf " Dropped %d queued message%s."
      dropped_count (if dropped_count = 1 then "" else "s")
  in
  match Bot.stop_session bot ~thread_id with
  | Bot.Session_not_found ->
    error_response "Session not found."
  | Bot.Session_stopped { project_name; dropped_count } ->
    ok_response [
      ("state", `String "stopped");
      ("project_name", `String project_name);
      ("thread_id", `String thread_id);
      ("dropped_count", `Int dropped_count);
      ("message",
       `String (Printf.sprintf "Stopped session for %s.%s"
                  project_name (dropped_text dropped_count)));
    ]
  | Bot.Session_stopping { project_name; had_running_process; dropped_count } ->
    let message =
      if had_running_process then
        Printf.sprintf "Stopping session for %s. Terminating the active agent process."
          project_name
      else
        Printf.sprintf
          "Stopping session for %s. The active session will stop as soon as its current turn or agent startup finishes."
          project_name
    in
    let message = message ^ dropped_text dropped_count in
    ok_response [
      ("state", `String "stopping");
      ("project_name", `String project_name);
      ("thread_id", `String thread_id);
      ("had_running_process", `Bool had_running_process);
      ("dropped_count", `Int dropped_count);
      ("message", `String message);
    ]
  | Bot.Session_already_stopping { project_name } ->
    ok_response [
      ("state", `String "already_stopping");
      ("project_name", `String project_name);
      ("thread_id", `String thread_id);
      ("message",
       `String (Printf.sprintf "Session for %s is already stopping." project_name));
    ]
  | Bot.Session_stop_failed err ->
    error_response (Printf.sprintf "Failed to stop session: %s" err)

let handle_send_message (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let thread_id = params |> member "thread_id" |> to_string in
  let message = params |> member "message" |> to_string in
  let source_thread_id =
    match params |> member "source_thread_id" |> to_string_option with
    | Some s when String.trim s <> "" -> Some (String.trim s)
    | _ -> None
  in
  let remaining_hops =
    let raw = params |> member "remaining_hops" in
    match raw with
    | `Null -> Ok None
    | `Int n -> Ok (Some n)
    | `Intlit s | `String s ->
      (match int_of_string_opt s with
       | Some n -> Ok (Some n)
       | None -> Error "remaining_hops must be an integer")
    | _ -> Error "remaining_hops must be an integer"
  in
  match remaining_hops with
  | Error err -> error_response err
  | Ok remaining_hops ->
  match Bot.send_inter_agent_message bot ?source_thread_id ?remaining_hops
          ~thread_id ~message () with
  | Bot.Inter_agent_message_sent sent ->
    ok_response [
      ("state", `String "sent");
      ("thread_id", `String sent.thread_id);
      ("message_id", `String sent.message_id);
      ("remaining_hops", `Int sent.remaining_hops);
      ("message", `String (Printf.sprintf
        "Sent message to <#%s>. remaining_hops=%d"
        sent.thread_id sent.remaining_hops));
    ]
  | Bot.Inter_agent_message_posted_not_routed sent ->
    ok_response [
      ("state", `String "posted_not_routed");
      ("thread_id", `String sent.thread_id);
      ("message_id", `String sent.message_id);
      ("remaining_hops", `Int sent.remaining_hops);
      ("message", `String (Printf.sprintf
        "Posted message to <#%s>, but the target session disappeared before routing. remaining_hops=%d"
        sent.thread_id sent.remaining_hops));
    ]
  | Bot.Inter_agent_message_rejected err ->
    error_response err

let handle_restart (bot : Bot.t) =
  Bot.trigger_restart bot ~notify:(fun msg ->
    Logs.info (fun m -> m "control_api restart: %s" msg));
  ok_response [("message", `String "Restart initiated.")]

let handle_rename_thread (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let thread_id = params |> member "thread_id" |> to_string in
  let name = params |> member "name" |> to_string in
  match Discord_rest.modify_channel bot.rest ~channel_id:thread_id ~name () with
  | Ok _ -> ok_response [("message", `String (Printf.sprintf "Renamed to %s." name))]
  | Error e -> error_response (Printf.sprintf "Rename failed: %s" e)

let handle_refresh_projects (bot : Bot.t) =
  match Bot.refresh_projects bot with
  | None -> error_response "Refresh already in progress."
  | Some (old_count, new_count) ->
    ok_response [
      ("total", `Int new_count);
      ("delta", `Int (new_count - old_count));
    ]

let handle_cleanup_channels (bot : Bot.t) =
  match Channel_manager.cleanup ~rest:bot.rest
          ~guild_id:bot.config.guild_id ~projects:(Bot.projects bot) (Bot.channels bot) with
  | Error e -> error_response (Printf.sprintf "Cleanup failed: %s" e)
  | Ok 0 -> ok_response [("deleted", `Int 0); ("message", `String "No stale channels.")]
  | Ok n -> ok_response [("deleted", `Int n);
      ("message", `String (Printf.sprintf "Cleaned up %d stale channels." n))]

(* ── Router ────────────────────────────────────────────────────── *)

let dispatch (bot : Bot.t) method_ params =
  try
    match method_ with
    | "health" -> handle_health bot
    | "list_projects" -> handle_list_projects bot
    | "list_sessions" -> handle_list_sessions bot
    | "import_project" -> handle_import_project bot params
    | "list_claude_sessions" -> handle_list_claude_sessions bot params
    | "list_codex_sessions" -> handle_list_codex_sessions bot params
    | "list_gemini_sessions" -> handle_list_gemini_sessions bot params
    | "start_session" -> handle_start_session bot params
    | "resume_session" -> handle_resume_session bot params
    | "stop_session" -> handle_stop_session bot params
    | "send_message" -> handle_send_message bot params
    | "default_agent" -> handle_default_agent bot params
    | "rescue_agent" -> handle_rescue_agent bot params
    | "get_agent_config" -> handle_get_agent_config bot params
    | "set_model" -> handle_set_model bot params
    | "set_effort" -> handle_set_effort bot params
    | "set_goal" -> handle_set_goal bot params
    | "start_login_flow" -> handle_start_login_flow bot params
    | "restart" -> handle_restart bot
    | "rename_thread" -> handle_rename_thread bot params
    | "cleanup_channels" -> handle_cleanup_channels bot
    | "refresh_projects" -> handle_refresh_projects bot
    | _ -> error_response (Printf.sprintf "Unknown method: %s" method_)
  with exn ->
    raise_if_cancelled exn;
    Logs.warn (fun m -> m "control_api: handler error: %s" (Printexc.to_string exn));
    error_response (Printexc.to_string exn)

(* ── Connection handler ────────────────────────────────────────── *)

let handle_connection bot flow =
  let started_at = Unix.gettimeofday () in
  let reader = Eio.Buf_read.of_flow ~max_size:max_request_size flow in
  match read_line_limited reader with
  | Error e ->
    send_response flow (error_response e)
  | Ok line ->
    let method_name = ref "<invalid>" in
    let response = match Yojson.Safe.from_string line with
      | exception _ -> error_response "invalid JSON"
      | json ->
        let open Yojson.Safe.Util in
        let method_ = json |> member "method" |> to_string_option in
        let params = match json |> member "params" with
          | `Null -> None | p -> Some p in
        (match method_ with
         | None -> error_response "missing 'method' field"
         | Some m ->
           method_name := m;
           dispatch bot m params)
    in
    send_response flow response;
    let elapsed = Unix.gettimeofday () -. started_at in
    if elapsed >= 0.5 then
      Logs.warn (fun m -> m "control_api: slow request method=%s elapsed=%.3fs"
        !method_name elapsed)

(* ── Server ────────────────────────────────────────────────────────── *)

let start ~(bot : Bot.t) ~sw ~(env : Eio_unix.Stdenv.base) =
  let path = socket_path () in
  Resource.ensure_parent_dir path;
  (* Remove stale socket from a previous run *)
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let addr = `Unix path in
  let socket = Eio.Net.listen ~sw ~backlog:64 ~reuse_addr:true net addr in
  Logs.info (fun m -> m "control_api: listening on %s" path);
  let min_accept_retry_delay = 0.1 in
  let max_accept_retry_delay = 5.0 in
  (* Accept loop — each connection handled in its own fiber *)
  let rec accept_loop retry_delay =
    match
      try Ok (Eio.Net.accept ~sw socket)
      with exn ->
        raise_if_cancelled exn;
        Error exn
    with
    | Ok (flow, _addr) ->
      Eio.Fiber.fork ~sw (fun () ->
        Fun.protect ~finally:(fun () -> Eio.Flow.close flow) (fun () ->
          handle_connection bot flow));
      accept_loop min_accept_retry_delay
    | Error exn ->
      Logs.warn (fun m -> m "control_api: accept failed: %s; retrying in %.1fs"
        (Printexc.to_string exn) retry_delay);
      Eio.Time.sleep clock retry_delay;
      accept_loop (min max_accept_retry_delay (retry_delay *. 2.0))
  in
  accept_loop min_accept_retry_delay
