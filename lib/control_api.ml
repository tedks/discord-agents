(** Unix domain socket control API — JSON-RPC-style server.

    Runs as a daemon fiber inside the bot's main switch. Accepts
    connections on a Unix socket, reads one JSON request per connection,
    dispatches to bot operations, and writes one JSON response.

    Protocol: line-delimited JSON over Unix domain socket.
    Request:  {"method": "...", "params": {...}}
    Response: {"ok": true, ...} or {"error": "..."}

    Replaces the MCP server's direct session file / Discord REST access. *)

let socket_path () =
  let home = Sys.getenv "HOME" in
  Filename.concat home ".config/discord-agents/control.sock"

(** Read one line from a buffered reader, up to a size limit. *)
let read_line_limited reader =
  try
    let line = Eio.Buf_read.line reader in
    if String.length line > 1_000_000 then
      Error "request too large"
    else
      Ok line
  with
  | End_of_file -> Error "empty request"
  | exn -> Error (Printexc.to_string exn)

(** Send a JSON response and close. *)
let send_response flow json =
  let data = Yojson.Safe.to_string json ^ "\n" in
  try Eio.Flow.copy_string data flow
  with _ -> ()  (* client may have disconnected *)

let ok_response body =
  `Assoc (("ok", `Bool true) :: body)

let error_response msg =
  `Assoc [("error", `String msg)]

(* ── Handlers ──────────────────────────────────────────────────── *)

let handle_health (bot : Bot.t) =
  let uptime = int_of_float (Unix.gettimeofday () -. bot.started_at) in
  ok_response [
    ("uptime_seconds", `Int uptime);
    ("sessions", `Int (Session_store.count bot.sessions));
    ("projects", `Int (List.length (Bot.projects bot)));
    ("channels", `Int (Channel_manager.count (Bot.channels bot)));
  ]

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
  let project_str = params |> member "project" |> to_string in
  let kind_str = match params |> member "agent" |> to_string_option with
    | Some s -> s | None -> "claude" in
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
    if working_dir = "" then
      error_response "No working directory available."
    else
      let thread_display_name = match thread_name with
        | Some n when String.length (String.trim n) > 0 ->
          let n = String.trim n in
          if String.length n > 80 then String.sub n 0 80 else n
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
      if thread_parent = "" then
        error_response "No channel found for thread creation."
      else
        match Discord_rest.create_thread_no_message bot.rest
                ~channel_id:thread_parent ~name:thread_display_name () with
        | Error e -> error_response (Printf.sprintf "Failed to create thread: %s" e)
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
          (match initial_prompt with
           | None ->
             Session_store.add bot.sessions ~thread_id:thread_ch.id session;
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
           | Some prompt ->
             session.processing <- true;
             Session_store.add bot.sessions ~thread_id:thread_ch.id session;
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
               Session_store.remove bot.sessions
                 ~thread_id:thread_ch.id;
               (match Discord_rest.delete_channel bot.rest
                       ~channel_id:thread_ch.id () with
                | Ok _ -> ()
                | Error de ->
                  Logs.warn (fun m -> m
                    "control_api: failed to clean up orphan thread \
                     %s after prompt-post failure: %s"
                    thread_ch.id de));
               error_response (Printf.sprintf
                 "Failed to post initial_prompt: %s" e)
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
               ])

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
  let sid_prefix = params |> member "session_id" |> to_string in
  let kind = match params |> member "kind" |> to_string_option with
    | None -> None
    | Some s ->
      (match Config.agent_kind_of_string (String.lowercase_ascii s) with
       | Ok k -> Some k | Error _ -> None)
  in
  (* Mirror Bot.handle_command's Resume_session dispatch: explicit
     kind looks up its own store; None tries Claude then Gemini. *)
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
  let found = match kind with
    | Some Config.Claude -> try_claude ()
    | Some Config.Codex -> try_codex ()
    | Some Config.Gemini -> try_gemini ()
    | None ->
      (match try_claude () with
       | Some _ as r -> r
       | None ->
         match try_codex () with
         | Some _ as r -> r
         | None -> try_gemini ())
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
        Session_store.add bot.sessions ~thread_id:thread_ch.id session;
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
        ])

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
    | "list_claude_sessions" -> handle_list_claude_sessions bot params
    | "list_codex_sessions" -> handle_list_codex_sessions bot params
    | "list_gemini_sessions" -> handle_list_gemini_sessions bot params
    | "start_session" -> handle_start_session bot params
    | "resume_session" -> handle_resume_session bot params
    | "restart" -> handle_restart bot
    | "rename_thread" -> handle_rename_thread bot params
    | "cleanup_channels" -> handle_cleanup_channels bot
    | "refresh_projects" -> handle_refresh_projects bot
    | _ -> error_response (Printf.sprintf "Unknown method: %s" method_)
  with exn ->
    Logs.warn (fun m -> m "control_api: handler error: %s" (Printexc.to_string exn));
    error_response (Printexc.to_string exn)

(* ── Connection handler ────────────────────────────────────────── *)

let handle_connection bot flow =
  let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
  match read_line_limited reader with
  | Error e ->
    send_response flow (error_response e)
  | Ok line ->
    let response = match Yojson.Safe.from_string line with
      | exception _ -> error_response "invalid JSON"
      | json ->
        let open Yojson.Safe.Util in
        let method_ = json |> member "method" |> to_string_option in
        let params = match json |> member "params" with
          | `Null -> None | p -> Some p in
        (match method_ with
         | None -> error_response "missing 'method' field"
         | Some m -> dispatch bot m params)
    in
    send_response flow response

(* ── Server ────────────────────────────────────────────────────── *)

let start ~(bot : Bot.t) ~sw ~(env : Eio_unix.Stdenv.base) =
  let path = socket_path () in
  (* Remove stale socket from a previous run *)
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  let net = Eio.Stdenv.net env in
  let addr = `Unix path in
  let socket = Eio.Net.listen ~sw ~backlog:5 ~reuse_addr:true net addr in
  Logs.info (fun m -> m "control_api: listening on %s" path);
  (* Accept loop — each connection handled in its own fiber *)
  let rec accept_loop () =
    let flow, _addr = Eio.Net.accept ~sw socket in
    Eio.Fiber.fork ~sw (fun () ->
      Fun.protect ~finally:(fun () -> Eio.Flow.close flow) (fun () ->
        handle_connection bot flow));
    accept_loop ()
  in
  accept_loop ()
