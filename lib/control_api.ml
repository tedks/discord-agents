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
    `Assoc [
      ("project_name", `String s.project_name);
      ("agent_kind", `String (Config.string_of_agent_kind s.agent_kind));
      ("message_count", `Int s.message_count);
      ("thread_id", `String s.thread_id);
      ("session_id", `String s.session_id);
    ]
  ) entries in
  ok_response [("sessions", `List sessions)]

let handle_list_claude_sessions _bot params =
  let hours = match params with
    | Some (`Assoc l) ->
      (match List.assoc_opt "hours" l with
       | Some (`Int h) -> h | _ -> 24)
    | _ -> 24 in
  let sessions = Claude_sessions.discover ~hours () in
  let items = List.map (fun (s : Claude_sessions.info) ->
    let sid_short = String.sub s.session_id 0
      (min 8 (String.length s.session_id)) in
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

let handle_start_session (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
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
      let s = if String.length s > 4000
        then String.sub s 0 4000 else s in
      if s = "" then None else Some s
    | None -> None in
  match Command.find_project_fuzzy (Bot.projects bot) project_str with
  | None -> error_response (Printf.sprintf "No project matching '%s'." project_str)
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
          let session : Session_store.session = {
            project_name = p.name; working_dir; agent_kind = kind;
            session_id;
            session_id_confirmed = (kind <> Config.Codex);
            thread_id = thread_ch.Discord_types.id;
            system_prompt = None; message_count = 0; processing = false;
            pending_queue = Queue.create (); initial_prompt;
          } in
          Session_store.add bot.sessions ~thread_id:thread_ch.id session;
          let branch_str = match branch_info with
            | Some b -> Printf.sprintf "\nBranch: `%s`" b | None -> "" in
          ignore (Discord_rest.create_message bot.rest ~channel_id:thread_ch.id
            ~content:(Printf.sprintf
              "**%s** session started for **%s**%s\nWorking in: `%s`\nSend a message to interact."
              kind_str p.name branch_str working_dir) ());
          ok_response [
            ("thread_id", `String thread_ch.id);
            ("working_dir", `String working_dir);
            ("branch", match branch_info with
              | Some b -> `String b | None -> `Null);
            ("project_name", `String p.name);
            ("session_id", `String session_id);
          ]

let handle_resume_session (bot : Bot.t) params =
  let open Yojson.Safe.Util in
  let params = match params with Some p -> p | None ->
    failwith "missing params" in
  let sid_prefix = params |> member "session_id" |> to_string in
  let found = Eio_unix.run_in_systhread (fun () ->
    Claude_sessions.find_by_prefix sid_prefix) in
  match found with
  | None -> error_response (Printf.sprintf "No Claude session matching '%s'." sid_prefix)
  | Some (full_sid, raw_working_dir) ->
    let sid_short = String.sub full_sid 0 (min 8 (String.length full_sid)) in
    let matched_project = List.find_opt (fun (p : Project.t) ->
      raw_working_dir = p.path
      || (String.length raw_working_dir > String.length p.path + 1
          && String.sub raw_working_dir 0 (String.length p.path + 1)
             = p.path ^ "/")
    ) (Bot.projects bot) in
    let thread_parent = match matched_project with
      | Some p ->
        (match Channel_manager.find_or_create ~rest:bot.rest
                 ~guild_id:bot.config.guild_id ~project:p (Bot.channels bot) with
         | Some ch_id -> ch_id
         | None ->
           (match bot.config.control_channel_id with
            | Some ctl -> ctl | None -> ""))
      | None ->
        (match bot.config.control_channel_id with
         | Some ctl -> ctl | None -> "")
    in
    if thread_parent = "" then
      error_response "No channel found for thread creation."
    else
      let working_dir = match matched_project with
        | Some p when p.is_bare && raw_working_dir = p.path ->
          (match Bot.working_dir_of_project p with
           | Ok wd -> wd | Error _ -> raw_working_dir)
        | _ -> raw_working_dir
      in
      let project_name = match matched_project with
        | Some p -> p.name
        | None -> Filename.basename raw_working_dir
      in
      let thread_name = Printf.sprintf "resume / %s" sid_short in
      (match Discord_rest.create_thread_no_message bot.rest
              ~channel_id:thread_parent ~name:thread_name () with
      | Error e -> error_response (Printf.sprintf "Failed to create thread: %s" e)
      | Ok thread_ch ->
        let session : Session_store.session = {
          project_name; working_dir;
          agent_kind = Config.Claude; session_id = full_sid;
          session_id_confirmed = true;
          thread_id = thread_ch.Discord_types.id;
          system_prompt = None; message_count = 1; processing = false;
          pending_queue = Queue.create (); initial_prompt = None;
        } in
        Session_store.add bot.sessions ~thread_id:thread_ch.id session;
        ignore (Discord_rest.create_message bot.rest ~channel_id:thread_ch.id
          ~content:(Printf.sprintf
            "**Resumed** Claude session `%s`\nWorking in: `%s`\nSend a message to continue."
            sid_short working_dir) ());
        ok_response [
          ("thread_id", `String thread_ch.id);
          ("working_dir", `String working_dir);
          ("session_id", `String full_sid);
          ("project_name", `String project_name);
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
