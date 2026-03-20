(** Top-level bot orchestrator.

    Owns the Discord connection, manages sessions, routes messages
    between Discord and agent subprocesses.

    Discord channel layout:
    - Control channel: server-wide commands, project overview
    - Project channels: one per project, start agents here
    - Session threads: one per agent session, bridges I/O *)

module SessionMap = Map.Make(String) (* thread_id -> session *)

type t = {
  config : Config.t;
  rest : Discord_rest.t;
  gateway : Discord_gateway.t;
  projects : Project.t list;
  mutable sessions : Session.t SessionMap.t;
}

(** Commands the bot recognizes in the control channel. *)
type command =
  | List_projects
  | List_sessions
  | Start_agent of { project : string; kind : Config.agent_kind }
  | Stop_session of { thread_id : string }
  | Help
  | Unknown of string

let parse_command content =
  let parts = String.split_on_char ' ' (String.trim content) in
  match parts with
  | ["!projects"] | ["!list"] -> List_projects
  | ["!sessions"] -> List_sessions
  | ["!start"; project; kind_str] ->
    (match Config.agent_kind_of_string kind_str with
     | Ok kind -> Start_agent { project; kind }
     | Error _ -> Unknown content)
  | ["!stop"; thread_id] -> Stop_session { thread_id }
  | ["!help"] -> Help
  | _ -> Unknown content

(** Handle a message from the control channel. *)
let handle_control_message t msg =
  let cmd = parse_command msg.Discord_types.content in
  match cmd with
  | List_projects ->
    let lines = List.map (fun (p : Project.t) ->
      Printf.sprintf "- **%s** (`%s`) %s"
        p.name p.path (if p.is_bare then "[bare]" else "")
    ) t.projects in
    let text = if lines = [] then "No projects found."
      else "**Projects:**\n" ^ String.concat "\n" lines in
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:text ())
  | List_sessions ->
    let entries = SessionMap.bindings t.sessions in
    let lines = List.map (fun (_tid, (s : Session.t)) ->
      Printf.sprintf "- **%s** / %s (%s) — %s"
        s.project_name
        (Config.string_of_agent_kind s.agent_kind)
        s.id
        (Session.show_state s.state)
    ) entries in
    let text = if lines = [] then "No active sessions."
      else "**Sessions:**\n" ^ String.concat "\n" lines in
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:text ())
  | Start_agent { project; kind } ->
    (* Find project, create worktree, create thread, start session *)
    let proj = List.find_opt (fun (p : Project.t) -> p.name = project) t.projects in
    (match proj with
     | None ->
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id
         ~content:(Printf.sprintf "Project `%s` not found." project) ())
     | Some p ->
       let branch = Printf.sprintf "agent/%s-%s"
         (Config.string_of_agent_kind kind)
         (Session.generate_id ())
       in
       match Project.create_worktree p ~branch_name:branch with
       | Error e ->
         ignore (Discord_rest.create_message t.rest
           ~channel_id:msg.channel_id
           ~content:(Printf.sprintf "Failed to create worktree: %s" e) ())
       | Ok worktree_path ->
         (* TODO: create Discord thread, then start session *)
         let thread_id = "TODO" in
         let session = Session.create
           ~project_name:p.name ~agent_kind:kind
           ~thread_id ~worktree_path
         in
         (match Session.start session with
          | Ok () ->
            t.sessions <- SessionMap.add thread_id session t.sessions;
            ignore (Discord_rest.create_message t.rest
              ~channel_id:msg.channel_id
              ~content:(Printf.sprintf "Started %s session for **%s** (thread: TODO)"
                (Config.string_of_agent_kind kind) p.name) ())
          | Error e ->
            ignore (Discord_rest.create_message t.rest
              ~channel_id:msg.channel_id
              ~content:(Printf.sprintf "Failed to start session: %s" e) ())))
  | Stop_session { thread_id } ->
    (match SessionMap.find_opt thread_id t.sessions with
     | None ->
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id
         ~content:"Session not found." ())
     | Some session ->
       Session.stop session;
       t.sessions <- SessionMap.remove thread_id t.sessions;
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id
         ~content:(Printf.sprintf "Stopped session %s." session.id) ()))
  | Help ->
    let text = String.concat "\n" [
      "**Commands:**";
      "`!projects` — list discovered projects";
      "`!sessions` — list active agent sessions";
      "`!start <project> <claude|codex|gemini>` — start an agent session";
      "`!stop <thread_id>` — stop a session";
      "`!help` — this message";
    ] in
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:text ())
  | Unknown _ -> ()

(** Handle a message in a session thread — forward to agent. *)
let handle_thread_message t msg =
  match SessionMap.find_opt msg.Discord_types.channel_id t.sessions with
  | None -> () (* Not a session thread, ignore *)
  | Some session ->
    (match Session.send_to_agent session msg.content with
     | Ok () -> ()
     | Error e ->
       Logs.warn (fun m -> m "failed to send to agent: %s" e))

(** Route an incoming Discord message. *)
let handle_message t (msg : Discord_types.message) =
  (* Ignore bot messages *)
  (match msg.author.bot with Some true -> () | _ ->
    match t.config.control_channel_id with
    | Some ctl_id when msg.channel_id = ctl_id ->
      handle_control_message t msg
    | _ ->
      handle_thread_message t msg)

let create config =
  let rest = Discord_rest.create ~token:config.Config.discord_token in
  let projects = Project.discover ~base_directories:config.base_directories in
  let gateway = Discord_gateway.create
    ~token:config.discord_token
    ~intents:Discord_gateway.default_intents
    ~handler:(fun _event -> ())
  in
  let bot = {
    config;
    rest;
    gateway;
    projects;
    sessions = SessionMap.empty;
  } in
  (* Wire up gateway handler *)
  bot.gateway.handler <- (fun event ->
    match event with
    | Discord_gateway.Message_received msg -> handle_message bot msg
    | _ -> ()
  );
  bot

let run bot =
  Logs.info (fun m -> m "bot: discovered %d projects" (List.length bot.projects));
  List.iter (fun (p : Project.t) ->
    Logs.info (fun m -> m "  - %s (%s)" p.name p.path)
  ) bot.projects;
  Discord_gateway.connect bot.gateway
