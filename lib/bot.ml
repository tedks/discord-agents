(** Top-level bot orchestrator.

    Owns the Discord connection, manages sessions, routes messages
    between Discord and agent subprocesses.

    Discord channel layout:
    - Control channel: server-wide commands, project overview
    - Project channels: one per project, start agents here
    - Session threads: one per agent session, bridges I/O *)

module SessionMap = Map.Make(String) (* thread_id -> agent_session *)

(** Lightweight agent session — tracks a Discord thread <-> Claude session. *)
type agent_session = {
  project_name : string;
  working_dir : string;
  agent_kind : Config.agent_kind;
  session_id : string;
  thread_id : string;
  mutable message_count : int;
}

type t = {
  config : Config.t;
  rest : Discord_rest.t;
  gateway : Discord_gateway.t;
  projects : Project.t list;
  mutable sessions : agent_session SessionMap.t;
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
}

(** Commands the bot recognizes in the control channel. *)
type command =
  | List_projects
  | List_sessions
  | List_claude_sessions
  | Start_agent of { project : string; kind : Config.agent_kind }
  | Resume_session of { session_id : string }
  | Stop_session of { thread_id : string }
  | Restart
  | Help
  | Unknown of string

let is_command content =
  let trimmed = String.trim content in
  String.length trimmed > 0 && trimmed.[0] = '!'

let parse_command content =
  let parts = String.split_on_char ' ' (String.trim content) in
  match parts with
  | ["!projects"] | ["!list"] -> List_projects
  | ["!sessions"] -> List_sessions
  | ["!claude-sessions"] -> List_claude_sessions
  | ["!start"; project; kind_str] ->
    (match Config.agent_kind_of_string kind_str with
     | Ok kind -> Start_agent { project; kind }
     | Error _ -> Unknown content)
  | ["!resume"; session_id] -> Resume_session { session_id }
  | ["!stop"; thread_id] -> Stop_session { thread_id }
  | ["!restart"] -> Restart
  | ["!help"] -> Help
  | _ -> Unknown content

(** Generate a UUID for Claude session tracking. *)
let generate_uuid () =
  let buf = Bytes.create 16 in
  let ic = open_in "/dev/urandom" in
  really_input ic buf 0 16;
  close_in ic;
  let hex = Buffer.create 32 in
  Bytes.iter (fun c ->
    Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c))
  ) buf;
  let s = Buffer.contents hex in
  Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub s 0 8) (String.sub s 8 4) (String.sub s 12 4)
    (String.sub s 16 4) (String.sub s 20 12)

(** Find a usable working directory for a project.
    For bare repos, look for master/ or main/ worktree. *)
let working_dir_of_project (p : Project.t) =
  if p.is_bare then
    let candidates = ["master"; "main"] in
    match List.find_opt (fun name ->
      let path = Filename.concat p.path name in
      try Sys.is_directory path with Sys_error _ -> false
    ) candidates with
    | Some name -> Ok (Filename.concat p.path name)
    | None -> Error "bare repo has no master/ or main/ worktree"
  else
    Ok p.path

(** Typing indicator refresh interval in seconds.
    Discord's typing indicator expires after ~10s, so refresh every 8s. *)
let typing_interval = 8.0

(** Split text into chunks that fit Discord's 2000-char message limit. *)
let split_message ?(max_len=1900) text =
  let len = String.length text in
  if len <= max_len then [text]
  else
    let rec split pos acc =
      if pos >= len then List.rev acc
      else
        let chunk_end = min (pos + max_len) len in
        split chunk_end (String.sub text pos (chunk_end - pos) :: acc)
    in
    split 0 []

(** Post a (potentially long) response to a Discord channel, splitting if needed. *)
let post_response rest ~channel_id text =
  let chunks = split_message text in
  List.iter (fun chunk ->
    match Discord_rest.create_message rest ~channel_id ~content:chunk () with
    | Ok _ -> ()
    | Error e -> Logs.warn (fun m -> m "bot: failed to post response: %s" e)
  ) chunks

(** Resolve a Claude project directory name back to a filesystem path.
    e.g. "-home-tedks-Projects-claude-discord" -> "/home/tedks/Projects/claude-discord"

    Can't just split on '-' because project names contain hyphens.
    Instead, walk the filesystem: at each level, try consuming path
    segments greedily (longest directory name that matches). *)
let resolve_project_dir proj_name =
  (* Strip leading dash *)
  let s = if String.length proj_name > 0 && proj_name.[0] = '-'
          then String.sub proj_name 1 (String.length proj_name - 1)
          else proj_name in
  let rec resolve path remaining =
    if String.length remaining = 0 then path
    else
      (* Try to find the longest prefix of 'remaining' that is a directory under 'path' *)
      let parts = String.split_on_char '-' remaining in
      let rec try_lengths n =
        if n < 1 then
          (* Fallback: just use the first part *)
          let first = List.hd parts in
          let rest_parts = List.tl parts in
          let next_path = Filename.concat path first in
          let rest = String.concat "-" rest_parts in
          resolve next_path rest
        else
          let candidate_parts = List.filteri (fun i _ -> i < n) parts in
          let candidate = String.concat "-" candidate_parts in
          let candidate_path = Filename.concat path candidate in
          if (try Sys.file_exists candidate_path with _ -> false) then
            let rest_parts = List.filteri (fun i _ -> i >= n) parts in
            let rest = String.concat "-" rest_parts in
            resolve candidate_path rest
          else
            try_lengths (n - 1)
      in
      try_lengths (List.length parts)
  in
  resolve "/" s

(** Info about a Claude Code session discovered on disk. *)
type claude_session_info = {
  cs_session_id : string;
  cs_project_dir : string;   (** e.g. "-home-tedks-Projects-claude-discord" *)
  cs_working_dir : string;   (** resolved working directory *)
  cs_summary : string;       (** first user message, truncated *)
  cs_mtime : float;
}

(** Scan ~/.claude/projects/ for recent Claude Code sessions.
    Returns sessions modified in the last [hours] hours, newest first. *)
let discover_claude_sessions ?(hours=24) () =
  Eio_unix.run_in_systhread @@ fun () ->
  let home = Sys.getenv "HOME" in
  let projects_dir = Filename.concat home ".claude/projects" in
  if not (Sys.file_exists projects_dir) then []
  else
    let cutoff = Unix.gettimeofday () -. (float_of_int hours *. 3600.0) in
    let results = ref [] in
    let project_dirs = Sys.readdir projects_dir |> Array.to_list in
    List.iter (fun proj_name ->
      let proj_path = Filename.concat projects_dir proj_name in
      if try Sys.is_directory proj_path with Sys_error _ -> false then begin
        let files = try Sys.readdir proj_path |> Array.to_list with Sys_error _ -> [] in
        List.iter (fun fname ->
          if Filename.check_suffix fname ".jsonl"
             && not (String.contains fname '/') then begin
            let fpath = Filename.concat proj_path fname in
            let stat = try Some (Unix.stat fpath) with Unix.Unix_error _ -> None in
            match stat with
            | Some st when st.Unix.st_mtime > cutoff ->
              let session_id = Filename.chop_suffix fname ".jsonl" in
              (* Extract first user message as summary *)
              let summary =
                try
                  let ic = open_in fpath in
                  let summary = ref "" in
                  (try while !summary = "" do
                    let line = input_line ic in
                    let json = Yojson.Safe.from_string line in
                    let open Yojson.Safe.Util in
                    if json |> member "type" |> to_string = "user" then begin
                      let msg = json |> member "message" in
                      let content = msg |> member "content" in
                      match content with
                      | `List items ->
                        List.iter (fun item ->
                          if !summary = "" then
                            match item |> member "type" |> to_string_option with
                            | Some "text" ->
                              let text = item |> member "text" |> to_string in
                              summary := String.sub text 0 (min 80 (String.length text))
                            | _ -> ()
                        ) items
                      | `String s ->
                        summary := String.sub s 0 (min 80 (String.length s))
                      | _ -> ()
                    end
                  done with End_of_file | _ -> ());
                  close_in ic;
                  !summary
                with _ -> "(unknown)"
              in
              (* Resolve working directory from project dir name.
                 The dir name is the cwd with / replaced by - and leading -.
                 We can't just split on - because project names have hyphens.
                 Instead, scan the filesystem to find the longest matching prefix. *)
              let working_dir = resolve_project_dir proj_name in
              results := {
                cs_session_id = session_id;
                cs_project_dir = proj_name;
                cs_working_dir = working_dir;
                cs_summary = summary;
                cs_mtime = st.Unix.st_mtime;
              } :: !results
            | _ -> ()
          end
        ) files
      end
    ) project_dirs;
    (* Sort newest first *)
    List.sort (fun a b -> compare b.cs_mtime a.cs_mtime) !results

(** Find a Claude session by ID (or prefix) and return its info. *)
let find_claude_session session_id_prefix =
  let home = Sys.getenv "HOME" in
  let projects_dir = Filename.concat home ".claude/projects" in
  if not (Sys.file_exists projects_dir) then None
  else
    let project_dirs = try Sys.readdir projects_dir |> Array.to_list with _ -> [] in
    let result = ref None in
    List.iter (fun proj_name ->
      if !result = None then begin
        let proj_path = Filename.concat projects_dir proj_name in
        if try Sys.is_directory proj_path with Sys_error _ -> false then begin
          let files = try Sys.readdir proj_path |> Array.to_list with _ -> [] in
          List.iter (fun fname ->
            if !result = None
               && Filename.check_suffix fname ".jsonl"
               && not (String.contains fname '/') then begin
              let sid = Filename.chop_suffix fname ".jsonl" in
              if String.length sid >= String.length session_id_prefix
                 && String.sub sid 0 (String.length session_id_prefix) = session_id_prefix then begin
                let working_dir = resolve_project_dir proj_name in
                result := Some (sid, working_dir)
              end
            end
          ) files
        end
      end
    ) project_dirs;
    !result

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
    let lines = List.map (fun (_tid, (s : agent_session)) ->
      Printf.sprintf "- **%s** / %s — %d messages (thread: <#%s>)"
        s.project_name
        (Config.string_of_agent_kind s.agent_kind)
        s.message_count
        s.thread_id
    ) entries in
    let text = if lines = [] then "No active sessions."
      else "**Sessions:**\n" ^ String.concat "\n" lines in
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:text ())
  | List_claude_sessions ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let sessions = discover_claude_sessions ~hours:24 () in
      let lines = List.map (fun (s : claude_session_info) ->
        let age_min = int_of_float ((Unix.gettimeofday () -. s.cs_mtime) /. 60.0) in
        let age_str =
          if age_min < 60 then Printf.sprintf "%dm ago" age_min
          else Printf.sprintf "%dh ago" (age_min / 60)
        in
        Printf.sprintf "- `%s` %s\n  %s — *%s*"
          (String.sub s.cs_session_id 0 (min 8 (String.length s.cs_session_id)))
          age_str
          s.cs_working_dir
          (if s.cs_summary = "" then "(no summary)" else s.cs_summary)
      ) (List.filteri (fun i _ -> i < 10) sessions) in
      let text = if lines = [] then "No recent Claude sessions found."
        else "**Recent Claude sessions** (last 24h):\n" ^ String.concat "\n" lines
             ^ "\n\nUse `!resume <session_id_prefix>` to attach." in
      ignore (Discord_rest.create_message t.rest
        ~channel_id:msg.channel_id ~content:text ()))
  | Start_agent { project; kind } ->
    let proj = List.find_opt (fun (p : Project.t) -> p.name = project) t.projects in
    (match proj with
     | None ->
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id
         ~content:(Printf.sprintf "Project `%s` not found." project) ())
     | Some p ->
       match working_dir_of_project p with
       | Error e ->
         ignore (Discord_rest.create_message t.rest
           ~channel_id:msg.channel_id
           ~content:(Printf.sprintf "Cannot find working directory: %s" e) ())
       | Ok working_dir ->
         let kind_str = Config.string_of_agent_kind kind in
         let thread_name = Printf.sprintf "%s / %s" kind_str p.name in
         (* Create a thread in the control channel *)
         match Discord_rest.create_thread_no_message t.rest
                 ~channel_id:msg.channel_id ~name:thread_name () with
         | Error e ->
           ignore (Discord_rest.create_message t.rest
             ~channel_id:msg.channel_id
             ~content:(Printf.sprintf "Failed to create thread: %s" e) ())
         | Ok thread_ch ->
           let session_id = generate_uuid () in
           let session = {
             project_name = p.name;
             working_dir;
             agent_kind = kind;
             session_id;
             thread_id = thread_ch.Discord_types.id;
             message_count = 0;
           } in
           t.sessions <- SessionMap.add thread_ch.id session t.sessions;
           (* Post welcome message in the thread *)
           let welcome = Printf.sprintf
             "**%s** session started for **%s**\nWorking in: `%s`\nSend a message to interact with the agent."
             kind_str p.name working_dir
           in
           ignore (Discord_rest.create_message t.rest
             ~channel_id:thread_ch.id ~content:welcome ()))
  | Resume_session { session_id } ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      (* find_claude_session runs blocking I/O *)
      let found = Eio_unix.run_in_systhread (fun () ->
        find_claude_session session_id
      ) in
      match found with
      | None ->
        ignore (Discord_rest.create_message t.rest
          ~channel_id:msg.channel_id
          ~content:(Printf.sprintf "No Claude session found matching `%s`." session_id) ())
      | Some (full_session_id, working_dir) ->
        let thread_name = Printf.sprintf "resume / %s" (String.sub full_session_id 0 8) in
        match Discord_rest.create_thread_no_message t.rest
                ~channel_id:msg.channel_id ~name:thread_name () with
        | Error e ->
          ignore (Discord_rest.create_message t.rest
            ~channel_id:msg.channel_id
            ~content:(Printf.sprintf "Failed to create thread: %s" e) ())
        | Ok thread_ch ->
          let session = {
            project_name = Filename.basename working_dir;
            working_dir;
            agent_kind = Config.Claude;
            session_id = full_session_id;
            thread_id = thread_ch.Discord_types.id;
            message_count = 1; (* >0 so we use --resume *)
          } in
          t.sessions <- SessionMap.add thread_ch.id session t.sessions;
          let welcome = Printf.sprintf
            "**Resumed** Claude session `%s`\nWorking in: `%s`\nSend a message to continue."
            (String.sub full_session_id 0 8) working_dir
          in
          ignore (Discord_rest.create_message t.rest
            ~channel_id:thread_ch.id ~content:welcome ()))
  | Stop_session { thread_id } ->
    (match SessionMap.find_opt thread_id t.sessions with
     | None ->
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id
         ~content:"Session not found." ())
     | Some session ->
       t.sessions <- SessionMap.remove thread_id t.sessions;
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id
         ~content:(Printf.sprintf "Stopped session for **%s**." session.project_name) ()))
  | Restart ->
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:"Rebuilding and restarting..." ());
    (* Build first, only restart if build succeeds *)
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let build_result = Eio_unix.run_in_systhread (fun () ->
        let exit_code = Sys.command
          "cd /home/tedks/Projects/claude-discord/master && nix develop --command dune build 2>&1" in
        exit_code
      ) in
      if build_result <> 0 then
        ignore (Discord_rest.create_message t.rest
          ~channel_id:msg.channel_id ~content:"Build failed, not restarting." ())
      else begin
        ignore (Discord_rest.create_message t.rest
          ~channel_id:msg.channel_id ~content:"Build succeeded, restarting now." ());
        (* Give Discord a moment to deliver the message *)
        Unix.sleepf 1.0;
        (* Re-exec ourselves *)
        let exe = "/home/tedks/Projects/claude-discord/master/_build/default/bin/main.exe" in
        Unix.execv exe [| exe |]
      end)
  | Help ->
    let text = String.concat "\n" [
      "**Commands:**";
      "`!projects` — list discovered projects";
      "`!sessions` — list active bot sessions";
      "`!claude-sessions` — list recent Claude Code sessions on this machine";
      "`!start <project> <claude|codex|gemini>` — start a new agent session";
      "`!resume <session_id>` — resume an existing Claude session in a new thread";
      "`!stop <thread_id>` — stop a session";
      "`!restart` — rebuild and restart the bot";
      "`!help` — this message";
    ] in
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:text ())
  | Unknown _ -> ()

(** Handle a message in a session thread — run the agent and stream the response. *)
let handle_thread_message t msg =
  match SessionMap.find_opt msg.Discord_types.channel_id t.sessions with
  | None -> ()
  | Some session ->
    (* Fork a fiber so we don't block the gateway recv loop *)
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let channel_id = msg.Discord_types.channel_id in
      (* Send typing indicator *)
      ignore (Discord_rest.send_typing t.rest ~channel_id ());
      let prompt = msg.content in
      Logs.info (fun m -> m "bot: running %s for %s: %s"
        (Config.string_of_agent_kind session.agent_kind)
        session.project_name
        (if String.length prompt > 80
         then String.sub prompt 0 80 ^ "..."
         else prompt));
      (* Accumulate text and post/edit a Discord message as chunks arrive *)
      let result_buf = Buffer.create 4096 in
      let current_msg_id = ref None in
      let current_msg_buf = Buffer.create 1900 in
      let last_typing = ref (Unix.gettimeofday ()) in
      let last_edit = ref (Unix.gettimeofday ()) in
      (* Flush current buffer to Discord — create or edit message *)
      let flush_to_discord () =
        let text = Buffer.contents current_msg_buf in
        if String.length text = 0 then ()
        else match !current_msg_id with
        | None ->
          (match Discord_rest.create_message t.rest ~channel_id ~content:text () with
           | Ok sent -> current_msg_id := Some sent.Discord_types.id
           | Error e -> Logs.warn (fun m -> m "bot: send error: %s" e))
        | Some mid ->
          (match Discord_rest.edit_message t.rest ~channel_id ~message_id:mid ~content:text () with
           | Ok _ -> ()
           | Error e -> Logs.warn (fun m -> m "bot: edit error: %s" e))
      in
      (* Start a new message (when current one is getting long) *)
      let start_new_message () =
        flush_to_discord ();
        Buffer.clear current_msg_buf;
        current_msg_id := None
      in
      let on_event = function
        | Agent_process.Text_delta text ->
          Buffer.add_string result_buf text;
          Buffer.add_string current_msg_buf text;
          (* If current message is getting long, start a new one *)
          if Buffer.length current_msg_buf > 1800 then
            start_new_message ()
          else begin
            (* Edit every 2s to avoid rate limits *)
            let now = Unix.gettimeofday () in
            if now -. !last_edit > 2.0 then begin
              flush_to_discord ();
              last_edit := now
            end;
            (* Refresh typing indicator *)
            if now -. !last_typing > typing_interval then begin
              ignore (Discord_rest.send_typing t.rest ~channel_id ());
              last_typing := now
            end
          end
        | Agent_process.Result { text = _; session_id = _ } ->
          (* Final flush *)
          flush_to_discord ()
        | Agent_process.Tool_use name ->
          Logs.debug (fun m -> m "bot: agent using tool: %s" name)
        | Agent_process.Error e ->
          Logs.warn (fun m -> m "bot: agent error event: %s" e)
        | Agent_process.Other _ -> ()
      in
      (match Agent_process.run_streaming ~sw:t.sw ~env:t.env
               ~working_dir:session.working_dir
               ~kind:session.agent_kind
               ~session_id:session.session_id
               ~message_count:session.message_count
               ~prompt ~on_event with
       | Ok () ->
         session.message_count <- session.message_count + 1;
         (* Final flush if not already done *)
         flush_to_discord ();
         if Buffer.length result_buf = 0 then
           ignore (Discord_rest.create_message t.rest
             ~channel_id ~content:"(no response)" ())
       | Error e ->
         Logs.warn (fun m -> m "bot: agent error: %s" e);
         ignore (Discord_rest.create_message t.rest
           ~channel_id
           ~content:(Printf.sprintf "Agent error: %s" e) ())))

(** Route an incoming Discord message. *)
let handle_message t (msg : Discord_types.message) =
  (* Ignore bot messages *)
  match msg.author.bot with Some true -> () | _ ->
  (* Commands (messages starting with !) are handled regardless of channel *)
  if is_command msg.content then
    handle_control_message t msg
  else
    handle_thread_message t msg

let create ~sw ~(env : Eio_unix.Stdenv.base) config =
  let rest = Discord_rest.create ~sw ~env ~token:config.Config.discord_token in
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
    env;
    sw;
  } in
  bot.gateway.handler <- (fun event ->
    match event with
    | Discord_gateway.Connected user ->
      Logs.info (fun m -> m "bot: connected as %s" user.Discord_types.username)
    | Discord_gateway.Message_received msg -> handle_message bot msg
    | Discord_gateway.Thread_created ch ->
      Logs.info (fun m -> m "bot: thread created: %s"
        (Option.value ~default:"(unnamed)" ch.Discord_types.name))
    | Discord_gateway.Disconnected reason ->
      Logs.warn (fun m -> m "bot: disconnected: %s" reason)
  );
  bot

let run ~sw:_ ~(env : Eio_unix.Stdenv.base) bot =
  Logs.info (fun m -> m "bot: discovered %d projects" (List.length bot.projects));
  List.iter (fun (p : Project.t) ->
    Logs.info (fun m -> m "  - %s (%s)" p.name p.path)
  ) bot.projects;
  Discord_gateway.connect ~sw:bot.sw ~env bot.gateway
