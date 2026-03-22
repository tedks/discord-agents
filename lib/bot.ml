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
  system_prompt : string option;
  mutable message_count : int;
}

let sessions_file () =
  let home = Sys.getenv "HOME" in
  Filename.concat home ".config/discord-agents/sessions.json"

let save_sessions sessions =
  let entries = SessionMap.bindings sessions in
  let json = `List (List.map (fun (_tid, s) ->
    `Assoc ([
      ("project_name", `String s.project_name);
      ("working_dir", `String s.working_dir);
      ("agent_kind", `String (Config.string_of_agent_kind s.agent_kind));
      ("session_id", `String s.session_id);
      ("thread_id", `String s.thread_id);
      ("message_count", `Int s.message_count);
    ] @ (match s.system_prompt with
         | Some sp -> [("system_prompt", `String sp)]
         | None -> []))
  ) entries) in
  let path = sessions_file () in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  output_char oc '\n';
  close_out oc

let load_sessions () =
  let path = sessions_file () in
  if not (Sys.file_exists path) then SessionMap.empty
  else
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      let json = Yojson.Safe.from_string (Bytes.to_string s) in
      let open Yojson.Safe.Util in
      let entries = to_list json |> List.map (fun j ->
        let thread_id = j |> member "thread_id" |> to_string in
        let session = {
          project_name = j |> member "project_name" |> to_string;
          working_dir = j |> member "working_dir" |> to_string;
          agent_kind = (match Config.agent_kind_of_string
            (j |> member "agent_kind" |> to_string) with
            | Ok k -> k | Error _ -> Config.Claude);
          session_id = j |> member "session_id" |> to_string;
          thread_id;
          system_prompt = j |> member "system_prompt" |> to_string_option;
          message_count = j |> member "message_count" |> to_int;
        } in
        (thread_id, session)
      ) in
      List.fold_left (fun acc (tid, s) -> SessionMap.add tid s acc) SessionMap.empty entries
    with exn ->
      Logs.warn (fun m -> m "bot: failed to load sessions: %s" (Printexc.to_string exn));
      SessionMap.empty

module ChannelMap = Map.Make(String) (* project_name -> channel_id *)

type t = {
  config : Config.t;
  rest : Discord_rest.t;
  gateway : Discord_gateway.t;
  projects : Project.t list;
  mutable sessions : agent_session SessionMap.t;
  mutable project_channels : string ChannelMap.t;
  mutable category_id : string option;
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
}

let add_session (t : t) thread_id session =
  t.sessions <- SessionMap.add thread_id session t.sessions;
  save_sessions t.sessions

let remove_session (t : t) thread_id =
  t.sessions <- SessionMap.remove thread_id t.sessions;
  save_sessions t.sessions

(** Commands the bot recognizes. *)
type command =
  | List_projects
  | List_sessions
  | List_claude_sessions
  | Start_agent of { project : string; kind : Config.agent_kind }
  | Resume_session of { session_id : string }
  | Stop_session of { thread_id : string }
  | Cleanup_channels
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
  (* !start project agent  OR  !start project (defaults to claude) *)
  | "!start" :: project :: kind_str :: _ ->
    let kind = match Config.agent_kind_of_string kind_str with
      | Ok k -> k | Error _ -> Config.Claude in
    Start_agent { project; kind }
  | ["!start"; project] ->
    Start_agent { project; kind = Config.Claude }
  | ["!start"] ->
    List_projects (* no args = show what's available *)
  | ["!resume"; session_id] -> Resume_session { session_id }
  | ["!stop"; thread_id] -> Stop_session { thread_id }
  | ["!cleanup-channels"] | ["!cleanup"] -> Cleanup_channels
  | ["!restart"] -> Restart
  | ["!help"] -> Help
  | _ -> Unknown content

(** Fuzzy-match a query against project names.
    Tries: exact match, case-insensitive match, prefix match, substring match.
    Returns the best matching project or None. *)
let find_project_fuzzy projects query =
  let q = String.lowercase_ascii query in
  (* Exact match *)
  match List.find_opt (fun (p : Project.t) -> p.name = query) projects with
  | Some _ as found -> found
  | None ->
  (* Case-insensitive exact *)
  match List.find_opt (fun (p : Project.t) ->
    String.lowercase_ascii p.name = q) projects with
  | Some _ as found -> found
  | None ->
  (* Numeric index (1-based) *)
  (match int_of_string_opt query with
   | Some n when n >= 1 && n <= List.length projects ->
     Some (List.nth projects (n - 1))
   | _ ->
  (* Prefix match *)
  let prefix_matches = List.filter (fun (p : Project.t) ->
    let name = String.lowercase_ascii p.name in
    String.length name >= String.length q
    && String.sub name 0 (String.length q) = q
  ) projects in
  match prefix_matches with
  | [p] -> Some p  (* unique prefix *)
  | _ ->
  (* Substring match *)
  let substr_matches = List.filter (fun (p : Project.t) ->
    let name = String.lowercase_ascii p.name in
    let rec has_substr i =
      if i + String.length q > String.length name then false
      else if String.sub name i (String.length q) = q then true
      else has_substr (i + 1)
    in
    has_substr 0
  ) projects in
  match substr_matches with
  | [p] -> Some p  (* unique substring *)
  | _ -> None)

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

(** Split text into chunks that fit Discord's 2000-char message limit.
    Tries to split at paragraph breaks, then newlines, then spaces.
    Avoids splitting inside code blocks (triple backticks). *)
let split_message ?(max_len=1900) text =
  let len = String.length text in
  if len <= max_len then [text]
  else
    let find_split_point pos limit =
      (* Try paragraph break first *)
      let try_find sep =
        let sep_len = String.length sep in
        let best = ref None in
        let i = ref pos in
        while !i + sep_len <= limit do
          if String.sub text !i sep_len = sep then
            best := Some !i;
          incr i
        done;
        !best
      in
      match try_find "\n\n" with
      | Some p -> p + 2
      | None ->
        match try_find "\n" with
        | Some p -> p + 1
        | None ->
          match try_find " " with
          | Some p -> p + 1
          | None -> limit
    in
    let rec split pos acc =
      if pos >= len then List.rev acc
      else
        let remaining = len - pos in
        if remaining <= max_len then
          List.rev (String.sub text pos remaining :: acc)
        else
          let split_at = find_split_point pos (pos + max_len) in
          let chunk = String.sub text pos (split_at - pos) in
          (* Check if we're splitting inside a code block *)
          let backtick_count =
            let count = ref 0 in
            let i = ref 0 in
            let s = chunk in
            let slen = String.length s in
            while !i + 2 < slen do
              if s.[!i] = '`' && s.[!i+1] = '`' && s.[!i+2] = '`' then begin
                incr count;
                i := !i + 3
              end else
                incr i
            done;
            !count
          in
          let chunk =
            if backtick_count mod 2 = 1 then
              (* Odd number of ``` — we're inside a code block. Close it. *)
              chunk ^ "\n```"
            else chunk
          in
          split split_at (chunk :: acc)
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
    let lines = List.mapi (fun i (p : Project.t) ->
      Printf.sprintf "`%d.` **%s** — `%s`%s"
        (i + 1) p.name p.path (if p.is_bare then " [bare]" else "")
    ) t.projects in
    let text = if lines = [] then "No projects found."
      else "**Projects** (use `!start <name>` or `!start <number>`):\n"
           ^ String.concat "\n" lines in
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
    let proj = find_project_fuzzy t.projects project in
    (match proj with
     | None ->
       (* Show suggestions *)
       let q = String.lowercase_ascii project in
       let suggestions = List.filter (fun (p : Project.t) ->
         let name = String.lowercase_ascii p.name in
         let rec has_substr i =
           if i + String.length q > String.length name then false
           else if String.sub name i (String.length q) = q then true
           else has_substr (i + 1)
         in
         has_substr 0
       ) t.projects in
       let text = match suggestions with
         | [] -> Printf.sprintf "No project matching `%s`. Try `!projects` to see the list." project
         | _ ->
           Printf.sprintf "No unique match for `%s`. Did you mean:\n%s"
             project
             (String.concat "\n" (List.map (fun (p : Project.t) ->
               Printf.sprintf "- `!start %s`" p.name) suggestions))
       in
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id ~content:text ())
     | Some p ->
       let kind_str = Config.string_of_agent_kind kind in
       (* Create a worktree for the session *)
       let branch_name = Printf.sprintf "agent/%s-%s"
         kind_str (String.sub (generate_uuid ()) 0 8) in
       (match Project.create_worktree p ~branch_name with
       | Error e ->
         (* Fall back to existing working dir if worktree creation fails *)
         Logs.warn (fun m -> m "bot: worktree creation failed, using default: %s" e);
         (match working_dir_of_project p with
          | Error e2 ->
            ignore (Discord_rest.create_message t.rest
              ~channel_id:msg.channel_id
              ~content:(Printf.sprintf "Cannot find working directory: %s" e2) ())
          | Ok working_dir ->
            ignore (Discord_rest.create_message t.rest
              ~channel_id:msg.channel_id
              ~content:(Printf.sprintf "Worktree failed (%s), using `%s`" e working_dir) ()))
       | Ok worktree_path ->
         let working_dir = worktree_path in
         let thread_name = Printf.sprintf "%s / %s" kind_str p.name in
         (* Create project channel on demand if it doesn't exist yet *)
         let thread_parent = match ChannelMap.find_opt p.name t.project_channels with
           | Some ch_id -> ch_id
           | None ->
             match t.category_id with
             | Some cat_id ->
               let topic = Printf.sprintf "Agent sessions for %s (%s)" p.name p.path in
               (match Discord_rest.create_channel t.rest ~guild_id:t.config.guild_id
                        ~name:p.name ~parent_id:cat_id ~topic () with
                | Ok ch ->
                  t.project_channels <- ChannelMap.add p.name ch.id t.project_channels;
                  Logs.info (fun m -> m "bot: created channel for project %s" p.name);
                  ch.id
                | Error _ -> msg.channel_id)
             | None -> msg.channel_id
         in
         match Discord_rest.create_thread_no_message t.rest
                 ~channel_id:thread_parent ~name:thread_name () with
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
             system_prompt = None;
             message_count = 0;
           } in
           add_session t thread_ch.id session;
           let welcome = Printf.sprintf
             "**%s** session started for **%s**\nBranch: `%s`\nWorking in: `%s`\nSend a message to interact with the agent."
             kind_str p.name branch_name working_dir
           in
           ignore (Discord_rest.create_message t.rest
             ~channel_id:thread_ch.id ~content:welcome ())))
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
            system_prompt = None;
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
       remove_session t thread_id;
       ignore (Discord_rest.create_message t.rest
         ~channel_id:msg.channel_id
         ~content:(Printf.sprintf "Stopped session for **%s**." session.project_name) ()))
  | Cleanup_channels ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      if t.config.guild_id = "" then
        ignore (Discord_rest.create_message t.rest ~channel_id:msg.channel_id
          ~content:"No guild_id configured." ())
      else begin
        match Discord_rest.get_guild_channels t.rest ~guild_id:t.config.guild_id () with
        | Error e ->
          ignore (Discord_rest.create_message t.rest ~channel_id:msg.channel_id
            ~content:(Printf.sprintf "Failed to get channels: %s" e) ())
        | Ok channels ->
          (* Discord lowercases channel names, so compare case-insensitively *)
          let project_names = List.map (fun (p : Project.t) ->
            String.lowercase_ascii p.name) t.projects in
          (* Find channels under Agent Projects that don't match any project,
             plus duplicates (keep only the first channel per name) *)
          let seen_names = Hashtbl.create 32 in
          let to_delete = List.filter (fun (ch : Discord_types.channel) ->
            match ch.parent_id, ch.name, t.category_id with
            | Some pid, Some name, Some cat_id when pid = cat_id ->
              let lname = String.lowercase_ascii name in
              if Hashtbl.mem seen_names lname then
                true (* duplicate — delete *)
              else begin
                Hashtbl.add seen_names lname true;
                not (List.mem lname project_names) (* stale — delete *)
              end
            | _ -> false
          ) channels in
          if to_delete = [] then
            ignore (Discord_rest.create_message t.rest ~channel_id:msg.channel_id
              ~content:"No stale channels to clean up." ())
          else begin
            let clock = Eio.Stdenv.clock t.env in
            let deleted = ref 0 in
            List.iter (fun (ch : Discord_types.channel) ->
              let name = Option.value ~default:"?" ch.name in
              match Discord_rest.delete_channel t.rest ~channel_id:ch.id () with
              | Ok () ->
                Logs.info (fun m -> m "bot: deleted stale channel %s (%s)" name ch.id);
                incr deleted;
                Eio.Time.sleep clock 0.5
              | Error e ->
                Logs.warn (fun m -> m "bot: failed to delete channel %s: %s" name e)
            ) to_delete;
            ignore (Discord_rest.create_message t.rest ~channel_id:msg.channel_id
              ~content:(Printf.sprintf "Cleaned up %d stale channels." !deleted) ())
          end
      end)
  | Restart ->
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:"Rebuilding and restarting..." ());
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let build_and_restart () =
        (* Build in the project directory *)
        let build_exit = Sys.command
          "cd /home/tedks/Projects/claude-discord/master && nix develop --command dune build 2>&1" in
        if build_exit <> 0 then
          `Build_failed
        else begin
          (* Spawn new instance — it will kill us via pidfile *)
          let _pid = Unix.create_process "/bin/sh"
            [| "/bin/sh"; "-c";
               "cd /home/tedks/Projects/claude-discord/master && nix develop --command dune exec discord-agents &" |]
            Unix.stdin Unix.stdout Unix.stderr in
          `Restarting
        end
      in
      match Eio_unix.run_in_systhread build_and_restart with
      | `Build_failed ->
        ignore (Discord_rest.create_message t.rest
          ~channel_id:msg.channel_id ~content:"Build failed, not restarting." ())
      | `Restarting ->
        ignore (Discord_rest.create_message t.rest
          ~channel_id:msg.channel_id ~content:"Build succeeded. New instance starting (will kill this one via pidfile)." ());
        (* Give the new instance time to start and kill us *)
        let clock = Eio.Stdenv.clock t.env in
        Eio.Time.sleep clock 30.0)
  | Help ->
    let text = String.concat "\n" [
      "**Commands:**";
      "`!projects` — list discovered projects";
      "`!sessions` — list active bot sessions";
      "`!claude-sessions` — list recent Claude Code sessions on this machine";
      "`!start <project> <claude|codex|gemini>` — start a new agent session";
      "`!resume <session_id>` — resume an existing Claude session in a new thread";
      "`!stop <thread_id>` — stop a session";
      "`!cleanup-channels` — delete stale project channels from before dedup";
      "`!restart` — rebuild and restart the bot";
      "`!help` — this message";
    ] in
    ignore (Discord_rest.create_message t.rest
      ~channel_id:msg.channel_id ~content:text ())
  | Unknown _ -> ()

(** Move a project's channel to the top of the category (position 0).
    Called on session activity so recently-used projects float up. *)
let bump_project_channel t project_name =
  match ChannelMap.find_opt project_name t.project_channels with
  | None -> ()
  | Some ch_id ->
    if t.config.guild_id <> "" then
      ignore (Discord_rest.modify_channel_position t.rest
        ~guild_id:t.config.guild_id ~channel_id:ch_id ~position:0 ())

(** Handle a message in a session thread — run the agent and stream the response. *)
let handle_thread_message t msg =
  match SessionMap.find_opt msg.Discord_types.channel_id t.sessions with
  | None -> ()
  | Some session ->
    (* Fork a fiber so we don't block the gateway recv loop *)
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let channel_id = msg.Discord_types.channel_id in
      (* Ack with eyes reaction, send typing, bump channel to top *)
      ignore (Discord_rest.create_reaction t.rest ~channel_id
        ~message_id:msg.id ~emoji:"\xF0\x9F\x91\x80" ());
      ignore (Discord_rest.send_typing t.rest ~channel_id ());
      bump_project_channel t session.project_name;
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
               ?system_prompt:session.system_prompt
               ~prompt ~on_event () with
       | Ok () ->
         session.message_count <- session.message_count + 1;
         save_sessions t.sessions;
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

(** Ensure a session exists for a channel, creating one if needed.
    Used for the control channel and project channels so users can
    just chat naturally without explicit !start. *)
let control_system_prompt t =
  let project_list = String.concat "\n" (List.mapi (fun i (p : Project.t) ->
    Printf.sprintf "  %d. %s (%s)" (i+1) p.name p.path
  ) t.projects) in
  Printf.sprintf
"You are the control agent for a Discord bot that manages AI coding sessions.

You are chatting in a Discord channel. The user can ask you questions, ask you to start sessions, or just chat.

Available bot commands (the user can type these, or you can suggest them):
  !start <project> [agent] — start a Claude session for a project (agent defaults to claude)
  !resume <session_id> — resume an existing Claude Code session
  !projects — list all projects
  !sessions — list active sessions
  !claude-sessions — list recent Claude Code sessions on this machine
  !cleanup-channels — delete stale Discord channels
  !restart — rebuild and restart the bot
  !help — show commands

Project names support fuzzy matching — prefixes and substrings work.
You can also use project numbers from the !projects list.

Known projects:
%s

If the user asks to start a session or work on a project, suggest the appropriate !start command.
If they ask about what's available, suggest !projects or !claude-sessions.
Keep responses concise — this is Discord, not a document." project_list

let ensure_channel_session t ~channel_id ~project_name ~working_dir ~system_prompt =
  match SessionMap.find_opt channel_id t.sessions with
  | Some _ -> ()
  | None ->
    let session = {
      project_name;
      working_dir;
      agent_kind = Config.Claude;
      session_id = generate_uuid ();
      thread_id = channel_id;
      system_prompt;
      message_count = 0;
    } in
    add_session t channel_id session;
    Logs.info (fun m -> m "bot: auto-created session for channel %s (%s)"
      channel_id project_name)

(** Route an incoming Discord message. *)
let handle_message t (msg : Discord_types.message) =
  (* Ignore bot messages *)
  match msg.author.bot with Some true -> () | _ ->
  (* Commands (messages starting with !) are handled regardless of channel *)
  if is_command msg.content then
    handle_control_message t msg
  else begin
    (* Check if this is in the control channel or a project channel —
       auto-create a session for natural language chat *)
    let is_control = match t.config.control_channel_id with
      | Some ctl_id -> msg.channel_id = ctl_id
      | None -> false
    in
    let project_for_channel =
      ChannelMap.bindings t.project_channels
      |> List.find_opt (fun (_, ch_id) -> ch_id = msg.channel_id)
      |> Option.map fst
    in
    if is_control then begin
      (* Control channel: global session with system prompt describing capabilities *)
      let working_dir = Sys.getcwd () in
      ensure_channel_session t ~channel_id:msg.channel_id
        ~project_name:"control" ~working_dir
        ~system_prompt:(Some (control_system_prompt t));
      handle_thread_message t msg
    end else match project_for_channel with
    | Some proj_name ->
      (* Project channel: auto-create a session for that project *)
      let proj = List.find_opt (fun (p : Project.t) -> p.name = proj_name) t.projects in
      (match proj with
       | Some p ->
         let working_dir = match working_dir_of_project p with
           | Ok d -> d | Error _ -> p.path in
         ensure_channel_session t ~channel_id:msg.channel_id
           ~project_name:p.name ~working_dir ~system_prompt:None;
         handle_thread_message t msg
       | None -> handle_thread_message t msg)
    | None ->
      (* Thread or unknown channel — check existing sessions *)
      handle_thread_message t msg
  end

(** Set up project channels under an "Agent Projects" category.
    Finds or creates the category, then creates a channel per project. *)
let setup_project_channels t =
  let guild_id = t.config.guild_id in
  if guild_id = "" then
    Logs.info (fun m -> m "bot: no guild_id configured, skipping channel setup")
  else begin
    match Discord_rest.get_guild_channels t.rest ~guild_id () with
    | Error e ->
      Logs.warn (fun m -> m "bot: failed to get guild channels: %s" e)
    | Ok channels ->
      (* Find or create "Agent Projects" category (type 4) *)
      let category = List.find_opt (fun (ch : Discord_types.channel) ->
        ch.type_ = Guild_category
        && ch.name = Some "Agent Projects"
      ) channels in
      let cat_id = match category with
        | Some ch ->
          Logs.info (fun m -> m "bot: found Agent Projects category: %s" ch.id);
          ch.id
        | None ->
          Logs.info (fun m -> m "bot: creating Agent Projects category");
          match Discord_rest.create_channel t.rest ~guild_id
                  ~name:"Agent Projects" ~channel_type:4 () with
          | Ok ch -> ch.id
          | Error e ->
            Logs.warn (fun m -> m "bot: failed to create category: %s" e);
            ""
      in
      if cat_id <> "" then begin
        t.category_id <- Some cat_id;
        (* Map existing text channels under this category to projects *)
        List.iter (fun (ch : Discord_types.channel) ->
          match ch.parent_id, ch.name with
          | Some pid, Some name when pid = cat_id ->
            (* Check if this matches a project name *)
            if List.exists (fun (p : Project.t) -> p.name = name) t.projects then begin
              t.project_channels <- ChannelMap.add name ch.id t.project_channels;
              Logs.info (fun m -> m "bot: mapped channel %s -> project %s" ch.id name)
            end
          | _ -> ()
        ) channels;
        (* Channels created on demand via !start. Log mapping status. *)
        let mapped = ChannelMap.cardinal t.project_channels in
        let total = List.length t.projects in
        Logs.info (fun m -> m "bot: %d/%d projects have channels (others created on demand)"
          mapped total)
      end
  end

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
    sessions = load_sessions ();
    project_channels = ChannelMap.empty;
    category_id = None;
    env;
    sw;
  } in
  bot.gateway.handler <- (fun event ->
    match event with
    | Discord_gateway.Connected user ->
      Logs.info (fun m -> m "bot: connected as %s" user.Discord_types.username);
      (* Set up project channels only on first connect, not on resume/reconnect *)
      if bot.category_id = None then
        Eio.Fiber.fork ~sw (fun () ->
          setup_project_channels bot;
          (* Post startup announcement *)
          match bot.config.control_channel_id with
          | Some ch_id ->
            let n_projects = List.length bot.projects in
            let n_sessions = SessionMap.cardinal bot.sessions in
            let n_channels = ChannelMap.cardinal bot.project_channels in
            let text = Printf.sprintf
              "Bot online. %d projects, %d channels, %d active sessions."
              n_projects n_channels n_sessions in
            ignore (Discord_rest.create_message bot.rest
              ~channel_id:ch_id ~content:text ())
          | None -> ())
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
