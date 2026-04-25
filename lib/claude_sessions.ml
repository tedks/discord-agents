(** Discovery of Claude Code sessions on disk.

    Scans ~/.claude/projects/ for recent session files (JSONL),
    extracts metadata (first user message, project directory),
    and resolves project directory names back to filesystem paths. *)

type info = {
  session_id : string;
  project_dir : string;
  working_dir : string;
  summary : string;
  mtime : float;
}

(** Resolve a Claude project directory name back to a filesystem path.
    e.g. "-home-tedks-Projects-claude-discord" -> "/home/tedks/Projects/claude-discord"

    Walks the filesystem greedily: at each level, tries the longest
    hyphenated segment that exists as a directory. *)
let resolve_project_dir proj_name =
  let s = if String.length proj_name > 0 && proj_name.[0] = '-'
          then String.sub proj_name 1 (String.length proj_name - 1)
          else proj_name in
  let rec resolve path remaining =
    if String.length remaining = 0 then path
    else
      let parts = String.split_on_char '-' remaining in
      let rec try_lengths n =
        if n < 1 then
          let first = List.hd parts in
          let rest_parts = List.tl parts in
          resolve (Filename.concat path first) (String.concat "-" rest_parts)
        else
          let candidate_parts = List.filteri (fun i _ -> i < n) parts in
          let candidate = String.concat "-" candidate_parts in
          let candidate_path = Filename.concat path candidate in
          if (try Sys.file_exists candidate_path with _ -> false) then
            let rest_parts = List.filteri (fun i _ -> i >= n) parts in
            resolve candidate_path (String.concat "-" rest_parts)
          else
            try_lengths (n - 1)
      in
      try_lengths (List.length parts)
  in
  resolve "/" s

(** Extract the first user message from a Claude session JSONL file. *)
let extract_summary fpath =
  try
    Resource.with_file_in fpath (fun ic ->
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
      !summary)
  with _ -> "(unknown)"

(** Scan for recent Claude sessions. Returns newest first. *)
let discover ?(hours=24) () =
  Eio_unix.run_in_systhread @@ fun () ->
  let home = Sys.getenv "HOME" in
  let projects_dir = Filename.concat home ".claude/projects" in
  if not (Sys.file_exists projects_dir) then []
  else
    let cutoff = Unix.gettimeofday () -. (float_of_int hours *. 3600.0) in
    let results = ref [] in
    let project_dirs =
      try Sys.readdir projects_dir |> Array.to_list with _ -> [] in
    List.iter (fun proj_name ->
      let proj_path = Filename.concat projects_dir proj_name in
      if (try Sys.is_directory proj_path with Sys_error _ -> false) then begin
        let files =
          try Sys.readdir proj_path |> Array.to_list with Sys_error _ -> [] in
        List.iter (fun fname ->
          if Filename.check_suffix fname ".jsonl"
             && not (String.contains fname '/') then begin
            let fpath = Filename.concat proj_path fname in
            match (try Some (Unix.stat fpath) with Unix.Unix_error _ -> None) with
            | Some st when st.Unix.st_mtime > cutoff ->
              results := {
                session_id = Filename.chop_suffix fname ".jsonl";
                project_dir = proj_name;
                working_dir = resolve_project_dir proj_name;
                summary = extract_summary fpath;
                mtime = st.Unix.st_mtime;
              } :: !results
            | _ -> ()
          end
        ) files
      end
    ) project_dirs;
    List.sort (fun a b -> compare b.mtime a.mtime) !results

(** Find a session by ID prefix. Wraps the filesystem walk in a
    systhread so the Eio fiber doesn't block — matches the convention
    Codex_sessions and Gemini_sessions use. *)
let find_by_prefix session_id_prefix =
  Eio_unix.run_in_systhread @@ fun () ->
  let home = Sys.getenv "HOME" in
  let projects_dir = Filename.concat home ".claude/projects" in
  if not (Sys.file_exists projects_dir) then None
  else
    let project_dirs =
      try Sys.readdir projects_dir |> Array.to_list with _ -> [] in
    let result = ref None in
    List.iter (fun proj_name ->
      if !result = None then begin
        let proj_path = Filename.concat projects_dir proj_name in
        if (try Sys.is_directory proj_path with Sys_error _ -> false) then begin
          let files =
            try Sys.readdir proj_path |> Array.to_list with _ -> [] in
          List.iter (fun fname ->
            if !result = None
               && Filename.check_suffix fname ".jsonl"
               && not (String.contains fname '/') then begin
              let sid = Filename.chop_suffix fname ".jsonl" in
              if String.length sid >= String.length session_id_prefix
                 && String.sub sid 0 (String.length session_id_prefix)
                    = session_id_prefix then
                result := Some (sid, resolve_project_dir proj_name)
            end
          ) files
        end
      end
    ) project_dirs;
    !result
