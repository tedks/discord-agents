(** Discovery of Gemini CLI sessions on disk.

    Scans ~/.gemini/tmp/<project-dir>/chats/session-*.json for recent
    session files, extracts metadata (first user message, project hash),
    and resolves projectHash back to a filesystem path via
    ~/.gemini/projects.json (projectHash = sha256 of the absolute path). *)

type info = {
  session_id : string;
  working_dir : string;  (** Resolved from projectHash; "" if unresolvable *)
  summary : string;
  mtime : float;
}

let read_file path =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    Bytes.to_string s)

(** SHA-256 of a string via the [sha256sum] shell utility, fed over
    stdin so we don't have to manage a temp file. Returns hex digest
    or "" on failure. We shell out to avoid taking a cryptographic-
    hash dependency for one-shot session discovery. *)
let sha256_hex s =
  try
    let (ic, oc) = Unix.open_process "sha256sum 2>/dev/null" in
    Fun.protect
      ~finally:(fun () -> ignore (Unix.close_process (ic, oc)))
      (fun () ->
        output_string oc s;
        close_out oc;
        let line = try input_line ic with End_of_file -> "" in
        match String.index_opt line ' ' with
        | Some i -> String.sub line 0 i
        | None -> String.trim line)
  with _ -> ""

(** Build a {sha256(path) -> path} table from ~/.gemini/projects.json. *)
let build_hash_table () =
  let home = Sys.getenv "HOME" in
  let path = Filename.concat home ".gemini/projects.json" in
  let tbl = Hashtbl.create 32 in
  if not (Sys.file_exists path) then tbl
  else begin
    (try
      let json = Yojson.Safe.from_string (read_file path) in
      let open Yojson.Safe.Util in
      (match json |> member "projects" with
       | `Assoc pairs ->
         List.iter (fun (project_path, _name) ->
           let h = sha256_hex project_path in
           if h <> "" then Hashtbl.replace tbl h project_path
         ) pairs
       | _ -> ())
    with _ -> ());
    tbl
  end

(** Extract (sessionId, projectHash, firstUserText) from a Gemini session
    JSON. Returns None if the file isn't a parseable session. *)
let extract_meta fpath =
  try
    let json = Yojson.Safe.from_string (read_file fpath) in
    let open Yojson.Safe.Util in
    let sid = json |> member "sessionId" |> to_string_option in
    let phash = json |> member "projectHash" |> to_string_option
      |> Option.value ~default:"" in
    let summary =
      match json |> member "messages" with
      | `List (m :: _) ->
        (match m |> member "type" |> to_string_option with
         | Some "user" ->
           (match m |> member "content" with
            | `List items ->
              (match List.find_opt (fun it ->
                 match it |> member "text" |> to_string_option with
                 | Some _ -> true | None -> false) items with
               | Some first ->
                 (match first |> member "text" |> to_string_option with
                  | Some t -> String.sub t 0 (min 80 (String.length t))
                  | None -> "")
               | None -> "")
            | `String s -> String.sub s 0 (min 80 (String.length s))
            | _ -> "")
         | _ -> "")
      | _ -> ""
    in
    match sid with
    | Some sid -> Some (sid, phash, summary)
    | None -> None
  with _ -> None

(** Walk ~/.gemini/tmp/*/chats/*.json, yielding (fpath, mtime) for each
    session-*.json newer than cutoff. *)
let walk_sessions ?(cutoff=neg_infinity) () =
  let home = Sys.getenv "HOME" in
  let tmp_dir = Filename.concat home ".gemini/tmp" in
  if not (Sys.file_exists tmp_dir) then []
  else
    let acc = ref [] in
    let project_dirs =
      try Sys.readdir tmp_dir |> Array.to_list with _ -> [] in
    List.iter (fun proj_name ->
      let chats_dir = Filename.concat
        (Filename.concat tmp_dir proj_name) "chats" in
      if (try Sys.is_directory chats_dir with Sys_error _ -> false) then begin
        let files =
          try Sys.readdir chats_dir |> Array.to_list with Sys_error _ -> [] in
        List.iter (fun fname ->
          if Filename.check_suffix fname ".json"
             && String.length fname >= 8
             && String.sub fname 0 8 = "session-" then begin
            let fpath = Filename.concat chats_dir fname in
            match (try Some (Unix.stat fpath) with Unix.Unix_error _ -> None) with
            | Some st when st.Unix.st_mtime > cutoff ->
              acc := (fpath, st.Unix.st_mtime) :: !acc
            | _ -> ()
          end
        ) files
      end
    ) project_dirs;
    !acc

(** Scan for recent Gemini sessions. Returns newest first. *)
let discover ?(hours=24) () =
  Eio_unix.run_in_systhread @@ fun () ->
  let cutoff = Unix.gettimeofday () -. (float_of_int hours *. 3600.0) in
  let hash_tbl = build_hash_table () in
  let files = walk_sessions ~cutoff () in
  let infos = List.filter_map (fun (fpath, mtime) ->
    match extract_meta fpath with
    | Some (session_id, phash, summary) ->
      let working_dir = try Hashtbl.find hash_tbl phash with Not_found -> "" in
      Some { session_id; working_dir; summary; mtime }
    | None -> None
  ) files in
  List.sort (fun a b -> compare b.mtime a.mtime) infos

(** Find a session by ID prefix across all projects.
    Returns (full_session_id, working_dir) or None. *)
let find_by_prefix session_id_prefix =
  Eio_unix.run_in_systhread @@ fun () ->
  let hash_tbl = build_hash_table () in
  let files = walk_sessions () in
  let prefix_len = String.length session_id_prefix in
  let matched = List.find_map (fun (fpath, _) ->
    match extract_meta fpath with
    | Some (sid, phash, _) ->
      if String.length sid >= prefix_len
         && String.sub sid 0 prefix_len = session_id_prefix then
        let wd = try Hashtbl.find hash_tbl phash with Not_found -> "" in
        Some (sid, wd)
      else None
    | None -> None
  ) files in
  matched
