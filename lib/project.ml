(** Project discovery and git worktree management.

    Scans configured base directories for projects (directories containing
    .git or that are bare repos with worktrees).

    Deduplicates by upstream remote URL — if multiple directories point
    to the same remote, only one is kept, named after the remote repo. *)

type t = {
  name : string;
  path : string; (** Path to the project root (bare repo or .git parent) *)
  is_bare : bool;
  remote_url : string option;
}

let is_git_dir path =
  Sys.file_exists (Filename.concat path ".git")

let is_bare_repo path =
  (* A bare repo has HEAD, objects/, refs/ directly in the directory *)
  Sys.file_exists (Filename.concat path "HEAD")
  && (try Sys.is_directory (Filename.concat path "objects") with Sys_error _ -> false)
  && (try Sys.is_directory (Filename.concat path "refs") with Sys_error _ -> false)

(** Get the origin remote URL for a git repo. Tries the repo directly,
    then master/ and main/ worktrees for bare repos. *)
let get_remote_url path is_bare =
  let try_git_dir dir =
    let cmd = Printf.sprintf "git -C %s remote get-url origin 2>/dev/null"
      (Filename.quote dir) in
    let ic = Unix.open_process_in cmd in
    let result = try Some (String.trim (input_line ic)) with End_of_file -> None in
    let _ = Unix.close_process_in ic in
    result
  in
  match try_git_dir path with
  | Some url -> Some url
  | None when is_bare ->
    (* Try worktree subdirs *)
    let candidates = ["master"; "main"] in
    List.find_map (fun name ->
      let sub = Filename.concat path name in
      if try Sys.is_directory sub with Sys_error _ -> false
      then try_git_dir sub
      else None
    ) candidates
  | None -> None

(** Extract a clean repo name from a remote URL.
    "git@github.com:tedks/CodingGame.git" -> "CodingGame"
    "https://github.com/tedks/PureSky.git" -> "PureSky" *)
let repo_name_of_url url =
  (* Strip trailing .git *)
  let url = if Filename.check_suffix url ".git"
    then Filename.chop_suffix url ".git" else url in
  (* Take the last path component *)
  match String.split_on_char '/' url with
  | [] -> None
  | parts ->
    let last = List.nth parts (List.length parts - 1) in
    (* Handle ssh format: might have ":" before user/repo *)
    let last = match String.split_on_char ':' last with
      | [_; repo_path] ->
        (* "tedks/CodingGame" -> take after / *)
        (match String.split_on_char '/' repo_path with
         | [_; name] -> name
         | _ -> repo_path)
      | _ -> last
    in
    if last = "" then None else Some last

let discover_in_directory base_dir =
  if not (Sys.file_exists base_dir && (try Sys.is_directory base_dir with Sys_error _ -> false)) then
    []
  else
    let entries = Sys.readdir base_dir |> Array.to_list in
    List.filter_map (fun name ->
      let path = Filename.concat base_dir name in
      if not (try Sys.is_directory path with Sys_error _ -> false) then None
      else
        let is_bare = is_bare_repo path in
        let is_git = is_git_dir path in
        if is_bare || is_git then
          let remote_url = get_remote_url path is_bare in
          Some { name; path; is_bare; remote_url }
        else
          None
    ) entries

(** Deduplicate projects by remote URL.
    When multiple directories share the same remote:
    - Prefer bare repos over normal repos
    - Name the project after the remote repo name
    - Keep whichever was found first *)
let deduplicate projects =
  let module UrlMap = Map.Make(String) in
  let by_url = ref UrlMap.empty in
  let no_remote = ref [] in
  List.iter (fun (p : t) ->
    match p.remote_url with
    | None -> no_remote := p :: !no_remote
    | Some url ->
      (* Normalize URL: strip .git, convert SSH to common format *)
      let norm = String.lowercase_ascii url in
      let norm = if String.length norm > 4 &&
        String.sub norm (String.length norm - 4) 4 = ".git"
        then String.sub norm 0 (String.length norm - 4) else norm in
      let key =
        (* git@github.com:user/repo -> github.com/user/repo *)
        match String.split_on_char ':' norm with
        | [host; path] when String.length host > 0 && host.[0] <> '/' ->
          let host = match String.split_on_char '@' host with
            | [_; h] -> h | _ -> host in
          host ^ "/" ^ path
        | _ ->
          (* https://github.com/user/repo -> github.com/user/repo *)
          let stripped = List.fold_left (fun s prefix ->
            if String.length s > String.length prefix &&
               String.sub s 0 (String.length prefix) = prefix
            then String.sub s (String.length prefix) (String.length s - String.length prefix)
            else s
          ) norm ["https://"; "http://"] in
          stripped
      in
      match UrlMap.find_opt key !by_url with
      | None -> by_url := UrlMap.add key p !by_url
      | Some existing ->
        (* Prefer bare repos (they're the canonical location for worktree users) *)
        if p.is_bare && not existing.is_bare then
          by_url := UrlMap.add key p !by_url
        (* else keep existing *)
  ) projects;
  (* Rename URL-matched projects to their remote repo name *)
  let url_projects = UrlMap.bindings !by_url |> List.map (fun (_, p) ->
    match p.remote_url with
    | Some url ->
      let name = match repo_name_of_url url with
        | Some n -> n
        | None -> p.name
      in
      { p with name }
    | None -> p
  ) in
  let all = url_projects @ List.rev !no_remote in
  (* Sort by name *)
  List.sort (fun a b -> String.compare a.name b.name) all

let discover ~base_directories =
  let raw = List.concat_map discover_in_directory base_directories in
  deduplicate raw

(** List worktrees for a project. Returns (branch_name, worktree_path) pairs. *)
let list_worktrees project =
  let cmd = Printf.sprintf "git -C %s worktree list --porcelain" (Filename.quote project.path) in
  let ic = Unix.open_process_in cmd in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  let _ = Unix.close_process_in ic in
  let rec parse_groups lines current_path current_branch acc =
    match lines with
    | [] ->
      let acc = match current_path, current_branch with
        | Some p, Some b -> (b, p) :: acc
        | Some p, None -> ("(detached)", p) :: acc
        | _ -> acc
      in
      List.rev acc
    | "" :: rest ->
      let acc = match current_path, current_branch with
        | Some p, Some b -> (b, p) :: acc
        | Some p, None -> ("(detached)", p) :: acc
        | _ -> acc
      in
      parse_groups rest None None acc
    | line :: rest ->
      let path = match String.split_on_char ' ' line with
        | "worktree" :: p :: _ -> Some p
        | _ -> current_path
      in
      let branch = match String.split_on_char ' ' line with
        | "branch" :: b :: _ ->
          let prefix = "refs/heads/" in
          if String.length b > String.length prefix
             && String.sub b 0 (String.length prefix) = prefix then
            Some (String.sub b (String.length prefix) (String.length b - String.length prefix))
          else
            Some b
        | _ -> current_branch
      in
      parse_groups rest path branch acc
  in
  parse_groups lines None None []

(** Find the default branch for a project (main or master). *)
let default_branch project =
  (* Try git symbolic-ref, then fall back to checking common names *)
  let try_branch name =
    let cmd = Printf.sprintf "git -C %s rev-parse --verify %s 2>/dev/null"
      (Filename.quote project.path) (Filename.quote name) in
    let ic = Unix.open_process_in cmd in
    let _output = try input_line ic with End_of_file -> "" in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> true
    | _ -> false
  in
  if try_branch "main" then "main"
  else if try_branch "master" then "master"
  else "HEAD"

(** Create a new worktree with a new branch for an agent session.
    Bases the branch on the project's default branch (main/master). *)
let create_worktree project ~branch_name =
  let worktree_path = Filename.concat project.path branch_name in
  let start_point = default_branch project in
  let cmd = Printf.sprintf "git -C %s worktree add -b %s %s %s 2>&1"
    (Filename.quote project.path)
    (Filename.quote branch_name)
    (Filename.quote worktree_path)
    (Filename.quote start_point)
  in
  let ic = Unix.open_process_in cmd in
  let output = Buffer.create 256 in
  (try while true do Buffer.add_string output (input_line ic); Buffer.add_char output '\n' done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok worktree_path
  | _ -> Error (Printf.sprintf "failed to create worktree %s: %s"
    branch_name (Buffer.contents output))
