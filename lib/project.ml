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

let strip_suffix s suffix =
  let suffix_len = String.length suffix in
  if String.length s >= suffix_len
     && String.sub s (String.length s - suffix_len) suffix_len = suffix
  then String.sub s 0 (String.length s - suffix_len)
  else s

let is_valid_github_path_part s =
  s <> ""
  && s <> "."
  && s <> ".."
  && String.for_all (function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' -> true
    | _ -> false) s

let parse_github_remote url =
  let parse_path path =
    let path = if String.length path > 0 && path.[0] = '/'
      then String.sub path 1 (String.length path - 1)
      else path in
    let path = strip_suffix path ".git" in
    match String.split_on_char '/' path with
    | [owner; repo]
      when is_valid_github_path_part owner
           && is_valid_github_path_part repo ->
      Some (String.lowercase_ascii owner, repo)
    | _ -> None
  in
  let lower = String.lowercase_ascii url in
  let https_prefix = "https://github.com/" in
  let ssh_prefix = "git@github.com:" in
  let ssh_url_prefix = "ssh://git@github.com/" in
  if String.length lower > String.length https_prefix
     && String.sub lower 0 (String.length https_prefix) = https_prefix
  then
    parse_path (String.sub url (String.length https_prefix)
      (String.length url - String.length https_prefix))
  else if String.length lower > String.length ssh_prefix
          && String.sub lower 0 (String.length ssh_prefix) = ssh_prefix
  then
    parse_path (String.sub url (String.length ssh_prefix)
      (String.length url - String.length ssh_prefix))
  else if String.length lower > String.length ssh_url_prefix
          && String.sub lower 0 (String.length ssh_url_prefix) = ssh_url_prefix
  then
    parse_path (String.sub url (String.length ssh_url_prefix)
      (String.length url - String.length ssh_url_prefix))
  else
    None

let validate_github_url url =
  let url = String.trim url in
  if url = "" then
    Error "GitHub URL is required."
  else if String.exists (fun c -> Char.code c < 0x20 || c = ' ') url then
    Error "GitHub URL must not contain spaces or control characters."
  else
    match parse_github_remote url with
    | Some _ -> Ok url
    | None ->
      Error "Expected a GitHub HTTPS or SSH URL like https://github.com/owner/repo.git or git@github.com:owner/repo.git."

let repo_name_from_github_url url =
  match parse_github_remote url with
  | Some (_, repo) -> Some repo
  | None -> None

let validate_import_name name =
  let name = String.trim name in
  if name = "" then
    Error "Project name is required."
  else if String.length name > 100 then
    Error "Project name must be 100 characters or fewer."
  else if not (is_valid_github_path_part name) then
    Error "Project name may contain only letters, numbers, dot, underscore, and hyphen."
  else
    Ok name

let remote_key url =
  match parse_github_remote url with
  | Some (owner, repo) ->
    Some ("github.com/" ^ owner ^ "/" ^ String.lowercase_ascii repo)
  | None ->
    let norm = String.lowercase_ascii url in
    let norm = strip_suffix norm ".git" in
    let stripped = List.fold_left (fun s prefix ->
      if String.length s > String.length prefix &&
         String.sub s 0 (String.length prefix) = prefix
      then String.sub s (String.length prefix) (String.length s - String.length prefix)
      else s
    ) norm ["https://"; "http://"] in
    (match String.split_on_char ':' stripped with
     | [host; path] when not (String.contains host '/') ->
       let host = match String.split_on_char '@' host with
         | [_; h] -> h | _ -> host in
       Some (host ^ "/" ^ path)
     | _ when stripped <> "" -> Some stripped
     | _ -> None)

let same_remote_url a b =
  match remote_key a, remote_key b with
  | Some a, Some b -> String.equal a b
  | _ -> false

let find_by_remote_url projects url =
  List.find_opt (fun (p : t) ->
    match p.remote_url with
    | Some remote -> same_remote_url remote url
    | None -> false) projects

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

(** Max git repos to pull in from a single non-git parent directory. Protects
    against vendored trees (node_modules/, third_party/) accidentally exploding
    the project list. *)
let max_nested_repos = 10

let project_of_dir ~name path =
  let is_bare = is_bare_repo path in
  let is_git = is_git_dir path in
  if is_bare || is_git then
    let remote_url = get_remote_url path is_bare in
    Some { name; path; is_bare; remote_url }
  else
    None

let discover_in_directory base_dir =
  if not (Sys.file_exists base_dir && (try Sys.is_directory base_dir with Sys_error _ -> false)) then
    []
  else
    let entries = Sys.readdir base_dir |> Array.to_list in
    List.concat_map (fun name ->
      let path = Filename.concat base_dir name in
      if not (try Sys.is_directory path with Sys_error _ -> false) then []
      else
        match project_of_dir ~name path with
        | Some p -> [p]
        | None ->
          (* Non-git directory: scan one level deeper for clustered repos
             (e.g. ~/Projects/books/{lsqlthw,rust,projectsthw}). *)
          let sub_entries =
            try Sys.readdir path |> Array.to_list
            with Sys_error _ -> []
          in
          let nested = List.filter_map (fun sub_name ->
            let sub_path = Filename.concat path sub_name in
            if not (try Sys.is_directory sub_path with Sys_error _ -> false) then None
            else
              project_of_dir ~name:(name ^ "/" ^ sub_name) sub_path
          ) sub_entries in
          if List.length nested > max_nested_repos then []
          else nested
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
      let key = Option.value (remote_key url) ~default:(String.lowercase_ascii url) in
      match UrlMap.find_opt key !by_url with
      | None -> by_url := UrlMap.add key p !by_url
      | Some existing ->
        (* Prefer bare repos (they're the canonical location for worktree users) *)
        if p.is_bare && not existing.is_bare then
          by_url := UrlMap.add key p !by_url
        (* else keep existing *)
  ) projects;
  (* Rename URL-matched projects to their remote repo name. For cluster
     repos (name contains "/" from nested discovery), keep the parent
     prefix so "books/rust" with remote "foo/rust-learn" becomes
     "books/rust-learn", preserving the grouping context.

     We take only the first slash because discovery currently recurses
     exactly one level (so at most one slash exists in the name). If
     discovery is ever extended to deeper recursion, revisit this to
     preserve the full parent path. *)
  let url_projects = UrlMap.bindings !by_url |> List.map (fun (_, p) ->
    match p.remote_url with
    | Some url ->
      let name = match repo_name_of_url url with
        | Some n ->
          (match String.index_opt p.name '/' with
           | Some i -> String.sub p.name 0 i ^ "/" ^ n
           | None -> n)
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

let rec mkdir_p path =
  if path = "" || path = Filename.dirname path then ()
  else if Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_rf (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path

let run_capture ?cwd args =
  let prefix = match cwd with
    | Some dir -> "cd " ^ Filename.quote dir ^ " && "
    | None -> "" in
  let cmd = prefix ^ String.concat " " (List.map Filename.quote args) ^ " 2>&1" in
  let ic = Unix.open_process_in cmd in
  let output = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string output (input_line ic);
       Buffer.add_char output '\n'
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok (Buffer.contents output)
  | _ -> Error (Buffer.contents output)

let default_branch project =
  let fallback () =
    let try_branch name =
      match run_capture ["git"; "-C"; project.path; "rev-parse"; "--verify"; name] with
      | Ok _ -> true
      | Error _ -> false
    in
    if try_branch "main" then "main"
    else if try_branch "master" then "master"
    else "HEAD"
  in
  let symbolic_ref ref_name =
    match run_capture ["git"; "-C"; project.path; "symbolic-ref"; "--short"; ref_name] with
    | Ok branch ->
      let branch = String.trim branch in
      if branch = "" then None else Some branch
    | Error _ -> None
  in
  if project.is_bare then
    match symbolic_ref "HEAD" with
    | Some branch -> branch
    | None -> fallback ()
  else
    match symbolic_ref "refs/remotes/origin/HEAD" with
    | Some branch ->
      let origin_prefix = "origin/" in
      if String.length branch > String.length origin_prefix
         && String.sub branch 0 (String.length origin_prefix) = origin_prefix
      then String.sub branch (String.length origin_prefix)
        (String.length branch - String.length origin_prefix)
      else branch
    | None -> fallback ()

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

let worktree_dir_name branch =
  String.map (function '/' | '\\' | ':' -> '-' | c -> c) branch

let unique_preserving_order items =
  let rec aux seen acc = function
    | [] -> List.rev acc
    | x :: xs when List.mem x seen -> aux seen acc xs
    | x :: xs -> aux (x :: seen) (x :: acc) xs
  in
  aux [] [] items

let default_worktree_path project =
  let branch = default_branch project in
  match
    list_worktrees project
    |> List.find_opt (fun (b, path) ->
      b = branch && path <> project.path && Sys.file_exists path)
  with
  | Some (_, path) -> Ok path
  | None ->
    let candidates =
      unique_preserving_order
        [branch; worktree_dir_name branch; "master"; "main"]
    in
    match List.find_opt (fun name ->
      let path = Filename.concat project.path name in
      try Sys.is_directory path with Sys_error _ -> false
    ) candidates with
    | Some name -> Ok (Filename.concat project.path name)
    | None ->
      (match List.find_opt (fun (_branch, path) ->
         path <> project.path && Sys.file_exists path) (list_worktrees project) with
       | Some (_, path) -> Ok path
       | None -> Error "bare repo has no default worktree")

let ensure_default_worktree project =
  match default_worktree_path project with
  | Ok path -> Ok path
  | Error _ ->
    let branch = default_branch project in
    let worktree_name =
      worktree_dir_name branch
    in
    let worktree_path = Filename.concat project.path worktree_name in
    match run_capture
      ["git"; "-C"; project.path; "worktree"; "add"; worktree_path; branch]
    with
    | Ok _ -> Ok worktree_path
    | Error err ->
      Error (Printf.sprintf "failed to create default worktree for %s: %s"
        branch err)

type import_result = {
  project : t;
  worktree_path : string;
}

let import_github ~base_directory ?name url =
  match validate_github_url url with
  | Error _ as err -> err
  | Ok url ->
    let name =
      match name with
      | Some n -> validate_import_name n
      | None ->
        (match repo_name_from_github_url url with
         | Some n -> validate_import_name n
         | None -> Error "Could not derive a project name from the GitHub URL.")
    in
    match name with
    | Error _ as err -> err
    | Ok name ->
      let target = Filename.concat base_directory name in
      if Sys.file_exists target then
        Error (Printf.sprintf "Target path already exists: %s" target)
      else begin
        mkdir_p base_directory;
        match run_capture ["git"; "clone"; "--bare"; url; target] with
        | Error err ->
          if Sys.file_exists target then rm_rf target;
          Error (Printf.sprintf "git clone failed: %s" err)
        | Ok _ ->
          let project = { name; path = target; is_bare = true; remote_url = Some url } in
          match ensure_default_worktree project with
          | Ok worktree_path -> Ok { project; worktree_path }
          | Error err ->
            if Sys.file_exists target then rm_rf target;
            Error err
      end

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

let run_git_capture project args =
  let cmd =
    "git -C "
    ^ Filename.quote project.path
    ^ " "
    ^ String.concat " " (List.map Filename.quote args)
    ^ " 2>&1"
  in
  let ic = Unix.open_process_in cmd in
  let output = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string output (input_line ic);
       Buffer.add_char output '\n'
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok ()
  | _ -> Error (Buffer.contents output)

let remove_worktree project ~branch_name ~worktree_path =
  let path_existed = Sys.file_exists worktree_path in
  let worktree_result =
    match run_git_capture project ["worktree"; "remove"; "--force"; worktree_path] with
    | Ok () -> Ok ()
    | Error err ->
      (match run_git_capture project ["worktree"; "prune"] with
       | Ok () when not path_existed -> Ok ()
       | Ok () -> Error err
       | Error prune_err ->
         Error (Printf.sprintf "%s; git worktree prune failed: %s"
           err prune_err))
  in
  let branch_result =
    run_git_capture project ["branch"; "-D"; branch_name]
  in
  match worktree_result, branch_result with
  | Ok (), Ok () -> Ok ()
  | Error worktree_err, Ok () ->
    Error (Printf.sprintf "failed to remove worktree %s: %s"
      worktree_path worktree_err)
  | Ok (), Error branch_err ->
    Error (Printf.sprintf "failed to delete branch %s: %s"
      branch_name branch_err)
  | Error worktree_err, Error branch_err ->
    Error (Printf.sprintf
      "failed to remove worktree %s: %s; failed to delete branch %s: %s"
      worktree_path worktree_err branch_name branch_err)
