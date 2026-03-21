(** Project discovery and git worktree management.

    Scans configured base directories for projects (directories containing
    .git or that are bare repos with worktrees). *)

type t = {
  name : string;
  path : string; (** Path to the project root (bare repo or .git parent) *)
  is_bare : bool;
}

let is_git_dir path =
  Sys.file_exists (Filename.concat path ".git")

let is_bare_repo path =
  (* A bare repo has HEAD, objects/, refs/ directly in the directory *)
  Sys.file_exists (Filename.concat path "HEAD")
  && Sys.is_directory (Filename.concat path "objects")
  && Sys.is_directory (Filename.concat path "refs")

let discover_in_directory base_dir =
  if not (Sys.file_exists base_dir && Sys.is_directory base_dir) then
    []
  else
    let entries = Sys.readdir base_dir |> Array.to_list in
    List.filter_map (fun name ->
      let path = Filename.concat base_dir name in
      if not (try Sys.is_directory path with Sys_error _ -> false) then None
      else if is_bare_repo path then
        Some { name; path; is_bare = true }
      else if is_git_dir path then
        Some { name; path; is_bare = false }
      else
        None
    ) entries

let discover ~base_directories =
  List.concat_map discover_in_directory base_directories

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
  (* Parse porcelain output: groups separated by blank lines *)
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
          (* Strip refs/heads/ prefix *)
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

(** Create a new worktree with a new branch for an agent session. *)
let create_worktree project ~branch_name =
  let worktree_path = Filename.concat project.path branch_name in
  let cmd = Printf.sprintf "git -C %s worktree add -b %s %s 2>&1"
    (Filename.quote project.path)
    (Filename.quote branch_name)
    (Filename.quote worktree_path)
  in
  let ic = Unix.open_process_in cmd in
  let rec drain () = match input_line ic with _ -> drain () | exception End_of_file -> () in
  drain ();
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok worktree_path
  | _ -> Error (Printf.sprintf "failed to create worktree: %s" branch_name)
