(** Tests for project discovery, focused on one-level recursion into non-git
    cluster directories (e.g. ~/Projects/books/{rust,sqlthw}). *)

module P = Discord_agents.Project

let mkdir_p path =
  let rec aux p =
    if not (Sys.file_exists p) then begin
      aux (Filename.dirname p);
      Unix.mkdir p 0o755
    end
  in
  aux path

let make_git_dir path =
  mkdir_p (Filename.concat path ".git")

let make_bare_repo path =
  mkdir_p path;
  (* A bare repo has HEAD file plus objects/ and refs/ directories *)
  let head = Filename.concat path "HEAD" in
  let oc = open_out head in
  output_string oc "ref: refs/heads/main\n";
  close_out oc;
  mkdir_p (Filename.concat path "objects");
  mkdir_p (Filename.concat path "refs")

(* Walk the tree without following symlinks so a test that creates a
   self-referential symlink (for the symlink-loop case) doesn't cause
   infinite recursion during cleanup. *)
let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { st_kind = S_DIR; _ } ->
    Sys.readdir path
    |> Array.iter (fun name -> rm_rf (Filename.concat path name));
    Unix.rmdir path
  | _ ->
    (* Regular file, symlink, or other — unlink directly. *)
    Unix.unlink path

let with_tmpdir f =
  let base = Filename.temp_file "discord_agents_test_" "" in
  Sys.remove base;
  Unix.mkdir base 0o755;
  Fun.protect ~finally:(fun () -> rm_rf base) (fun () -> f base)

let names_of projects =
  List.map (fun (p : P.t) -> p.name) projects
  |> List.sort String.compare

(* ── tests ───────────────────────────────────────────────────────── *)

let test_flat_repo () =
  with_tmpdir (fun base ->
    make_git_dir (Filename.concat base "alpha");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "single flat repo"
      ["alpha"] (names_of ps))

let test_bare_repo () =
  with_tmpdir (fun base ->
    make_bare_repo (Filename.concat base "beta");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "single bare repo"
      ["beta"] (names_of ps);
    Alcotest.(check bool) "bare flag set"
      true (List.hd ps).is_bare)

let test_cluster_non_git_parent () =
  with_tmpdir (fun base ->
    make_git_dir (Filename.concat base "books/rust");
    make_git_dir (Filename.concat base "books/sqlthw");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "cluster children discovered with parent/child names"
      ["books/rust"; "books/sqlthw"] (names_of ps))

let test_cluster_ignored_when_parent_is_git () =
  (* If the parent is itself a git repo, we should NOT recurse into it. *)
  with_tmpdir (fun base ->
    make_git_dir (Filename.concat base "interview");
    make_git_dir (Filename.concat base "interview/narmi");
    make_git_dir (Filename.concat base "interview/magiceden");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "nested repos ignored when parent is a repo"
      ["interview"] (names_of ps))

let test_mixed_top_level () =
  with_tmpdir (fun base ->
    make_git_dir (Filename.concat base "flat");
    make_bare_repo (Filename.concat base "flatbare");
    make_git_dir (Filename.concat base "cluster/one");
    make_git_dir (Filename.concat base "cluster/two");
    mkdir_p (Filename.concat base "not_git/not_a_repo");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "mixed: flat + bare + cluster; non-git skipped"
      ["cluster/one"; "cluster/two"; "flat"; "flatbare"] (names_of ps))

let test_cluster_cap () =
  (* More than max_nested_repos git children in a single non-git parent
     should cause the whole cluster to be skipped. Protects against
     vendored trees blowing up the project list. *)
  with_tmpdir (fun base ->
    for i = 0 to 10 do
      make_git_dir (Filename.concat base (Printf.sprintf "vendor/pkg%02d" i))
    done;
    (* Sibling cluster under the cap should still be found *)
    make_git_dir (Filename.concat base "books/rust");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "oversized cluster skipped; small cluster kept"
      ["books/rust"] (names_of ps))

let test_cluster_at_cap () =
  with_tmpdir (fun base ->
    for i = 0 to 9 do
      make_git_dir (Filename.concat base (Printf.sprintf "c/p%d" i))
    done;
    let ps = P.discover_in_directory base in
    Alcotest.(check int) "exactly max_nested_repos kept"
      10 (List.length ps))

let test_missing_base_dir () =
  with_tmpdir (fun base ->
    let ps = P.discover_in_directory (Filename.concat base "does_not_exist") in
    Alcotest.(check (list string)) "missing base dir returns empty"
      [] (names_of ps))

let test_file_in_base_dir () =
  (* Regular files at top level should be ignored, not crash. *)
  with_tmpdir (fun base ->
    let oc = open_out (Filename.concat base "readme.txt") in
    output_string oc "hello";
    close_out oc;
    make_git_dir (Filename.concat base "alpha");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "file ignored, repo found"
      ["alpha"] (names_of ps))

let test_file_in_cluster () =
  (* Regular files inside a cluster dir should be ignored. *)
  with_tmpdir (fun base ->
    mkdir_p (Filename.concat base "books");
    let oc = open_out (Filename.concat base "books/notes.md") in
    output_string oc "notes";
    close_out oc;
    make_git_dir (Filename.concat base "books/rust");
    let ps = P.discover_in_directory base in
    Alcotest.(check (list string)) "file in cluster ignored"
      ["books/rust"] (names_of ps))

let test_deduplicate_preserves_cluster_names () =
  (* Cluster children without a remote should retain their parent/child
     names through deduplicate (which only renames URL-matched entries). *)
  with_tmpdir (fun base ->
    make_git_dir (Filename.concat base "books/rust");
    make_git_dir (Filename.concat base "books/sqlthw");
    let ps = P.discover ~base_directories:[base] in
    Alcotest.(check (list string)) "names preserved through deduplicate"
      ["books/rust"; "books/sqlthw"] (names_of ps))

(* Initialize a real git repo with a fake origin remote so get_remote_url
   has something to find. Required for tests that exercise the dedup
   rename-by-remote path. *)
let make_git_repo_with_remote path ~remote_url =
  mkdir_p path;
  let run cmd =
    let exit_code = Sys.command (Printf.sprintf "%s 2>/dev/null" cmd) in
    if exit_code <> 0 then
      Alcotest.failf "setup command failed (%d): %s" exit_code cmd
  in
  run (Printf.sprintf "git -C %s init -q --initial-branch=main"
    (Filename.quote path));
  run (Printf.sprintf "git -C %s remote add origin %s"
    (Filename.quote path) (Filename.quote remote_url))

let test_cluster_preserves_parent_with_remote () =
  (* Real bug from code review: when a clustered repo has a remote URL,
     the old dedup path renamed it to just the remote-basename and dropped
     the parent/ prefix. Preserving "books/rust-learn" instead of
     collapsing to "rust-learn" keeps the grouping context users set up. *)
  with_tmpdir (fun base ->
    make_git_repo_with_remote (Filename.concat base "books/rust")
      ~remote_url:"https://github.com/example/rust-learn.git";
    make_git_repo_with_remote (Filename.concat base "tutorials/react")
      ~remote_url:"git@github.com:example/react-from-scratch.git";
    let ps = P.discover ~base_directories:[base] in
    Alcotest.(check (list string)) "parent prefix kept, basename from remote"
      ["books/rust-learn"; "tutorials/react-from-scratch"]
      (names_of ps))

let test_symlink_loop_no_hang () =
  (* Symlink loop inside a cluster should not hang or crash. One-level
     recursion plus is_directory checks make this safe; we only recurse
     into direct children, not into symlink targets that loop back. *)
  with_tmpdir (fun base ->
    make_git_dir (Filename.concat base "cluster/real");
    let loop_src = Filename.concat base "cluster/loop" in
    Unix.symlink base loop_src;
    let ps = P.discover_in_directory base in
    (* 'cluster/real' should be found; the symlink shouldn't cause a hang.
       The symlink points to a dir that already contains cluster/ (but we
       only recurse one level, so this terminates). Either the symlink's
       target resolves to a non-git dir and is skipped, or it resolves to
       one that contains cluster/ (still one level below). We assert the
       real repo is always present and nothing crashes. *)
    Alcotest.(check bool) "real cluster repo discovered"
      true (List.mem "cluster/real" (names_of ps)))

let test_permission_error_skipped () =
  (* An unreadable subdirectory should be silently skipped, not crash
     discovery. Our is_directory/readdir calls are wrapped in
     try...with Sys_error _ -> []. *)
  with_tmpdir (fun base ->
    make_git_dir (Filename.concat base "cluster/readable");
    let blocked = Filename.concat base "cluster/blocked" in
    mkdir_p blocked;
    Unix.chmod blocked 0o000;
    let restore () =
      try Unix.chmod blocked 0o755 with Unix.Unix_error _ -> ()
    in
    Fun.protect ~finally:restore (fun () ->
      let ps = P.discover_in_directory base in
      Alcotest.(check bool) "readable cluster child discovered"
        true (List.mem "cluster/readable" (names_of ps))))

let discovery_tests = [
  Alcotest.test_case "flat repo" `Quick test_flat_repo;
  Alcotest.test_case "bare repo" `Quick test_bare_repo;
  Alcotest.test_case "cluster under non-git parent" `Quick
    test_cluster_non_git_parent;
  Alcotest.test_case "no recursion into git parent" `Quick
    test_cluster_ignored_when_parent_is_git;
  Alcotest.test_case "mixed top level" `Quick test_mixed_top_level;
  Alcotest.test_case "cluster cap skips oversized" `Quick test_cluster_cap;
  Alcotest.test_case "cluster at cap kept" `Quick test_cluster_at_cap;
  Alcotest.test_case "missing base dir" `Quick test_missing_base_dir;
  Alcotest.test_case "file at top level" `Quick test_file_in_base_dir;
  Alcotest.test_case "file in cluster" `Quick test_file_in_cluster;
  Alcotest.test_case "names preserved through deduplicate" `Quick
    test_deduplicate_preserves_cluster_names;
  Alcotest.test_case "cluster parent preserved when child has remote" `Quick
    test_cluster_preserves_parent_with_remote;
  Alcotest.test_case "symlink loop doesn't hang discovery" `Quick
    test_symlink_loop_no_hang;
  Alcotest.test_case "unreadable subdir skipped, not crashes" `Quick
    test_permission_error_skipped;
]

let () =
  Alcotest.run "discord_project" [
    "discovery", discovery_tests;
  ]
