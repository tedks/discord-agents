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

let rec rm_rf path =
  if Sys.file_exists path then begin
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path
  end

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
]

let () =
  Alcotest.run "discord_project" [
    "discovery", discovery_tests;
  ]
