module S = Discord_agents.Host_project_scan
module H = Discord_agents.Host
let check label expected actual = Alcotest.(check bool) label true (expected = actual)
let local = H.local_host "writer"
let remote = { local with H.name = "executor"; transport = H.Ssh "executor" }
let record ?(path="/root/repo") ?(remote_url=`Null) () = `Assoc [
  "name", `String "repo"; "path", `String path; "is_bare", `Bool false; "remote_url", remote_url]
let document projects = Yojson.Safe.to_string (`Assoc ["version", `Int 1; "projects", `List projects])
let test_decode () =
  check "empty is a successful result" (Ok []) (S.decode (document []));
  let decoded = S.decode (document [record ~remote_url:(`String "git@github.com:Tedks/Repo.git") ()]) in
  check "normalized identity" (Ok [S.{name="repo";path="/root/repo";is_bare=false;repository=Some "github.com/tedks/repo"}]) decoded;
  List.iter (fun text -> check "malformed refused" (Error S.Malformed) (S.decode text))
    [""; "{}"; "[]"; "banner\n" ^ document []; "{\"version\":1,\"version\":1,\"projects\":[]}";
     document [record (); record ()]; document [record ~path:"relative" ()];
     document [record ~path:"/bad\000path" ()]; document [record ~path:"/bad\255" ()];
     document (List.init 513 (fun i -> record ~path:("/" ^ string_of_int i) ()))];
  List.iter (fun url -> check "no cross-host identity for private/local remotes" None (S.repository_key url))
    ["/srv/repo.git"; "../repo"; "file:///repo"; "https://token:secret@example.org/repo.git";
     "https://example.org/repo?token=secret"; "https://example.org/repo#secret"; "https://token:secret@github.com/org/repo";
     "https://github.com/org/repo?secret=token"; "https://example.org/.git"];
  check "arbitrary server paths retain case" (Some "ssh://git@example.org/Owner/Repo")
    (S.repository_key "ssh://git@example.org/Owner/Repo.git");
  check "scp home-relative path differs from absolute SSH URL" false
    (S.repository_key "ssh://git@example.org/Owner/Repo.git" = S.repository_key "git@example.org:Owner/Repo.git");
  check "HTTP and SSH identities conservative outside GitHub" false
    (S.repository_key "https://example.org/Owner/Repo.git" = S.repository_key "ssh://git@example.org/Owner/Repo.git")

let test_refusal_and_transport () =
  let calls = ref 0 in
  let run ~timeout_s ~max_output_bytes ~cwd host argv =
    incr calls; check "deadline" 30 timeout_s; check "cap" (512*1024) max_output_bytes;
    check "remote root" "/" cwd; check "host preserved" remote host;
    check "roots are arguments" "/missing-on-writer" (List.hd (List.rev argv));
    Ok (document []) in
  check "remote observation" (Ok []) (S.observe ~run ~host:remote ~roots:["/missing-on-writer"]);
  check "one command" 1 !calls;
  check "disabled" (Error (S.Transport Disabled))
    (S.observe ~run ~host:{remote with enabled=false} ~roots:["/"]);
  check "no roots is not a scan" (Error S.Invalid_roots) (S.observe ~run ~host:remote ~roots:[]);
  check "relative roots" (Error S.Invalid_roots) (S.observe ~run ~host:remote ~roots:["relative"]);
  check "too many roots" (Error S.Invalid_roots) (S.observe ~run ~host:remote ~roots:(List.init 33 (fun _ -> "/")));
  check "no refused commands" 1 !calls;
  let failed ~timeout_s:_ ~max_output_bytes:_ ~cwd:_ _ _ = Error (Discord_agents.Host_probe.Process Timed_out) in
  check "timeout never empty" (Error (S.Transport Timed_out)) (S.observe ~run:failed ~host:remote ~roots:["/"])

let run argv =
  let pid = Unix.create_process (List.hd argv) (Array.of_list argv) Unix.stdin Unix.stdout Unix.stderr in
  match snd (Unix.waitpid [] pid) with Unix.WEXITED 0 -> () | _ -> Alcotest.fail "fixture command failed"
let write path text = Out_channel.with_open_bin path (fun oc -> output_string oc text)
let rec remove dir =
  match (Unix.lstat dir).Unix.st_kind with
  | Unix.S_DIR -> Sys.readdir dir |> Array.iter (fun name -> remove (Filename.concat dir name)); Unix.rmdir dir
  | _ -> Unix.unlink dir
let test_real_scan () =
  let dir = Unix.realpath (Filename.temp_dir "inventory ': " "") in
  Fun.protect ~finally:(fun () -> remove dir) (fun () ->
    let base = Filename.concat dir "projects" in Unix.mkdir base 0o700;
    let repo = Filename.concat base "repo" and bare = Filename.concat base "bare" in
    run ["git";"init";"-q";repo]; run ["git";"init";"-q";"--bare";bare];
    run ["git";"-C";repo;"remote";"add";"origin";"git@github.com:Tedks/Repo.git"];
    Unix.symlink repo (Filename.concat base "alias");
    let group = Filename.concat base "group" in Unix.mkdir group 0o700;
    run ["git";"init";"-q";Filename.concat group "nested"];
    let ssh = Filename.concat dir "ssh" in
    write ssh ("#!/bin/sh\nexec python3 -I " ^ Filename.quote (Filename.concat (Sys.getcwd ()) (if Sys.file_exists "fake_ssh.py" then "fake_ssh.py" else "test/fake_ssh.py")) ^ " \"$@\"\n");
    Unix.chmod ssh 0o755;
    Eio_main.run (fun env ->
      let clock = Eio.Stdenv.mono_clock env in
      let scan host roots = S.collect ~clock ~ssh ~host ~roots () in
      List.iter (fun host ->
        match scan host [base] with
        | Error _ -> Alcotest.fail "fixture scan failed"
        | Ok projects ->
          check "aliases deduplicated; bare and nested retained" 3 (List.length projects);
          check "bare metadata" true (List.exists (fun (p:S.project) -> p.path=bare && p.is_bare) projects);
          check "origin identity" true (List.exists (fun (p:S.project) -> p.path=repo && p.repository=Some "github.com/tedks/repo") projects);
          check "missing root fails whole scan" true (Result.is_error (scan host [base; dir ^ "/missing"]))) [local;remote];
      run ["git";"-C";repo;"-c";"user.email=test@example.invalid";"-c";"user.name=Fixture";
           "-c";"commit.gpgsign=false";"commit";"-q";"--allow-empty";"-m";"fixture"];
      let linked = Filename.concat base "linked" in
      run ["git";"-C";repo;"worktree";"add";"-q";linked];
      (match scan local [base] with
       | Ok ps -> check "gitfile worktree supported" true (List.exists (fun (p:S.project) -> p.path=linked) ps)
       | Error _ -> Alcotest.fail "gitfile scan failed");
      run ["git";"init";"-q";base];
      let partial = Filename.concat base "partial" in Unix.mkdir partial 0o700;
      Unix.mkdir (Filename.concat partial ".git") 0o700;
      check "broken .git directory cannot borrow parent repo" true (Result.is_error (scan local [base]));
      Unix.rmdir (Filename.concat partial ".git"); Unix.rmdir partial;
      check "scan succeeds after partial fixture removed" true (Result.is_ok (scan local [base]));
      let private_repo = Filename.concat group "private" in
      run ["git";"init";"-q";private_repo];
      if Unix.geteuid () <> 0 then begin
        Unix.chmod private_repo 0;
        Fun.protect ~finally:(fun () -> Unix.chmod private_repo 0o700) (fun () ->
          check "unsearchable nested repo fails whole scan" true (Result.is_error (scan local [base])))
      end;
      check "scan succeeds after permissions restored" true (Result.is_ok (scan local [base]));
      let old_path = Sys.getenv "PATH" in
      let actual_git = Option.get (Discord_agents.Resource.executable_in_path "git") in
      let bin = Filename.temp_dir ~temp_dir:"/tmp" "inventory-owner-" "" in
      let wrapper = Filename.concat bin "git" in
      write wrapper ("#!/bin/sh\nGIT_TEST_ASSUME_DIFFERENT_OWNER=1 exec " ^ Filename.quote actual_git ^
        " -c safe.directory= \"$@\"\n");
      Unix.chmod wrapper 0o755;
      Fun.protect ~finally:(fun () -> Unix.putenv "PATH" old_path; Unix.unlink wrapper; Unix.rmdir bin) (fun () ->
        Unix.putenv "PATH" (bin ^ ":" ^ old_path);
        check "Git dubious ownership policy remains enforced" true (Result.is_error (scan local [base])));
      check "scan succeeds after owner fixture removed" true (Result.is_ok (scan local [base]));
      check "base repo does not list its .git internals" (Ok []) (scan local [repo]);
      let empty = Filename.concat dir "empty" in Unix.mkdir empty 0o700;
      check "successful empty scan" (Ok []) (scan local [empty]);
      check "unreachable" (Error (S.Transport Unreachable)) (scan {remote with transport=H.Ssh "unreachable"} [base]);
      write (Filename.concat empty "file") "not a repository";
      check "ordinary files ignored" (Ok []) (scan local [empty]);
      let bad = Filename.concat base "bad" in Unix.mkdir bad 0o700;
      write (Filename.concat bad ".git") "not a gitfile";
      check "invalid git metadata not partial success" true (Result.is_error (scan local [base]));
      Unix.unlink (Filename.concat bad ".git"); Unix.rmdir bad;
      for i=1 to 4097 do write (Filename.concat empty (string_of_int i)) "" done;
      check "entry cap fails rather than truncates" true (Result.is_error (scan local [empty]))))
let () = Alcotest.run "Host project scan" ["scan", [
  Alcotest.test_case "strict decoder and repository identity" `Quick test_decode;
  Alcotest.test_case "refusal and transport contract" `Quick test_refusal_and_transport;
  Alcotest.test_case "local and fake-SSH discovery" `Quick test_real_scan;
]]
