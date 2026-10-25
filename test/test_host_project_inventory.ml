module I = Discord_agents.Host_project_inventory
module S = Discord_agents.Host_project_scan
module H = Discord_agents.Host
let host = H.local_host "writer"
let roots = ["/projects"]
let project = S.{name="repo";path="/projects/repo";is_bare=false;repository=Some "github.com/org/repo"}
let check label expected actual = Alcotest.(check bool) label true (expected=actual)
let get = function Ok x -> x | Error e -> Alcotest.fail e
let record state now result = get (I.record state ~now ~host ~roots result)
let view state now = I.view state ~now ~host ~roots
let test_history () =
  check "unseen" "unknown" (view I.empty 1.).state;
  let good = record I.empty 10. (Ok [project]) in
  check "fresh" "observed" (view good 11.).state;
  check "old evidence" "stale" (view good 400.).state;
  check "clock rollback" "stale" (view good 9.).state;
  let failed = record good 20. (Error (S.Transport Unreachable)) in
  let offline = view failed 21. in
  check "offline is not empty" [project] offline.projects;
  check "last successful timestamp retained" (Some 10.) offline.last_success_at;
  check "last attempt recorded" (Some 20.) offline.attempted_at;
  check "failure not live availability" "unavailable" offline.state;
  let aged = view failed 400. in
  check "failed observation ages" "stale" aged.state;
  check "aged failure retains projects" [project] aged.projects;
  check "aged failure retains reason" offline.error aged.error;
  let empty = record failed 30. (Ok []) in
  check "successful empty replaces" [] (view empty 31.).projects;
  check "empty is observed" "observed" (view empty 31.).state;
  check "disabled hides observations" "disabled" (I.view good ~now:11. ~host:{host with enabled=false} ~roots).state;
  check "changed roots invalidated" [] (I.view good ~now:11. ~host ~roots:["/other"]).projects;
  let changed = {host with transport=H.Ssh "new-destination"} in
  check "changed destination invalidated" "configuration_changed" (I.view good ~now:11. ~host:changed ~roots).state;
  let changed_failure = get (I.record good ~now:20. ~host:changed ~roots (Error S.Malformed)) in
  check "failure never relabels old config data" [] (I.view changed_failure ~now:21. ~host:changed ~roots).projects;
  let second = {host with name="other"} in
  let both = get (I.record good ~now:20. ~host:second ~roots (Ok [project])) in
  check "same paths on different hosts retained" 2 (List.length both);
  let renamed = get (I.record good ~now:20. ~host:{host with name="WRITER"} ~roots (Ok [])) in
  check "case change replaces host key" 1 (List.length renamed);
  check "invalid clock refused" true (Result.is_error (I.record good ~now:nan ~host ~roots (Ok [])))
let test_disk () =
  let dir = Filename.temp_dir "host-inventory-store" "" in
  let path = Filename.concat dir "cache.json" in
  Fun.protect ~finally:(fun () ->
    Sys.readdir dir |> Array.iter (fun name -> Unix.unlink (Filename.concat dir name)); Unix.rmdir dir) (fun () ->
    check "missing file is unknown inventory" (Ok []) (I.load path);
    let good = record I.empty 10. (Ok [project]) in
    check "roundtrip" (Ok good) (I.decode (I.encode good));
    let invalid = [{(List.hd good) with I.error=Some (String.make 65 'x')}] in
    check "bounded failure codes" true (Result.is_error (I.decode (I.encode invalid)));
    ignore (get (I.save path good));
    check "restart reload" (Ok good) (I.load path);
    check "private cache permissions" 0o600 ((Unix.stat path).st_perm land 0o777);
    let failed = record good 20. (Error S.Malformed) in
    ignore (get (I.save path failed));
    check "offline survives restart" [project] (view (get (I.load path)) 21.).projects;
    check "parent file refused" true (Result.is_error (I.save (Filename.concat path "child") good));
    let denied _ _ = raise (Unix.Unix_error (Unix.ENOSPC,"write","SECRET")) in
    check "write error reported" true (Result.is_error (I.save ~write:denied path good));
    check "prior snapshot intact" (Ok failed) (I.load path);
    let unconfirmed _ _ = raise (Discord_agents.Resource.Durable_write_visible_but_unconfirmed (path,Failure "SECRET")) in
    check "uncertain durability not success" true (Result.is_error (I.save ~write:unconfirmed path good));
    List.iter (fun text ->
      Out_channel.with_open_bin path (fun oc -> output_string oc text);
      check "corrupt cache refused" true (Result.is_error (I.load path)))
      [""; "{}"; "{\"version\":1,\"version\":1,\"entries\":[]}"; "[]"];
    Unix.unlink path; Unix.mkfifo path 0o600;
    check "FIFO refused without blocking" true (Result.is_error (I.load path));
    Unix.unlink path;
    let fd=Unix.openfile path [Unix.O_CREAT;Unix.O_WRONLY] 0o600 in
    Unix.ftruncate fd (I.max_file_bytes+1); Unix.close fd;
    check "oversized file refused" true (Result.is_error (I.load path)))
let test_bounds_and_corruption () =
  let good=record [] 10. (Ok [project]) in
  let large=List.init 100 (fun i -> {project with S.name=String.make 2048 'a';path="/repo/" ^ string_of_int i}) in
  let oversized=record good 20. (Ok large) in
  check "oversized snapshot becomes failed attempt" (Some "snapshot byte limit exceeded") (view oversized 21.).error;
  check "oversized snapshot retains prior success" [project] (view oversized 21.).projects;
  let second={host with H.name="second"} in
  let both=get (I.record oversized ~now:30. ~host:second ~roots (Ok [])) in
  check "other hosts continue" 2 (List.length both);
  check "oversized attempt reloads" (Ok both) (I.decode (I.encode both));
  let churn=ref [] in
  for i=0 to 70 do
    let h={host with H.name="host" ^ string_of_int i} in
    churn := get (I.record (I.prune !churn ~configured:[h]) ~now:100. ~host:h ~roots (Ok []))
  done;
  check "removed host churn bounded" 1 (List.length !churn);
  let full=List.init 64 (fun i -> {(List.hd good) with I.host={host with name="h" ^ string_of_int i}}) in
  check "host cap explicit" true (Result.is_error (I.record full ~now:30. ~host:second ~roots (Ok [])));
  let missing=record [] 10. (Error S.Malformed) in
  check "failure without prior success reloads" (Ok missing) (I.decode (I.encode missing));
  let reasons : Discord_agents.Host_capability.unknown list =
    [Disabled;Invalid_context;Missing_executable;Unreachable;Timed_out;Output_limit;Setup_failed;Io_failed;
     Command_failed;Malformed;Not_supported;Executable_changed;Stale;Context_changed] in
  List.iter (fun reason -> let state=record good 20. (Error (S.Transport reason)) in
    check "all failure labels decode" (Ok state) (I.decode (I.encode state))) reasons;
  let replace key value = function
    | `Assoc xs -> `Assoc ((key,value)::List.remove_assoc key xs) | _ -> assert false in
  let entry=I.entry_json (List.hd good) in
  let success=I.success_json (Option.get (List.hd good).I.success) in
  let document entry=Yojson.Safe.to_string (`Assoc ["version",`Int 1;"entries",`List [entry]]) in
  let malformed = [
    replace "host" (`Int 5) entry; replace "host" (`Assoc []) entry;
    replace "roots" (`String "/projects") entry; replace "host_name" (`String "bad\255") entry;
    replace "success" (replace "at" (`Float 30.) success) entry;
    replace "success" (replace "projects" (`List [replace "is_bare" (`String "yes") (I.project_json project)]) success) entry;
    (match entry with `Assoc xs -> `Assoc (("host_name",`String "duplicate")::xs) | _ -> assert false);
    replace "success" (replace "projects" (`List [
      (match I.project_json project with `Assoc xs -> `Assoc (("path",`String "/duplicate")::xs) | _ -> assert false)]) success) entry;
  ] in
  List.iter(fun bad -> check "nested corruption returns Error" true (Result.is_error (I.decode (document bad)))) malformed
let () = Alcotest.run "Host inventory snapshots" ["snapshots", [
  Alcotest.test_case "bounds, churn and nested corruption" `Quick test_bounds_and_corruption;
  Alcotest.test_case "state transitions and contexts" `Quick test_history;
  Alcotest.test_case "durable reload and write/read failures" `Quick test_disk;
]]
