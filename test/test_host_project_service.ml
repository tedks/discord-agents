module S = Discord_agents.Host_project_service
module I = Discord_agents.Host_project_inventory
module Scan = Discord_agents.Host_project_scan
module H = Discord_agents.Host
let check label expected actual = Alcotest.(check bool) label true (expected=actual)
let get = function Ok x -> x | Error e -> Alcotest.fail e
let local = H.local_host "writer"
let remote = {local with H.name="remote";transport=H.Ssh "remote"}
let disabled = {remote with H.name="off";enabled=false}
let registry = Some H.{writer="writer";hosts=[local;remote;disabled]}
let roots = ["/projects"]
let project ?(repository=Some "github.com/org/repo") path = Scan.{name="repo";path;is_bare=false;repository}
let with_store f =
  let dir=Filename.temp_dir "project-service-" "" in
  let path=Filename.concat dir "cache.json" in
  Fun.protect ~finally:(fun () ->
    Sys.readdir dir |> Array.iter (fun n -> Unix.unlink (Filename.concat dir n)); Unix.rmdir dir)
    (fun () -> f (S.create path))
let refresh ?(save=fun path entries -> I.save path entries) ?(preflight=fun _ -> Ok ()) ~collect store name =
  S.refresh ~save ~preflight ~collect ~now:(fun () -> 100.) ~local_name:"writer" ~registry
    ~base_directories:roots store name
let listing ?(offset=0) ?(limit=8) store = get (S.listing ~now:101. ~local_name:"writer" ~registry
  ~base_directories:roots ~offset ~limit store)
let member = Yojson.Safe.Util.member
let int name json = member name json |> Yojson.Safe.Util.to_int
let record entries host ps = get (I.record entries ~now:100. ~host ~roots (Ok ps))
let test_refresh () = with_store (fun store ->
  let calls=ref 0 in
  let collect ~host ~roots:rs = incr calls;check "executor selected" remote host;
    check "configured roots" roots rs; Ok [project "/executor/repo"] in
  List.iter (fun name -> check "refused" true (Result.is_error (refresh ~collect store name))) ["missing";"off"];
  check "no refused probes" 0 !calls;
  check "disk preflight refuses" true (Result.is_error (refresh ~preflight:(fun _ -> Error "no space") ~collect store "remote"));
  check "no disk-refused probe" 0 !calls;
  store.busy <- true;
  check "one at a time" true (Result.is_error (refresh ~collect store "remote")); store.busy <- false;
  let response=get (refresh ~collect store "remote") in
  check "scan success" (`Bool true) (member "scan_succeeded" response);
  check "snapshot persisted" (Ok store.entries) (I.load store.path);
  let prior=store.entries in
  let failing ~host:_ ~roots:_ = Error (Scan.Transport Unreachable) in
  let response=get (refresh ~collect:failing store "remote") in
  check "failure explicit" (`Bool false) (member "scan_succeeded" response);
  check "offline retained" 1 (int "total_locations" (listing (S.create store.path)));
  let large ~host:_ ~roots:_ = Ok (List.init 100 (fun i ->
    {(project ("/large/" ^ string_of_int i)) with Scan.name=String.make 2048 'a'})) in
  let too_large=get (refresh ~collect:large store "remote") in
  check "oversized observation not reported successful" (`Bool false) (member "scan_succeeded" too_large);
  check "oversized observation keeps prior locations" 1 (int "total_locations" (listing store));
  let saved=store.entries in
  let denied _ _ = Error "save failed" in
  check "save failure returned" true (Result.is_error (refresh ~save:denied ~collect store "remote"));
  check "memory unchanged on save failure" saved store.entries;
  check "cache failure visible" (`String "save failed") (member "cache_error" (listing store));
  let nested ~host:_ ~roots:_ =
    check "concurrent reentry refused" true (Result.is_error (refresh ~collect store "remote")); Ok [] in
  ignore(get(refresh ~collect:nested store "remote"));
  check "successful empty replaces" 0 (int "total_locations" (listing store));
  check "prior result immutable" 1 (List.length (Option.get (List.hd prior).I.success).projects);
  let raised ~host:_ ~roots:_ = raise Exit in
  (try ignore(refresh ~collect:raised store "remote"); Alcotest.fail "exception missing" with Exit -> ());
  check "slot released on exception" false store.busy;
  let too_many=Some H.{writer="writer";hosts=List.init 65(fun i -> H.local_host ("h"^string_of_int i))} in
  check "host bound" true (Result.is_error (S.listing ~now:101. ~local_name:"writer" ~registry:too_many
    ~base_directories:roots ~offset:0 ~limit:8 store)))
let test_load_recovery () = with_store (fun store ->
  let initial=record (record [] local [project "/local/repo"]) remote [project "/remote/repo"] in
  ignore(get(I.save store.path initial));
  let recovering=S.create ~load:(fun _ -> Error "injected transient read failure") store.path in
  check "failed load identified separately" true recovering.load_failed;
  let calls=ref 0 in
  let collect ~host:_ ~roots:_ = incr calls; Ok [project "/remote/new"] in
  ignore(get(refresh ~collect recovering "remote"));
  check "load retried before one scan" 1 !calls;
  check "other host preserved through recovery" 2 (int "total_locations" (listing recovering));
  check "other host exact data preserved" (I.find initial local) (I.find recovering.entries local);
  check "recovery durable" (Ok recovering.entries) (I.load store.path);
  check "load warning cleared after recovery" false recovering.load_failed;
  check "public warning cleared after recovery" None recovering.cache_error;
  Out_channel.with_open_bin store.path (fun oc -> output_string oc "broken");
  let broken=S.create store.path in
  calls := 0;
  check "unreadable cache blocks refresh" true (Result.is_error (refresh ~collect broken "remote"));
  check "unreadable cache prevents scan" 0 !calls;
  check "unreadable cache warning persists" true (Option.is_some broken.cache_error);
  check "slot released after refused recovery" false broken.busy;
  check "bad file preserved for repair" "broken"
    (In_channel.with_open_bin store.path In_channel.input_all);
  ignore(get(I.save store.path initial));
  ignore(get(refresh ~collect broken "remote"));
  check "repair recovers every host" 2 (int "total_locations" (listing broken)))
let test_listing () = with_store (fun store ->
  let ps = [project "/projects/a"; project "/projects/b";project ~repository:None "/projects/local"] in
  store.entries <- record (record [] local ps) remote ps;
  let full=listing store in
  check "network identity grouped, local kept separate" 3 (int "total_repositories" full);
  check "all locations retained" 6 (int "total_locations" full);
  let first=listing ~limit:2 store and second=listing ~offset:2 ~limit:2 store in
  check "page one" (`Int 2) (member "next_offset" first);
  check "page two" (`Int 4) (member "next_offset" second);
  check "last page" `Null (member "next_offset" (listing ~offset:4 ~limit:2 store));
  check "huge offset no overflow" (`List []) (member "groups" (listing ~offset:max_int store));
  check "invalid page refused" true (Result.is_error (S.listing ~now:101. ~local_name:"writer" ~registry
    ~base_directories:roots ~offset:0 ~limit:9 store));
  let changed=Some H.{writer="writer";hosts=[local;{remote with transport=H.Ssh "elsewhere"}]} in
  let changed=get (S.listing ~now:101. ~local_name:"writer" ~registry:changed ~base_directories:roots ~offset:0 ~limit:8 store) in
  check "changed context hides old locations" 3 (int "total_locations" changed);
  let forced=get (S.listing ~now:101. ~local_name:"new-writer" ~registry ~base_directories:roots ~offset:0 ~limit:8 store) in
  let hs=member "hosts" forced |> Yojson.Safe.Util.to_list in
  check "old writer explicitly unroutable" (`String "unroutable")
    (member "state" (List.find(fun h -> member "host" h = `String "writer") hs));
  let hostile=project ("/projects/\n@everyone\226\128\174") in
  store.entries <- record [] local [{hostile with name="fake\nline"}];
  let report=member "report" (listing store) |> Yojson.Safe.Util.to_string in
  check "display cannot mention everyone" false (Discord_agents.Resource.contains_substring ~haystack:report ~needle:"@everyone");
  check "display cannot forge line" false (Discord_agents.Resource.contains_substring ~haystack:report ~needle:"fake\nline");
  let huge=String.make 4096 '\001' in
  let huge_path="/" ^ String.sub huge 1 4094 in
  let hs=List.init 8 (fun i -> {remote with H.name="h" ^ string_of_int i}) in
  let entries=List.fold_left (fun entries h ->
    record entries h [Scan.{name=huge;path=huge_path;is_bare=false;repository=Some (String.sub huge 0 4094 ^ h.H.name)}]) [] hs in
  check "large fixture valid cache" (Ok entries) (I.decode (I.encode entries));
  store.entries <- entries;
  let registry=Some H.{writer="h0";hosts=hs} in
  let large=get (S.listing ~now:101. ~local_name:"writer" ~registry ~base_directories:roots ~offset:0 ~limit:8 store) in
  check "large fixture not empty" 8 (int "total_locations" large);
  check "response bounded below 1 MiB client cap" true
    (String.length (Yojson.Safe.to_string large) < 1024*1024);
  Out_channel.with_open_bin store.path (fun oc -> output_string oc "broken");
  check "corrupt cache warning preserved" true (Option.is_some (S.create store.path).cache_error))
let () = Alcotest.run "Host project service" ["inventory",[
  Alcotest.test_case "cache load recovery preserves other hosts" `Quick test_load_recovery;
  Alcotest.test_case "refresh provenance, refusal and atomic publication" `Quick test_refresh;
  Alcotest.test_case "grouped paginated observations and display" `Quick test_listing;
]]
