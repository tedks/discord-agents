module Host = Discord_agents.Host

let parse s = Host.registry_of_yojson (Yojson.Safe.from_string s)

let parse_error s =
  match parse s with
  | _ -> None
  | exception Failure msg -> Some msg

let check_rejected ~label ~mentions s =
  match parse_error s with
  | None -> Alcotest.failf "%s: expected a parse error" label
  | Some msg ->
    if not (Discord_agents.Resource.contains_substring ~haystack:msg ~needle:mentions)
    then Alcotest.failf "%s: error %S does not mention %S" label msg mentions

let names registry = List.map (fun (h : Host.t) -> h.name) registry.Host.hosts

let test_validate_name () =
  let ok n = Alcotest.(check bool) n true (Result.is_ok (Host.validate_name n)) in
  let bad n = Alcotest.(check bool) n false (Result.is_ok (Host.validate_name n)) in
  ok "tower0"; ok "framework0"; ok "host-1.lan"; ok "a_b";
  bad ""; bad "a b"; bad "a@b"; bad "-x"; bad ".x"; bad "a/b"; bad "a\nb";
  bad (String.make 64 'a');
  ok (String.make 63 'a')

let test_validate_destination () =
  let ok d =
    Alcotest.(check bool) d true
      (Result.is_ok (Host.validate_destination ~field:"ssh" d)) in
  let bad d =
    Alcotest.(check bool) d false
      (Result.is_ok (Host.validate_destination ~field:"ssh" d)) in
  ok "framework0"; ok "tedks@framework0.local"; ok "192.168.1.158";
  bad ""; bad "-oProxyCommand=touch /tmp/x"; bad "-p22"; bad "a b";
  bad "a\tb"; bad "a\nb"; bad "a\127"

let test_minimal_registry_inserts_local_writer () =
  let r = parse {|{"writer": "tower0"}|} in
  Alcotest.(check string) "writer" "tower0" r.writer;
  Alcotest.(check (list string)) "hosts" ["tower0"] (names r);
  Alcotest.(check bool) "writer is local" true (Host.is_local (Host.writer r))

let test_full_registry () =
  let r = parse {|{
    "writer": "tower0",
    "executors": {
      "framework0": {"ssh": "framework0", "writer_ssh": "tower0",
                     "base_directories": ["~/Projects"]},
      "drynwyn": {"ssh": "drynwyn", "enabled": false},
      "tower0": {"local": true}
    }
  }|} in
  Alcotest.(check (list string)) "config order kept"
    ["framework0"; "drynwyn"; "tower0"] (names r);
  match Host.find r "framework0", Host.find r "drynwyn" with
  | Some fw, Some dr ->
    Alcotest.(check bool) "framework0 ssh" true
      (Host.equal_transport fw.transport (Host.Ssh "framework0"));
    Alcotest.(check (option string)) "writer_ssh" (Some "tower0") fw.writer_ssh;
    Alcotest.(check (option (list string))) "base dirs"
      (Some ["~/Projects"]) fw.base_directories;
    Alcotest.(check bool) "framework0 enabled by default" true fw.enabled;
    Alcotest.(check bool) "drynwyn disabled" false dr.enabled
  | _ -> Alcotest.fail "expected framework0 and drynwyn"

let test_rejections () =
  check_rejected ~label:"writer required" ~mentions:"writer"
    {|{"executors": {}}|};
  check_rejected ~label:"bad writer name" ~mentions:"hosts.writer"
    {|{"writer": "a b"}|};
  check_rejected ~label:"writer over ssh" ~mentions:"local"
    {|{"writer": "tower0", "executors": {"tower0": {"ssh": "tower0"}}}|};
  check_rejected ~label:"writer disabled" ~mentions:"cannot be disabled"
    {|{"writer": "tower0",
       "executors": {"tower0": {"local": true, "enabled": false}}}|};
  check_rejected ~label:"second local" ~mentions:"only the writer"
    {|{"writer": "tower0", "executors": {"framework0": {"local": true}}}|};
  check_rejected ~label:"no transport" ~mentions:"ssh"
    {|{"writer": "tower0", "executors": {"framework0": {}}}|};
  check_rejected ~label:"local and ssh" ~mentions:"must not set ssh"
    {|{"writer": "tower0",
       "executors": {"tower0": {"local": true, "ssh": "tower0"}}}|};
  check_rejected ~label:"option injection" ~mentions:"must not start with '-'"
    {|{"writer": "tower0",
       "executors": {"x": {"ssh": "-oProxyCommand=touch /tmp/pwned"}}}|};
  check_rejected ~label:"writer_ssh injection" ~mentions:"must not start with '-'"
    {|{"writer": "tower0",
       "executors": {"x": {"ssh": "x", "writer_ssh": "-F/tmp/cfg"}}}|};
  check_rejected ~label:"duplicate executor" ~mentions:"listed twice"
    {|{"writer": "tower0",
       "executors": {"x": {"ssh": "x"}, "x": {"ssh": "y"}}}|};
  check_rejected ~label:"bad executor name" ~mentions:"hosts.executors"
    {|{"writer": "tower0", "executors": {"x@y": {"ssh": "x"}}}|};
  check_rejected ~label:"unknown host field" ~mentions:"enabeld"
    {|{"writer": "tower0", "executors": {"x": {"ssh": "x", "enabeld": true}}}|};
  check_rejected ~label:"unknown hosts field" ~mentions:"writers"
    {|{"writer": "tower0", "writers": []}|};
  check_rejected ~label:"empty base dir" ~mentions:"base_directories"
    {|{"writer": "tower0",
       "executors": {"x": {"ssh": "x", "base_directories": [" "]}}}|};
  check_rejected ~label:"executors as list" ~mentions:"keyed by host name"
    {|{"writer": "tower0", "executors": [{"ssh": "x"}]}|}

let test_roundtrip () =
  let r = parse {|{
    "writer": "tower0",
    "executors": {
      "framework0": {"ssh": "framework0", "writer_ssh": "tower0.lan",
                     "base_directories": ["~/Projects", "/srv"]},
      "drynwyn": {"ssh": "drynwyn", "enabled": false}
    }
  }|} in
  let r' = Host.registry_of_yojson (Host.yojson_of_registry r) in
  Alcotest.(check bool) "roundtrip equal" true (Host.equal_registry r r')

let test_names_this_machine () =
  let check label expected name hostname =
    Alcotest.(check bool) label expected
      (Host.names_this_machine ~hostname name)
  in
  check "exact" true "tower0" "tower0";
  check "fqdn hostname" true "tower0" "tower0.local";
  check "fqdn name" true "tower0.local" "tower0";
  check "different" false "framework0" "tower0";
  check "prefix is not a match" false "tower" "tower0"

let test_check_writer () =
  let r = parse {|{"writer": "tower0"}|} in
  Alcotest.(check bool) "no hosts block" true
    (Result.is_ok (Host.check_writer ~hostname:"anything" ~force:false None));
  Alcotest.(check bool) "on the writer" true
    (Result.is_ok (Host.check_writer ~hostname:"tower0" ~force:false (Some r)));
  (match Host.check_writer ~hostname:"framework0" ~force:false (Some r) with
   | Ok () -> Alcotest.fail "expected refusal on another machine"
   | Error msg ->
     List.iter (fun needle ->
       Alcotest.(check bool) ("mentions " ^ needle) true
         (Discord_agents.Resource.contains_substring ~haystack:msg ~needle))
       ["tower0"; "framework0"; "--force-writer"]);
  Alcotest.(check bool) "forced" true
    (Result.is_ok (Host.check_writer ~hostname:"framework0" ~force:true (Some r)))

let () =
  Alcotest.run "host" [
    "host", [
      Alcotest.test_case "validate name" `Quick test_validate_name;
      Alcotest.test_case "validate destination" `Quick test_validate_destination;
      Alcotest.test_case "minimal registry inserts local writer" `Quick
        test_minimal_registry_inserts_local_writer;
      Alcotest.test_case "full registry" `Quick test_full_registry;
      Alcotest.test_case "rejections" `Quick test_rejections;
      Alcotest.test_case "json roundtrip" `Quick test_roundtrip;
      Alcotest.test_case "names this machine" `Quick test_names_this_machine;
      Alcotest.test_case "check writer" `Quick test_check_writer;
    ];
  ]
