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
  bad (String.make 65 'a');
  ok (String.make 64 'a')

let test_validate_destination () =
  let ok d =
    Alcotest.(check bool) d true
      (Result.is_ok (Host.validate_destination ~field:"ssh" d)) in
  let bad d =
    Alcotest.(check bool) d false
      (Result.is_ok (Host.validate_destination ~field:"ssh" d)) in
  ok "framework0"; ok "tedks@framework0.local"; ok "192.168.1.158";
  ok "tedks@[fe80::1%eth0]"; ok "host:2222";
  bad ""; bad "-oProxyCommand=touch /tmp/x"; bad "-p22"; bad "a b";
  bad "a\tb"; bad "a\nb"; bad "a\127";
  (* writer_ssh ends up as a word a remote login shell parses *)
  bad "tower0$(touch /tmp/x)"; bad "a;b"; bad "`id`"; bad "a|b"; bad "a&b";
  bad "a'b"; bad "a\"b"; bad "a>b"; bad "a/b"

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
  check "prefix is not a match" false "tower" "tower0";
  check "case-insensitive" true "Tower0" "tower0.LOCAL";
  (* Two qualified names sharing a first label are different machines;
     equating them would let both pass the writer guard. *)
  check "qualified names must match exactly" false
    "bot.prod.example" "bot.dev.example";
  check "identical qualified names" true "bot.prod.example" "BOT.prod.example"

let test_check_writer () =
  let r = parse {|{"writer": "tower0"}|} in
  Alcotest.(check bool) "no hosts block" true
    (Result.is_ok (Host.check_writer ~hostname:"anything" None));
  Alcotest.(check bool) "on the writer" true
    (Result.is_ok (Host.check_writer ~hostname:"tower0" (Some r)));
  (match Host.check_writer ~hostname:"framework0" (Some r) with
   | Ok () -> Alcotest.fail "expected refusal on another machine"
   | Error msg ->
     List.iter (fun needle ->
       Alcotest.(check bool) ("mentions " ^ needle) true
         (Discord_agents.Resource.contains_substring ~haystack:msg ~needle))
       ["tower0"; "framework0"; "--force-writer"; Host.force_writer_env]);
  Alcotest.(check bool) "forced" true
    (Result.is_ok (Host.check_writer ~hostname:"framework0" ~force:"--force-writer" (Some r)))

let test_more_rejections () =
  check_rejected ~label:"duplicate field" ~mentions:"given twice"
    {|{"writer": "tower0", "writer": "framework0"}|};
  check_rejected ~label:"duplicate host field" ~mentions:"given twice"
    {|{"writer": "tower0",
       "executors": {"x": {"ssh": "x", "ssh": "-oProxyCommand=y"}}}|};
  check_rejected ~label:"same first label" ~mentions:"same host"
    {|{"writer": "tower0",
       "executors": {"fw": {"ssh": "fw"}, "FW.lan": {"ssh": "fw2"}}}|};
  check_rejected ~label:"option-like base dir" ~mentions:"base_directories"
    {|{"writer": "tower0",
       "executors": {"x": {"ssh": "x", "base_directories": ["-rf"]}}}|};
  check_rejected ~label:"metachar destination" ~mentions:"may contain only"
    {|{"writer": "tower0",
       "executors": {"x": {"ssh": "x", "writer_ssh": "t$(id)"}}}|}

let test_qualified_writer_key_matches_short_executor () =
  (* hosts.writer spelled with a domain still finds the local entry. *)
  let r = parse {|{"writer": "tower0.local",
                   "executors": {"tower0": {"local": true}}}|} in
  Alcotest.(check (list string)) "no duplicate writer" ["tower0"] (names r);
  Alcotest.(check bool) "find by short name" true
    (Option.is_some (Host.find r "tower0"));
  Alcotest.(check bool) "find by qualified name" true
    (Option.is_some (Host.find r "TOWER0.local"))

let test_qualified_hosts_sharing_a_label () =
  let r = parse {|{"writer": "bot.prod.example",
                   "executors": {"bot.dev.example": {"ssh": "bot-dev"}}}|} in
  Alcotest.(check (list string)) "both kept"
    ["bot.prod.example"; "bot.dev.example"] (names r);
  let found n = Option.map (fun (h : Host.t) -> h.name) (Host.find r n) in
  Alcotest.(check (option string)) "exact prod" (Some "bot.prod.example")
    (found "BOT.prod.example");
  Alcotest.(check (option string)) "exact dev" (Some "bot.dev.example")
    (found "bot.dev.example");
  Alcotest.(check (option string)) "short name is ambiguous" None (found "bot");
  (* Forced onto the dev machine: recorded as dev, not as a bare "bot"
     that could resolve to prod. *)
  Alcotest.(check (result string string)) "forced on dev"
    (Ok "bot.dev.example")
    (Host.session_host_name ~hostname:"bot.dev.example" (Some r));
  let both = parse {|{"writer": "w", "executors": {
                       "bot.prod.example": {"ssh": "a"},
                       "bot.dev.example": {"ssh": "b"}}}|} in
  Alcotest.(check int) "two qualified executors" 3 (List.length both.hosts);
  (* A bare "bot" hostname cannot say which of the two it is: refuse,
     forced or not, rather than record a name that could resolve to the
     other machine. *)
  Alcotest.(check bool) "ambiguous hostname refused for stamping" false
    (Result.is_ok (Host.session_host_name ~hostname:"bot" (Some r)));
  Alcotest.(check bool) "ambiguous hostname refused by the guard" false
    (Result.is_ok (Host.check_writer ~hostname:"bot" (Some r)));
  Alcotest.(check bool) "ambiguous even when forced" false
    (Result.is_ok (Host.startup_check ~hostname:"bot" ~force:"test" (Some r)));
  check_rejected ~label:"writer matching two executors" ~mentions:"more than one"
    {|{"writer": "tower0", "executors": {
         "tower0.local": {"ssh": "a"}, "tower0.lan": {"ssh": "b"}}}|}

let test_this_machine_name () =
  let name hostname = Host.this_machine_name ~hostname () in
  Alcotest.(check (result string string)) "lowercased"
    (Ok "tower0") (name "Tower0");
  (* The whole name: two FQDNs sharing a first label are two machines. *)
  Alcotest.(check (result string string)) "qualified kept whole"
    (Ok "bot.dev.example") (name "Bot.Dev.Example");
  Alcotest.(check bool) "too long" false
    (Result.is_ok (name (String.make 65 'h')));
  Alcotest.(check bool) "unusable characters" false
    (Result.is_ok (name "bad host"))

let test_session_host_name () =
  let r = parse {|{"writer": "tower0",
                   "executors": {"framework0": {"ssh": "framework0"}}}|} in
  let stamp ?registry hostname =
    Host.session_host_name ~hostname registry in
  Alcotest.(check (result string string)) "single host"
    (Ok "tower0") (stamp "tower0");
  Alcotest.(check (result string string)) "single host, qualified hostname"
    (Ok "tower0.local") (stamp "tower0.local");
  Alcotest.(check (result string string)) "on the writer"
    (Ok "tower0") (stamp ~registry:r "tower0.local");
  (* Forced onto another machine: sessions really run there, so they
     must not be recorded as the configured writer's. *)
  Alcotest.(check (result string string)) "forced elsewhere"
    (Ok "framework0") (stamp ~registry:r "framework0");
  let q = parse {|{"writer": "tower0.local"}|} in
  Alcotest.(check (result string string)) "registry spelling kept"
    (Ok "tower0.local") (stamp ~registry:q "tower0");
  Alcotest.(check bool) "unusable hostname" false
    (Result.is_ok (stamp (String.make 70 'x')))

let test_force_writer_source () =
  let src ?(args = []) env_value = Host.force_writer_source ~args ~env_value in
  Alcotest.(check (option string)) "flag" (Some "--force-writer")
    (src ~args:["--force-writer"] None);
  List.iter (fun v ->
    Alcotest.(check bool) ("env " ^ v) true (Option.is_some (src (Some v))))
    ["1"; "true"; "YES"; " on "];
  List.iter (fun v ->
    Alcotest.(check bool) ("env " ^ v) false (Option.is_some (src (Some v))))
    [""; "0"; "false"; "no"];
  Alcotest.(check (option string)) "unset" None (src None)

let test_startup_check () =
  let r = parse {|{"writer": "tower0"}|} in
  Alcotest.(check (result string string)) "writer" (Ok "tower0")
    (Host.startup_check ~hostname:"tower0" (Some r));
  Alcotest.(check bool) "refused" false
    (Result.is_ok (Host.startup_check ~hostname:"framework0" (Some r)));
  Alcotest.(check (result string string)) "forced" (Ok "framework0")
    (Host.startup_check ~hostname:"framework0" ~force:"test" (Some r));
  Alcotest.(check bool) "bad hostname, no registry" false
    (Result.is_ok (Host.startup_check ~hostname:"bad host" None))

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
      Alcotest.test_case "more rejections" `Quick test_more_rejections;
      Alcotest.test_case "qualified writer key matches short executor" `Quick
        test_qualified_writer_key_matches_short_executor;
      Alcotest.test_case "qualified hosts sharing a label" `Quick
        test_qualified_hosts_sharing_a_label;
      Alcotest.test_case "this machine name" `Quick test_this_machine_name;
      Alcotest.test_case "session host name" `Quick test_session_host_name;
      Alcotest.test_case "force writer source" `Quick test_force_writer_source;
      Alcotest.test_case "startup check" `Quick test_startup_check;
    ];
  ]
