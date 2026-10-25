module D = Discord_agents.Host_diagnostics
module H = Discord_agents.Host
module C = Discord_agents.Host_capability
module Callback = Discord_agents.Host_callback
let check label expected actual = Alcotest.(check bool) label true (expected = actual)
let contains text needle = Discord_agents.Resource.contains_substring ~haystack:text ~needle
let writer = H.local_host "writer"
let remote = { writer with H.name = "remote"; transport = H.Ssh "executor-alias";
  writer_ssh = Some "writer-alias" }
let disabled = { remote with H.name = "hands-off"; enabled = false }
let registry = Some { H.writer = "writer"; hosts = [writer; remote; disabled] }
let report json = Yojson.Safe.Util.(json |> member "report" |> to_string)

let test_routing () =
  check "single host" [writer] (D.hosts ~local_name:"writer" None);
  check "configured SSH exact" (Ok remote) (D.resolve ~local_name:"writer" ~registry "remote");
  List.iter (fun name -> check "refused" true
    (Result.is_error (D.resolve ~local_name:"writer" ~registry name))) ["hands-off"; "unknown"];
  let forced = D.hosts ~local_name:"remote" registry in
  check "actual writer becomes local" true (H.is_local (Option.get (H.lookup forced "remote")));
  check "old writer lacks a route" true (Result.is_error (D.resolve ~local_name:"remote" ~registry "writer"));
  check "unlisted forced writer" true (H.is_local (List.hd (D.hosts ~local_name:"elsewhere" registry)));
  let inventory = report (D.inventory ~local_name:"writer" registry) in
  List.iter (fun expected -> check expected true (contains inventory expected))
    ["no probes run"; "disabled; not contacted"; "reachability unknown"];
  let ambiguous = Some { H.writer = "writer"; hosts = [writer;
    {remote with name = "remote.a"}; {remote with name = "remote.b"}] } in
  check "ambiguous short name" true
    (Result.is_error (D.resolve ~local_name:"writer" ~registry:ambiguous "remote"))

let test_display () =
  let raw = "/a\n`@everyone_*" ^ "\226\128\174" ^ "é" ^ "\255" in
  let text = D.display raw in
  check "ASCII only" true (String.for_all (fun c -> c >= ' ' && c < '\127') text);
  check "no mention" false (contains text "@everyone");
  check "visible bidi" true (contains text "[U+202E]");
  check "visible invalid bytes" true (contains text "[invalid UTF-8]");
  check "markdown escaped" true (contains text "\\`[U+0040]everyone\\_\\*");
  check "bounded display" true (String.ends_with ~suffix:" [truncated]" (D.display (String.make 600 'a')));
  check "does not mutate paths" "/a\n`@everyone_*\226\128\174é\255" raw

let test_gate () =
  let gate = D.create_gate () in
  check "nested request rejected" true (Result.is_ok (D.with_slot gate (fun () ->
    check "busy" true (Result.is_error (D.with_slot gate (fun () -> assert false))))));
  (try ignore (D.with_slot gate (fun () -> failwith "cancelled")) with Failure _ -> ());
  check "slot released after failure" (Ok 42) (D.with_slot gate (fun () -> 42));
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.mono_clock env in
    let result = D.with_deadline ~clock ~seconds:0.01 (fun () ->
      D.with_slot gate (fun () -> Eio.Time.Mono.sleep clock 10.)) in
    check "cancelled" true (Result.is_error result);
    check "slot released after Eio cancellation" (Ok ()) (D.with_slot gate (fun () -> ())))

let test_observation () =
  let calls = ref [] in
  let run ~timeout_s ~max_output_bytes ~cwd host argv =
    check "executor and cwd preserved" (remote, "/project") (host, cwd);
    check "per-probe bounds" true (timeout_s <= 8 && max_output_bytes <= 65536);
    calls := argv :: !calls;
    if List.mem "capability-identity" argv then Ok "/cli/agy\000stamp\n"
    else if List.mem "--version" argv then Ok "version-SECRET\226\128\174\n"
    else if List.mem "--help" argv then Ok "Usage: agy [options]\nOptions:\n  --help  help\n"
    else begin
      check "reverse route" true (List.mem "writer-alias" argv);
      check "explicit writer socket" true (contains (List.hd (List.rev argv)) "'/chosen/control.sock'");
      Ok Callback.reply
    end in
  let get now = D.diagnose ~run ~now ~host:remote ~cwd:"/project" ~agent:Discord_agents.Config.Agy
    ~writer:"writer" ~mcp_path:(Some "/chosen/mcp") ~socket_path:"/chosen/control.sock" |> report in
  let text = get (fun () -> 100.) in
  List.iter (fun expected -> check expected true (contains text expected))
    ["CLI credential store: unknown (no advertised probe)"; "MCP control callback: reachable";
     "Model access: unknown"; "[U+202E]"; "--help"];
  check "no auth or model invocation" false
    (List.exists (fun argv -> List.exists (fun x -> x = "login" || x = "auth" || x = "-p") argv) !calls);
  let times = ref [100.; 100.; 200.] in
  let now () = match !times with t::rest -> times := rest; t | [] -> 200. in
  let stale = get now in
  check "stale version withheld" false (contains stale "version-SECRET");
  check "stale callback" true (contains stale "MCP control callback: unknown (stale)");
  let run ~timeout_s:_ ~max_output_bytes:_ ~cwd:_ _ _ =
    Error (Discord_agents.Host_probe.Host (Discord_agents.Host_exec.Unreachable "SECRET")) in
  let failed = D.diagnose ~run ~now:(fun () -> 100.) ~host:remote ~cwd:"/project"
    ~agent:Discord_agents.Config.Claude ~writer:"writer" ~mcp_path:(Some "/mcp")
    ~socket_path:"/socket" |> report in
  check "transport failure remains unknown" true (contains failed "unknown (transport unreachable)");
  check "raw failure discarded" false (contains failed "SECRET")

let () = Alcotest.run "Host diagnostics" ["diagnostics", [
  Alcotest.test_case "routing and inventory" `Quick test_routing;
  Alcotest.test_case "safe display" `Quick test_display;
  Alcotest.test_case "concurrency and cancellation" `Quick test_gate;
  Alcotest.test_case "fresh observations and callback route" `Quick test_observation;
]]
