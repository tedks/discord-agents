module C = Discord_agents.Host_capability
module Host = Discord_agents.Host
module Probe = Discord_agents.Host_probe
module Config = Discord_agents.Config
open C

let host = Host.local_host "writer"
let remote = { host with Host.name = "executor"; transport = Ssh "executor" }
let exited code text = Error (Probe.Host (Discord_agents.Host_exec.Exited (code, text)))
let check label expected actual = Alcotest.(check bool) label true (expected = actual)
let now () = 100.

let page agent scope flags commands =
  "Usage: " ^ Config.string_of_agent_kind agent ^ " " ^ String.concat " " scope ^ " [OPTIONS]\n\nCommands:\n" ^
  String.concat "" (List.map (fun c -> "  " ^ c ^ "  description\n") commands) ^
  "\nOptions:\n" ^ String.concat "" (List.map (fun f -> "  " ^ f ^ "  description\n") flags)

let fixture ?(agent = Config.Codex) ?(modify = fun _ result -> result) () =
  let calls = ref [] in
  let run ~timeout_s ~max_output_bytes ~cwd target argv =
    check "finite deadline" 5 timeout_s;
    Alcotest.(check bool) "bounded output" true (max_output_bytes > 0 && max_output_bytes <= 65536);
    check "cwd retained" "/project with spaces" cwd;
    calls := (target.Host.name, argv) :: !calls;
    let parent = if agent = Config.Claude then "auth" else "login" in
    let key, result = match argv with
      | ["sh"; "-c"; script; "capability-identity"; name] ->
        check "fixed identity script" C.identity_script script;
        check "only selected CLI" (Config.string_of_agent_kind agent) name;
        ["identity"], Ok "/cli path/agent\000metadata\n"
      | "sh" :: "-c" :: _ :: "capability-cli" :: "/cli path/agent" :: args ->
        args, (match args with
        | ["--version"] -> Ok "agent 1.0\n"
        | ["--help"] -> Ok (page agent [] ["-p"; "--json"] [parent; "exec"])
        | ["exec"; "--help"] -> Ok (page agent ["exec"] ["--json"] ["resume"])
        | ["exec"; "resume"; "--help"] -> Ok (page agent ["exec"; "resume"] ["--json"] [])
        | [p; "--help"] when p = parent -> Ok (page agent [p] [] ["status"])
        | [p; "status"; "--help"] when p = parent -> Ok (page agent [p; "status"] [] [])
        | [p; "status"] when p = parent ->
          if agent = Config.Claude then Ok {|{"loggedIn":true,"email":"PRIVATE","token":"SECRET"}|}
          else Ok "Logged in using an API key: SECRET\n"
        | _ -> Alcotest.fail "unexpected CLI command")
      | _ -> Alcotest.fail "unexpected probe argv" in
    modify key result in
  run, calls

let observe ?(target = host) ?(agent = Config.Codex) run =
  C.observe ~run ~now ~host:target ~cwd:"/project with spaces" ~agent

let test_versions_flags_hosts () =
  let run, _ = fixture () in
  let a = observe run in
  check "version" (Known "agent 1.0") a.version;
  check "credentials reported" (Known Present) a.credentials;
  check "flag advertised" (Known true) (C.advertised_flag a ~scope:["exec"] "--json");
  check "old CLI lacks advertised flag" (Known false)
    (C.advertised_flag a ~scope:["exec"] "--dangerously-bypass-approvals-and-sandbox");
  let run, _ = fixture ~modify:(fun args result -> match args with
    | ["--version"] -> Ok "agent 2.0\n"
    | ["login"; "status"] -> exited 1 "Not logged in\n"
    | _ -> result) () in
  let b = observe ~target:remote run in
  check "different host version" (Known "agent 2.0") b.version;
  check "different host auth" (Known Absent) b.credentials;
  check "host retained" remote b.host;
  let retained = Marshal.to_string a [] in
  Alcotest.(check bool) "no credential output retained" false
    (Discord_agents.Resource.contains_substring ~haystack:retained ~needle:"SECRET")

let test_identity_and_failures () =
  List.iter (fun (result, expected) ->
    let run, calls = fixture ~modify:(fun args value -> if args = ["identity"] then result else value) () in
    let s = observe run in
    check "identity result" expected s.executable;
    check "no CLI after unavailable identity" 1 (List.length !calls)) [
      exited 3 "", Known None;
      Ok "/bad\npath\000stamp", Unknown Malformed;
      Ok "/path\000stamp\000extra", Unknown Malformed;
      Error (Probe.Host (Discord_agents.Host_exec.Unreachable "SECRET")), Unknown Unreachable;
      Error (Probe.Process Timed_out), Unknown Timed_out;
      Error (Probe.Process Output_limit_exceeded), Unknown Output_limit;
    ];
  let count = ref 0 in
  let run, _ = fixture ~modify:(fun args result ->
    if args = ["identity"] then (incr count; if !count = 2 then Ok "/cli path/agent\000new stamp" else result)
    else result) () in
  let s = observe run in
  check "upgrade invalidates version" (Unknown Executable_changed) s.version;
  check "upgrade invalidates auth" (Unknown Executable_changed) s.credentials;
  let count = ref 0 in
  let run, _ = fixture ~modify:(fun args result ->
    if args = ["identity"] then (incr count; if !count = 2 then Error (Probe.Process Timed_out) else result)
    else result) () in
  check "recheck failure keeps its cause" (Unknown Timed_out) (observe run).version;
  let run, _ = fixture ~modify:(fun args result ->
    if args = ["--version"] then Ok "banner\nagent 1.0\n" else result) () in
  check "noisy version unknown" (Unknown Malformed) (observe run).version

let test_status_guards () =
  let run, calls = fixture ~modify:(fun args result ->
    if args = ["--help"] then Ok (page Config.Codex [] ["--help"] []) else result) () in
  check "old CLI no auth subcommand" (Unknown Not_supported) (observe run).credentials;
  Alcotest.(check bool) "never attempted status" false
    (List.exists (fun (_,args) -> List.mem "status" args) !calls);
  Alcotest.(check bool) "no unadvertised exec subcommand" false
    (List.exists (fun (_,args) -> List.mem "exec" args) !calls);
  let run, _ = fixture ~modify:(fun args result ->
    if args = ["login"; "status"; "--help"] then Ok (page Config.Codex [] [] []) else result) () in
  check "parent help cannot authorize status" (Unknown Malformed) (observe run).credentials;
  let run, _ = fixture ~modify:(fun args result ->
    if args = ["--help"] then Error (Probe.Process Timed_out) else result) () in
  check "missing evidence is not unsupported" (Unknown Timed_out) (observe run).credentials;
  let run, calls = fixture ~agent:Config.Gemini () in
  check "Gemini unknown" (Unknown Not_supported) (observe ~agent:Config.Gemini run).credentials;
  Alcotest.(check bool) "Gemini no auth command" false
    (List.exists (fun (_,args) -> List.mem "status" args || List.mem "login" args) !calls)

let test_auth_parsing () =
  check "Claude present" (Known Present) (C.auth_of_claude (Ok {|{"loggedIn":true,"token":"SECRET"}|}));
  check "Claude absent" (Known Absent) (C.auth_of_claude (exited 1 {|{"loggedIn":false}|}));
  List.iter (fun result -> check "untrustworthy status" (Unknown Malformed) (C.auth_of_claude result)) [
    Ok "noise"; Ok {|{"loggedIn":true,"loggedIn":false}|}; Ok {|{"loggedIn":false}|};
    exited 1 {|{"loggedIn":true}|}; Ok {|{"loggedIn":"true"}|} ];
  check "Codex error is not absent" (Unknown Command_failed) (C.auth_of_codex (exited 1 "server problem SECRET"));
  let run, _ = fixture ~agent:Config.Claude () in
  let s = observe ~agent:Config.Claude run in
  check "Claude observed" (Known Present) s.credentials;
  List.iter (fun needle -> Alcotest.(check bool) "private fields discarded" false
    (Discord_agents.Resource.contains_substring ~haystack:(Marshal.to_string s []) ~needle)) ["SECRET"; "PRIVATE"]

let test_freshness_and_disable () =
  let run, calls = fixture () in
  let s = observe run in
  let current ~time ~age = C.current ~now:time ~max_age:age ~host ~cwd:s.cwd ~agent:Config.Codex s in
  check "fresh" s (current ~time:110. ~age:10.);
  List.iter (fun (time,age) -> check "stale" (Unknown Stale) (current ~time ~age).version)
    [110.1,10.; 99.,10.; nan,10.; 100.,infinity; 100.,(-1.)];
  check "cwd invalidates" (Unknown Context_changed)
    (C.current ~now:100. ~max_age:10. ~host ~cwd:"/other" ~agent:Config.Codex s).credentials;
  check "host config invalidates" (Unknown Context_changed)
    (C.current ~now:100. ~max_age:10. ~host:remote ~cwd:s.cwd ~agent:Config.Codex s).credentials;
  check "agent invalidates" (Unknown Context_changed)
    (C.current ~now:100. ~max_age:10. ~host ~cwd:s.cwd ~agent:Config.Claude s).credentials;
  calls := [];
  check "disabled" (Unknown Disabled) (observe ~target:{ host with enabled = false } run).version;
  check "disabled no calls" [] !calls;
  let bad = C.observe ~run ~now ~host ~cwd:"relative" ~agent:Config.Codex in
  check "unanchored cwd" (Unknown Invalid_context) bad.version;
  check "invalid context no calls" [] !calls

let test_help_parser () =
  let help = C.parse_help ~agent:Config.Codex ~scope:[]
    "noise\nUsage: codex [OPTIONS]\nOptions:\n  -c, --config <key=value>\n          --wrapped description\n      --json  desc\n prose mentions --wrong\nExamples:\n--dangerously-bypass-approvals-and-sandbox is unavailable on this version\n" in
  match help with
  | Known h -> check "only declarations" ["--config"; "--json"; "-c"] h.flags
  | _ -> Alcotest.fail "help should parse"

let test_real_transport () =
  let dir = Filename.temp_dir "capability = ' " "" in
  let old_path = Sys.getenv "PATH" in
  Fun.protect ~finally:(fun () ->
    Unix.putenv "PATH" old_path;
    Sys.readdir dir |> Array.iter (fun name -> Unix.unlink (Filename.concat dir name));
    Unix.rmdir dir) (fun () ->
    let write name contents =
      let path = Filename.concat dir name in
      let oc = open_out path in
      output_string oc contents; close_out oc; Unix.chmod path 0o755; path in
    let _ = write "codex" {|#!/bin/sh
case "$*" in
  --version) echo 'codex 0.1';;
  --help) printf 'Usage: codex [OPTIONS]\nCommands:\n  login  Login\n  exec  Exec\nOptions:\n  --help  Help\n';;
  'exec --help') printf 'Usage: codex exec [OPTIONS]\nCommands:\n  resume  Resume\nOptions:\n  --json  JSON\n';;
  'exec resume --help') printf 'Usage: codex exec resume [OPTIONS]\n';;
  'login --help') printf 'Usage: codex login [OPTIONS]\nCommands:\n  status  Status\n';;
  'login status --help') printf 'Usage: codex login status [OPTIONS]\n';;
  'login status') printf 'Logged in using API key SECRET\n';;
  *) exit 99;;
esac
|} in
    let ssh = write "ssh" (Printf.sprintf "#!/bin/sh\nexec python3 -I %s \"$@\"\n"
      (Filename.quote (Filename.concat (Sys.getcwd ()) "fake_ssh.py"))) in
    Unix.putenv "PATH" (dir ^ ":" ^ old_path);
    Eio_main.run (fun env ->
      List.iter (fun host ->
        let snapshot = C.collect ~clock:(Eio.Stdenv.mono_clock env) ~now ~ssh
          ~host ~cwd:dir ~agent:Config.Codex () in
        check "transport version" (Known "codex 0.1") snapshot.version;
        check "transport auth" (Known Present) snapshot.credentials;
        (match snapshot.executable with
         | Known (Some executable) -> check "resolved executor path" (Filename.concat dir "codex") executable.path
         | _ -> Alcotest.fail "missing executable identity")) [host; remote]))

let () = Alcotest.run "host_capability" ["observations", [
  Alcotest.test_case "per-host versions flags and credentials" `Quick test_versions_flags_hosts;
  Alcotest.test_case "identity changes and unknown failures" `Quick test_identity_and_failures;
  Alcotest.test_case "status invocation guards" `Quick test_status_guards;
  Alcotest.test_case "auth whitelisting and no secret retention" `Quick test_auth_parsing;
  Alcotest.test_case "staleness context and disabled hosts" `Quick test_freshness_and_disable;
  Alcotest.test_case "help declarations only" `Quick test_help_parser;
  Alcotest.test_case "local and fake-SSH collection" `Quick test_real_transport;
]]
