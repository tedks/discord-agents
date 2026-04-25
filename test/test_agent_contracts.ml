(** Live contract tests for the Claude / Codex / Gemini agent binaries.

    A failed test pinpoints a specific bit of the binary's surface that
    our integration depends on and that has stopped being true in the
    user's local environment. The test never asserts against
    snapshotted output: it queries the live binary every time, because
    a snapshot is just a frozen claim about how the binary used to
    behave and is indistinguishable from the binary lying.

    Two tiers:

    - Tier 1, CLI surface (always run). Asserts that [<binary> --help]
      still mentions every flag and subcommand we put on the wire from
      [Agent_process]. Cost: ~300ms total, no network, deterministic.

    - Tier 2, live schema ([LIVE=1] only). Invokes each agent with a
      trivial prompt and asserts that the JSON event stream still
      contains the field names and enum values our parser depends on.
      Cost: a few cents and ~10s; gated because it spends real money
      on the user's API account. Auth failures are detected and
      reported as skips, not failures (auth is a config issue, not a
      contract drift).

    Per-agent skip on missing binary, with a printed reason at suite
    start — silent skips look like passes and are dangerous. *)

(* ── small subprocess helper ─────────────────────────────────────── *)

(** Run a shell command, capture stdout+stderr together, return the
    exit status and the captured text. *)
let run_capture cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  let status = Unix.close_process_in ic in
  (status, Buffer.contents buf)

let binary_present binary =
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" binary) = 0

(** Cached [<binary> [<subcmd>] --help] — one subprocess call per pair
    no matter how many tests reference it. *)
let help_cache : (string * string option, string) Hashtbl.t = Hashtbl.create 8

let cached_help binary subcmd =
  let key = (binary, subcmd) in
  match Hashtbl.find_opt help_cache key with
  | Some text -> text
  | None ->
    let cmd = match subcmd with
      | None -> Printf.sprintf "%s --help" binary
      | Some sub -> Printf.sprintf "%s %s --help" binary sub
    in
    let (_, text) = run_capture cmd in
    Hashtbl.add help_cache key text;
    text

let contains_substring text needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) text 0 in
    true
  with Not_found -> false

(** Assert [needle] appears in [<binary> [<subcmd>] --help]. The
    [~why] string is folded into the test's check label so a failure
    points at the integration site that breaks if the contract is
    gone. *)
let must_mention ~binary ?subcmd ~why needle =
  let context = match subcmd with
    | None -> Printf.sprintf "%s --help" binary
    | Some sub -> Printf.sprintf "%s %s --help" binary sub
  in
  let help = cached_help binary subcmd in
  Alcotest.(check bool)
    (Printf.sprintf "%s mentions %S — used by %s" context needle why)
    true (contains_substring help needle)

(* ── Tier 1: CLI surface ─────────────────────────────────────────── *)

(* Each list IS the spec of what we put on the wire. Adding a new flag
   to Agent_process without adding it here is silent risk. *)

let claude_surface = [
  Alcotest.test_case "supports -p" `Quick (fun () ->
    must_mention ~binary:"claude"
      ~why:"agent_process.claude_args base flag" "-p");
  Alcotest.test_case "supports --output-format" `Quick (fun () ->
    must_mention ~binary:"claude"
      ~why:"agent_process.claude_args" "--output-format");
  Alcotest.test_case "supports stream-json" `Quick (fun () ->
    must_mention ~binary:"claude"
      ~why:"parse_stream_json_line consumes this format" "stream-json");
  Alcotest.test_case "supports --session-id" `Quick (fun () ->
    must_mention ~binary:"claude"
      ~why:"claude_args fresh-run id pin" "--session-id");
  Alcotest.test_case "supports --resume" `Quick (fun () ->
    must_mention ~binary:"claude"
      ~why:"claude_args resume on subsequent turns" "--resume");
  Alcotest.test_case "supports --mcp-config" `Quick (fun () ->
    must_mention ~binary:"claude"
      ~why:"agent_process MCP server wiring" "--mcp-config");
  Alcotest.test_case "supports --append-system-prompt" `Quick (fun () ->
    must_mention ~binary:"claude"
      ~why:"agent_process system_prompt forwarding" "--append-system-prompt");
]

let codex_surface = [
  Alcotest.test_case "exec subcommand exists" `Quick (fun () ->
    must_mention ~binary:"codex"
      ~why:"codex_args base subcommand" "exec");
  Alcotest.test_case "exec --json" `Quick (fun () ->
    must_mention ~binary:"codex" ~subcmd:"exec"
      ~why:"codex_args + parse_codex_json_line" "--json");
  Alcotest.test_case "exec --full-auto" `Quick (fun () ->
    must_mention ~binary:"codex" ~subcmd:"exec"
      ~why:"codex_args runs unattended" "--full-auto");
  Alcotest.test_case "exec --skip-git-repo-check" `Quick (fun () ->
    must_mention ~binary:"codex" ~subcmd:"exec"
      ~why:"codex_args inside fresh worktrees" "--skip-git-repo-check");
  Alcotest.test_case "exec resume subcommand" `Quick (fun () ->
    must_mention ~binary:"codex" ~subcmd:"exec"
      ~why:"codex_args resume on subsequent turns" "resume");
  Alcotest.test_case "exec resume --json" `Quick (fun () ->
    must_mention ~binary:"codex" ~subcmd:"exec resume"
      ~why:"codex_args resume invocation" "--json");
  Alcotest.test_case "exec resume --skip-git-repo-check" `Quick (fun () ->
    must_mention ~binary:"codex" ~subcmd:"exec resume"
      ~why:"codex_args resume invocation" "--skip-git-repo-check");
]

let gemini_surface = [
  Alcotest.test_case "supports -p" `Quick (fun () ->
    must_mention ~binary:"gemini"
      ~why:"agent_process gemini args" "-p");
  Alcotest.test_case "supports -o" `Quick (fun () ->
    must_mention ~binary:"gemini"
      ~why:"agent_process gemini args" "-o");
  Alcotest.test_case "supports stream-json" `Quick (fun () ->
    must_mention ~binary:"gemini"
      ~why:"parse_stream_json_line consumes this format" "stream-json");
]

(* ── Tier 2: live schema (LIVE=1 only) ───────────────────────────── *)

(* These cost real money on the user's API account, so they're opt-in.
   Each makes one tiny invocation per agent and asserts on the JSON
   event SHAPE only — never content, since model output text varies. *)

let live_enabled =
  match Sys.getenv_opt "LIVE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

(** Heuristic: many possible auth-failure messages from the three
    binaries. Any match means we should skip rather than fail — the
    user's config issue, not a contract drift. *)
let auth_failure_indicators =
  ["unauthenticated"; "Unauthorized"; "Not authorized";
   "API key"; "api_key"; "log in"; "login required";
   "Please run"; "authenticate"; "credentials"; "401"; "403"]

let looks_like_auth_failure text =
  List.exists (fun ind -> contains_substring text ind) auth_failure_indicators

(** Common shape for a live test: invoke, parse with the project's
    parser, run shape assertions on the resulting events. Skips
    cleanly on detected auth failure. *)
let live_test ~name ~cmd ~parse ~assertions =
  Alcotest.test_case name `Slow (fun () ->
    Printf.printf "  [live] %s ...\n%!" name;
    let (status, output) = run_capture cmd in
    let exit_ok = match status with Unix.WEXITED 0 -> true | _ -> false in
    if not exit_ok && looks_like_auth_failure output then begin
      Printf.printf "  [skip] %s: looks like auth not configured\n%!" name;
      Alcotest.skip ()
    end;
    if not exit_ok then begin
      let trimmed = if String.length output > 400
        then String.sub output 0 400 ^ "..." else output in
      Alcotest.failf "%s exited non-zero: %s" name trimmed
    end;
    let lines = String.split_on_char '\n' output
                |> List.filter (fun l -> l <> "") in
    let events = List.concat_map parse lines in
    assertions events)

(* Concrete shape predicates over Agent_process.stream_event lists. *)

let any_text_delta events =
  List.exists (function
    | Discord_agents.Agent_process.Text_delta _ -> true
    | _ -> false) events

let any_result_with_session_id events =
  List.exists (function
    | Discord_agents.Agent_process.Result { session_id = Some _; _ } -> true
    | _ -> false) events

let codex_live = [
  live_test ~name:"emits thread.started + agent_message"
    ~cmd:"codex exec --json --full-auto --skip-git-repo-check \
          --ephemeral -- 'reply with the single word ok'"
    ~parse:Discord_agents.Agent_process.parse_codex_json_line
    ~assertions:(fun events ->
      Alcotest.(check bool)
        "thread.started captured as Result with session_id"
        true (any_result_with_session_id events);
      Alcotest.(check bool)
        "agent_message captured as Text_delta"
        true (any_text_delta events));
]

let claude_live =
  let test_uuid =
    (* Random per-run id so concurrent runs don't collide on a fixed value. *)
    Random.self_init ();
    Printf.sprintf "%08x-%04x-4%03x-8%03x-%012x"
      (Random.bits ()) (Random.bits () land 0xffff)
      (Random.bits () land 0xfff) (Random.bits () land 0xfff)
      (Random.bits ())
  in
  [ live_test ~name:"emits assistant text + result"
      ~cmd:(Printf.sprintf
              "claude -p --verbose --output-format stream-json \
               --session-id %s -- 'reply with the single word ok'"
              test_uuid)
      ~parse:Discord_agents.Agent_process.parse_stream_json_line
      ~assertions:(fun events ->
        Alcotest.(check bool)
          "assistant text captured as Text_delta"
          true (any_text_delta events);
        Alcotest.(check bool)
          "result event captured with session_id"
          true (any_result_with_session_id events));
  ]

let gemini_live = [
  live_test ~name:"emits stream-json text"
    ~cmd:"gemini -p 'reply with the single word ok' -o stream-json"
    ~parse:Discord_agents.Agent_process.parse_stream_json_line
    ~assertions:(fun events ->
      Alcotest.(check bool)
        "assistant text captured as Text_delta"
        true (any_text_delta events));
]

(* ── runner ──────────────────────────────────────────────────────── *)

(** Build the test list for one agent. Returns [] (and prints a skip
    notice) if the binary isn't on PATH. The Alcotest report will show
    the group with zero tests, making the skip visible. *)
let group binary surface live =
  if not (binary_present binary) then begin
    Printf.printf "[skip] %s not on PATH — skipping all %s tests\n%!"
      binary binary;
    []
  end else
    let live_tests = if live_enabled then live else [] in
    surface @ live_tests

let () =
  if not live_enabled then
    Printf.printf
      "[note] LIVE=1 not set — skipping live schema tests \
       (set LIVE=1 to spend a few cents validating the JSON event \
       schemas against real agent invocations)\n%!";
  Alcotest.run "agent_contracts" [
    "claude", group "claude" claude_surface claude_live;
    "codex",  group "codex"  codex_surface  codex_live;
    "gemini", group "gemini" gemini_surface gemini_live;
  ]
