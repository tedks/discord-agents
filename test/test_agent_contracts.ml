(** Live contract tests for the Claude / Codex / Gemini agent binaries.

    Every test here invokes the binary the same way [Agent_process]
    does in production and asserts that the resulting JSON event
    stream still contains the field names and enum values our parser
    depends on. There are no [--help] substring checks — those would
    only verify that the binary still advertises a flag, not that
    using the flag still works the way our integration assumes.
    The only test worth running is the one that exercises the actual
    workflow.

    Tests SKIP (not fail) on:
    - Binary not on PATH — local users configure their own agent set;
      a missing gemini shouldn't fail tests for someone who only uses
      codex. The skip notice is printed at suite start so it stays
      visible.
    - Detected auth failure — that's a user config issue, not a
      contract drift in the binary.

    Tests FAIL (not skip) on anything else: non-zero exit with a
    non-auth-looking error, missing event fields, schema drift.

    Cost: roughly $0.05 in API calls per run, ~30s wall time. The
    Codex resume test leaves a session file in ~/.codex; that's the
    cost of testing the actual resume workflow we depend on, rather
    than a snapshot that would let us pretend it still works. *)

(* ── subprocess + parse helpers ──────────────────────────────────── *)

let run_capture cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let buf = Buffer.create 8192 in
  (try
     while true do
       Buffer.add_channel buf ic 8192
     done
   with End_of_file -> ());
  let status = Unix.close_process_in ic in
  (status, Buffer.contents buf)

let binary_present binary =
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" binary) = 0

let contains_substring text needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) text 0 in
    true
  with Not_found -> false

(** Heuristic union of auth-failure messages across the three
    binaries. Any match short-circuits to a clean skip — auth is
    user config, not a contract drift. *)
let auth_failure_indicators = [
  "unauthenticated"; "Unauthorized"; "Not authorized";
  "API key"; "api_key"; "401"; "403";
  "log in"; "Please log in"; "login required";
  "Please run"; "authenticate"; "credentials"; "OAuth";
]

let looks_like_auth_failure text =
  List.exists (fun ind -> contains_substring text ind) auth_failure_indicators

let parse_lines parse output =
  String.split_on_char '\n' output
  |> List.filter (fun l -> l <> "")
  |> List.concat_map parse

let any_text_delta =
  List.exists (function
    | Discord_agents.Agent_process.Text_delta _ -> true
    | _ -> false)

let captured_session_id events =
  List.find_map (function
    | Discord_agents.Agent_process.Result { session_id = Some sid; _ } ->
      Some sid
    | _ -> None) events

(** Run a command, skip on auth failure, fail on any other non-zero
    exit, return parsed events on success. *)
let invoke_or_skip ~label ~cmd ~parse =
  Printf.printf "  [live] %s ...\n%!" label;
  let (status, output) = run_capture cmd in
  let exit_ok = match status with Unix.WEXITED 0 -> true | _ -> false in
  if not exit_ok && looks_like_auth_failure output then begin
    Printf.printf "  [skip] %s: looks like auth not configured\n%!" label;
    Alcotest.skip ()
  end;
  if not exit_ok then begin
    let trimmed =
      if String.length output > 400
      then String.sub output 0 400 ^ "\n...(truncated)" else output
    in
    Alcotest.failf "%s exited non-zero: %s" label trimmed
  end;
  parse_lines parse output

(** Generate a UUID-v4 for Claude session ids. Reads /proc on Linux
    or shells out to uuidgen. We don't reuse [Resource.generate_uuid]
    because it depends on Mirage_crypto_rng being initialized — which
    bin/main.ml sets up but this test binary doesn't. *)
let fresh_uuid () =
  let (_, out) = run_capture
    "uuidgen 2>/dev/null || cat /proc/sys/kernel/random/uuid" in
  String.trim out

(* ── per-agent live tests ────────────────────────────────────────── *)

(* Each test invokes the binary with the same flag shape that
   Agent_process uses in production. The prompt always asks for a
   single-word reply so the response is small and cheap. *)

let codex_fresh =
  Alcotest.test_case "fresh exec emits thread.started + agent_message"
    `Slow (fun () ->
      let cmd =
        "codex exec --json --full-auto --skip-git-repo-check --ephemeral \
         -- 'reply with the single word ok'"
      in
      let events = invoke_or_skip ~label:"codex fresh" ~cmd
        ~parse:Discord_agents.Agent_process.parse_codex_json_line in
      Alcotest.(check bool)
        "thread.started yields a session id"
        true (captured_session_id events <> None);
      Alcotest.(check bool)
        "agent_message yields a Text_delta"
        true (any_text_delta events))

let codex_resume =
  Alcotest.test_case "resume preserves the captured session id"
    `Slow (fun () ->
      let fresh_cmd =
        "codex exec --json --full-auto --skip-git-repo-check \
         -- 'reply with the single word ok'"
      in
      let fresh_events = invoke_or_skip ~label:"codex fresh (for resume)"
        ~cmd:fresh_cmd
        ~parse:Discord_agents.Agent_process.parse_codex_json_line in
      let sid = match captured_session_id fresh_events with
        | Some s -> s
        | None -> Alcotest.fail "codex fresh did not emit a session id"
      in
      let resume_cmd = Printf.sprintf
        "codex exec resume --json --full-auto --skip-git-repo-check %s \
         -- 'reply with the single word ok'" sid
      in
      let resume_events = invoke_or_skip ~label:"codex resume"
        ~cmd:resume_cmd
        ~parse:Discord_agents.Agent_process.parse_codex_json_line in
      Alcotest.(check (option string))
        "resume reports the same session id we resumed against"
        (Some sid) (captured_session_id resume_events);
      Alcotest.(check bool)
        "resume produces an agent_message"
        true (any_text_delta resume_events))

let claude_fresh =
  Alcotest.test_case "fresh -p with --session-id emits text + result"
    `Slow (fun () ->
      let sid = fresh_uuid () in
      let cmd = Printf.sprintf
        "claude -p --verbose --output-format stream-json --session-id %s \
         -- 'reply with the single word ok'" sid
      in
      let events = invoke_or_skip ~label:"claude fresh" ~cmd
        ~parse:Discord_agents.Agent_process.parse_stream_json_line in
      Alcotest.(check bool)
        "assistant text captured as Text_delta"
        true (any_text_delta events);
      Alcotest.(check (option string))
        "result event reports the session id we passed in"
        (Some sid) (captured_session_id events))

let claude_resume =
  Alcotest.test_case "resume continues a prior --session-id"
    `Slow (fun () ->
      let sid = fresh_uuid () in
      let fresh_cmd = Printf.sprintf
        "claude -p --verbose --output-format stream-json --session-id %s \
         -- 'reply with the single word ok'" sid
      in
      let _ = invoke_or_skip ~label:"claude fresh (for resume)"
        ~cmd:fresh_cmd
        ~parse:Discord_agents.Agent_process.parse_stream_json_line in
      let resume_cmd = Printf.sprintf
        "claude -p --verbose --output-format stream-json --resume %s \
         -- 'reply with the single word ok'" sid
      in
      let resume_events = invoke_or_skip ~label:"claude resume"
        ~cmd:resume_cmd
        ~parse:Discord_agents.Agent_process.parse_stream_json_line in
      Alcotest.(check bool)
        "resume produces assistant text"
        true (any_text_delta resume_events))

let gemini_fresh =
  Alcotest.test_case "fresh -p emits init session_id + assistant text"
    `Slow (fun () ->
      let cmd =
        "gemini -p 'reply with the single word ok' -o stream-json --yolo"
      in
      let events = invoke_or_skip ~label:"gemini fresh" ~cmd
        ~parse:Discord_agents.Agent_process.parse_gemini_stream_json_line in
      Alcotest.(check bool)
        "init yields a session id"
        true (captured_session_id events <> None);
      Alcotest.(check bool)
        "assistant content captured as Text_delta"
        true (any_text_delta events))

let gemini_resume =
  Alcotest.test_case "resume preserves the captured session id"
    `Slow (fun () ->
      let fresh_cmd =
        "gemini -p 'reply with the single word ok' -o stream-json --yolo"
      in
      let fresh_events = invoke_or_skip ~label:"gemini fresh (for resume)"
        ~cmd:fresh_cmd
        ~parse:Discord_agents.Agent_process.parse_gemini_stream_json_line in
      let sid = match captured_session_id fresh_events with
        | Some s -> s
        | None -> Alcotest.fail "gemini fresh did not emit a session id"
      in
      let resume_cmd = Printf.sprintf
        "gemini -p 'reply with the single word ok' -o stream-json --yolo \
         --resume %s" sid
      in
      let resume_events = invoke_or_skip ~label:"gemini resume"
        ~cmd:resume_cmd
        ~parse:Discord_agents.Agent_process.parse_gemini_stream_json_line in
      Alcotest.(check (option string))
        "resume reports the same session id we resumed against"
        (Some sid) (captured_session_id resume_events);
      Alcotest.(check bool)
        "resume produces assistant text"
        true (any_text_delta resume_events))

(* ── runner ──────────────────────────────────────────────────────── *)

(** Build the test list for one agent. Returns [] (and prints a skip
    notice at suite start) if the binary isn't on PATH. The Alcotest
    report shows the group with zero tests, which keeps the skip
    visible — silent skips look like passes. *)
let group binary tests =
  if not (binary_present binary) then begin
    Printf.printf "[skip] %s not on PATH — skipping all %s tests\n%!"
      binary binary;
    []
  end else tests

let () =
  Alcotest.run "agent_contracts" [
    "claude", group "claude" [claude_fresh; claude_resume];
    "codex",  group "codex"  [codex_fresh;  codex_resume];
    "gemini", group "gemini" [gemini_fresh; gemini_resume];
  ]
