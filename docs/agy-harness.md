# agy (Antigravity CLI) harness — design

Status: agent kind merged (#157); session discovery and resume in
progress. Facts below were checked against agy 1.1.3 on tower0, signed
out on 2026-10-23 and signed in on 2026-10-24. Every signed-in run so
far ended in an account quota error ("Individual quota reached", on
every listed model), so no successful turn has been captured yet.
Anything marked *unverified* must be confirmed before code relies on
it.

## Why

Google withdrew the consumer OAuth sign-in that Gemini CLI used, so the
`gemini` agent can no longer authenticate except with an API key. Gemini
stays in the tree, marked unmaintained, as an API-key fallback. Work on
Google models moves to agy (Antigravity CLI). agy is a prebuilt Go
binary pinned through home-manager (`antigravity-cli.nix`), not an npm
package.

## What agy exposes (verified)

`agy --help` lists: `-p/--print`, `--prompt-interactive`, `--continue`,
`--conversation <ID>`, `--dangerously-skip-permissions`, `--model`,
`--agent`, `--mode {accept-edits,plan}`, `--add-dir`, `--project`,
`--new-project`, `--sandbox`, `--log-file`, and
`--print-timeout` (default **5m0s**).

### Structured output exists, but `--help` doesn't list it

`--help` documents no structured output, but agy's own flag parser
accepts two flags it doesn't list:

| Probe (`agy -p hi …`, unauthenticated) | Result |
|---|---|
| `--bogus-flag x` | exit 2, `flags provided but not defined: -bogus-flag` |
| `--output-format stream-json` | accepted; emits one JSON line (below) |
| `--output-format json` | accepted; same object, no `event` wrapper |
| `--json-schema {}` | exit 1, `--json-schema can only be used when --output-format is 'json' or 'stream-json'` |

The binary also contains the usage string `Output format for print mode
(text, json, stream-json)`, sitting next to `print-timeout (default %s)`
among the CLI's own flag strings. These flags belong to agy, not to a
vendored dependency.

Signed out, the only event is the terminal one:

```json
{"event":"result","result":{"conversation_id":"","status":"ERROR","response":"","error":"authentication failed or timed out","duration_seconds":0,"num_turns":0,"usage":{"input_tokens":0,"output_tokens":0,"thinking_tokens":0,"total_tokens":0}}}
```

Note the discriminator is `event`, not `type` (Claude, Codex and Gemini
all use `type`).

Signed in, a run emits `init`, then one `step_update` per step, then
`result` (captured 2026-10-24, trimmed):

```json
{"event":"init","conversation_id":"b94541d7-…","init":{"cwd":"/tmp/…","tools":["run_command","view_file","replace_file_content","call_mcp_tool", …],"permission_mode":"always-proceed"}}
{"event":"step_update","step_update":{"conversation_id":"b94541d7-…","step_index":2,"state":"DONE","step_type":"agent_response","duration_seconds":0.22}}
{"event":"step_update","step_update":{"conversation_id":"b94541d7-…","step_index":3,"state":"DONE","step_type":"error_message"}}
{"event":"result","result":{"conversation_id":"b94541d7-…","status":"ERROR","response":"","error":"Individual quota reached. …","num_turns":1, …}}
```

`step_update` carried no text or tool detail in these runs. The binary
has a `text_delta` struct tag, but no successful turn has been captured
to show where text and tool calls appear. **Text-delta and tool-step
shapes are still unverified.** The parser reads the id from `init` and
`result`, takes the reply from `result.response`, and passes
`step_update` through as `Other`. agy's built-in tools include
`run_command`, `view_file`, `replace_file_content`,
`multi_replace_file_content`, `write_to_file`, `grep_search`,
`find_by_name`, `list_dir`, `search_web` and `read_url_content`. MCP
tools are reached through a generic `call_mcp_tool`, not listed
individually in `init`.

### Other verified behaviour

- **Workspace = cwd.** The log shows `workspaceDirs=[<cwd>]`. Running in
  the worktree is enough.
- **Print-mode timeout is 5 minutes by default.** Agent turns routinely
  run longer, so the harness must pass an explicit `--print-timeout`.
- **Unauthenticated print mode blocks for about a minute.** It prints an
  OAuth URL to stderr, then `Waiting for authentication (timeout 60s)...
  Or, paste the authorization code here and press Enter`. This wait is
  separate from `--print-timeout`. When it expires, agy emits the
  `ERROR` result below and exits 1 (measured: 60s, exit 1, with stdin at
  `/dev/null`). So a signed-out turn costs a minute and is reported as a
  failure, but does not hang. The bot gives agy `/dev/null` as stdin so
  that prompt can never read the bot's own terminal. agy does accept the
  code on stdin, so a bot-driven sign-in would need a dedicated login
  path that pipes the code in. Session turns can't do it, and it is
  untested.
- **A dash-leading prompt is safe after `-p`.** `agy -p -x …` takes `-x`
  as the prompt; it is not parsed as an unknown flag.
- **Headless tool approval auto-denies.** The binary contains `the %s
  tool(s) required approval that headless mode cannot prompt for, so
  they were auto-denied`. The harness must pass
  `--dangerously-skip-permissions`, which matches the trust model behind
  Codex's `--dangerously-bypass-approvals-and-sandbox`.
- **Conversation storage (verified signed in).** Everything is under
  `~/.gemini/antigravity-cli/`. (`~/.gemini/antigravity/` is never
  created; the log line about it is a red herring.)
  - `conversations/<id>.db` is SQLite with protobuf blobs.
  - `brain/<id>/.system_generated/logs/transcript.jsonl` has one JSON
    object per step. Every conversation has one. Line 1 is the
    `USER_INPUT` step, whose `content` wraps the prompt in
    `<USER_REQUEST>…</USER_REQUEST>`.
  - `conversation_summaries.db` holds title, preview and step count.
    Its `workspace_uris` column is empty.
  - `cache/last_conversations.json` maps `{cwd: latest conversation id}`.
    It is the only place a working directory is recorded.
- **Resume (verified signed in).** `--conversation <id>` continues the
  conversation; step indices carry on from the previous run. It works
  from any cwd, so the directory only decides which files the agent
  touches. **An unknown id does not fail.** agy silently starts a new
  conversation under a fresh id. The bot compares the reported id with
  the one it asked for, and tells the user when they differ.
- **Models.** On this account `agy models` lists Claude Opus 5.5 and
  Claude Sonnet 5.5 (Low/Medium/High) and GPT-OSS 120B. No Gemini models
  are offered. `--model` takes these labels verbatim. The quota is per
  account, not per model.
- **Shared config.** `~/.gemini/config/` holds `config.json` (permission
  grants, dotfiles-managed), `GEMINI.md`, and an empty `mcp_config.json`.
  agy shares this config with the Antigravity IDE.

## Mapping onto the Gemini shape

Each Gemini integration point gets an agy counterpart in the same place.

| Gemini | agy | Status |
|---|---|---|
| `Config.Gemini`, `"gemini"` | `Config.Agy`, `"agy"` | scaffold PR |
| `caller_pinned_session_id Gemini = false` | `false`; id from `init` and `result` `conversation_id` | done |
| `reasoning_efforts_for_agent Gemini = []` | `[]`; no effort flag exists | scaffold PR |
| `gemini_args` | `agy_args`: `agy -p <prompt> --output-format stream-json --dangerously-skip-permissions --print-timeout <long> [--model m] [--conversation <id>]` | scaffold PR |
| `parse_gemini_stream_json_line` | `parse_agy_stream_json_line`: `init` id, `result` text/error/id; empty `conversation_id` → no id | done; deltas/tools await a successful capture |
| `gemini_tool_name_of` | `agy_tool_name_of` | awaits a successful capture showing tool steps |
| `compose_session_prompt` inline `<bot-context>` | same (agy has no system-prompt flag) | scaffold PR |
| `setup_gemini_mcp` (`<cwd>/.gemini/settings.json`) | no per-worktree route exists; see below | **operator decision** |
| `Gemini_sessions` (`~/.gemini/tmp`) | `Agy_sessions` (`brain/*/…/transcript.jsonl` + `last_conversations.json`) | done |
| `!gemini-sessions`, `list_gemini_sessions` | `!agy-sessions`, `list_agy_sessions` | done |
| `login_help_json Gemini` / `start_login_flow` enum | agy entry: run `agy` interactively on the host | scaffold PR |
| agent enums (`mcp_tool.ml`, `control_api.ml`, Python oracle) | add `"agy"` | scaffold PR |

### MCP: only a user-global route exists

Measured 2026-10-24 by pointing each candidate at a wrapper that
records being launched, then execs the real `discord-agents-mcp`:

| Route | Loaded by `agy -p`? |
|---|---|
| `<cwd>/.agents/plugins/<name>/{plugin.json,mcp_config.json}` | **no**: `agy plugin list` says "No imported plugins"; `agy plugin enable <name>` says not found (it passes `agy plugin validate`) |
| `<cwd>/.agents/mcp_config.json` | **no**: server never launched |
| `~/.gemini/config/mcp_config.json` (global) | **yes**: launched at startup with cwd = the workspace |

The server agy launches inherits agy's environment:
`DISCORD_AGENTS_THREAD_ID` set on agy reached it unchanged. So one
static global entry can serve every agy session and still identify the
calling thread. That global file is user config shared with the user's
own interactive agy and the Antigravity IDE. Whether the bot may write
it, or whether it goes into home-manager, is an operator decision. The
two probe writes were reverted to the original empty file at once.

Recommended shape, so nothing per-session is ever written and
interactive agy gets nothing:

```json
{"mcpServers":{"discord_agents":{"command":"/bin/sh","args":["-c","[ -n \"$DISCORD_AGENTS_MCP_FOR_AGY\" ] || exit 0; exec \"$DISCORD_AGENTS_MCP_FOR_AGY\""]}}}
```

The bot would set `DISCORD_AGENTS_MCP_FOR_AGY` to the resolved server
path in agy's child environment only. Still to verify: how agy reacts
to a server that exits immediately in sessions where the variable is
unset.

## Remaining

1. Capture a **successful** `stream-json` turn that uses tools, once the
   account quota resets. Then parse text deltas and tool steps and add
   `agy_tool_name_of`.
2. Operator decision on the MCP route (above), then wire it.
3. Decide whether to detect a signed-out run early (stderr
   `Authentication required`) instead of waiting out agy's 60s auth
   timeout.
