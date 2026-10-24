# agy (Antigravity CLI) harness — design

Status: design, plus scaffolding in progress. The facts below were
checked against agy 1.1.3 on tower0 on 2026-10-23, with agy **not
signed in**. Anything marked *unverified* has to be confirmed after
sign-in before code relies on it.

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

The only event observed so far is the terminal one, which agy emitted
when authentication failed:

```json
{"event":"result","result":{"conversation_id":"","status":"ERROR","response":"","error":"authentication failed or timed out","duration_seconds":0,"num_turns":0,"usage":{"input_tokens":0,"output_tokens":0,"thinking_tokens":0,"total_tokens":0}}}
```

Note the discriminator is `event`, not `type` (Claude, Codex and Gemini
all use `type`).

The binary has Go types `steps.StreamEvent` and `steps.InitPayload`,
and struct tags `text_delta`, `step_type`, `tool_name` and `tool_info`.
That points to more events (an init event, text deltas, tool steps).
**Their wire shapes are unverified**, and the parser must not guess
them. Until a signed-in capture exists, the parser handles only
`result`. Everything else is passed through as `Other`, so display
falls back to the final `result.response` text.

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
  that prompt can never read the bot's own terminal. Because agy accepts the code on stdin, a sign-in
  flow driven through the bot looks possible, but is untested.
- **A dash-leading prompt is safe after `-p`.** `agy -p -x …` takes `-x`
  as the prompt; it is not parsed as an unknown flag.
- **Headless tool approval auto-denies.** The binary contains `the %s
  tool(s) required approval that headless mode cannot prompt for, so
  they were auto-denied`. The harness must pass
  `--dangerously-skip-permissions`, which matches the trust model behind
  Codex's `--dangerously-bypass-approvals-and-sandbox`.
- **State directories.** App data lives in `~/.gemini/antigravity-cli/`
  (logs, `conversation_summaries.db`, `implicit/*.pb`). The server
  *reads* conversations from `~/.gemini/antigravity/conversations/`
  (log: `Failed to read conversations directory …`). According to the
  embedded agent docs, each conversation has `transcript.jsonl` and
  `transcript_full.jsonl`. **Where these files live on disk is
  unverified.**
- **Shared config.** `~/.gemini/config/` holds `config.json` (permission
  grants, dotfiles-managed), `GEMINI.md`, and an empty `mcp_config.json`.
  agy shares this config with the Antigravity IDE.

## Mapping onto the Gemini shape

Each Gemini integration point gets an agy counterpart in the same place.

| Gemini | agy | Status |
|---|---|---|
| `Config.Gemini`, `"gemini"` | `Config.Agy`, `"agy"` | scaffold PR |
| `caller_pinned_session_id Gemini = false` | `false`; id is `result.conversation_id` | scaffold PR |
| `reasoning_efforts_for_agent Gemini = []` | `[]`; no effort flag exists | scaffold PR |
| `gemini_args` | `agy_args`: `agy -p <prompt> --output-format stream-json --dangerously-skip-permissions --print-timeout <long> [--model m] [--conversation <id>]` | scaffold PR |
| `parse_gemini_stream_json_line` | `parse_agy_stream_json_line`: `result` only; empty `conversation_id` → no id | scaffold PR; deltas/tools after sign-in |
| `gemini_tool_name_of` | `agy_tool_name_of` | after sign-in (tool names unknown) |
| `compose_session_prompt` inline `<bot-context>` | same (agy has no system-prompt flag) | scaffold PR |
| `setup_gemini_mcp` (`<cwd>/.gemini/settings.json`) | candidate: `<cwd>/.agents/plugins/discord-agents/{plugin.json,mcp_config.json}` + `.agents/` in `.git/info/exclude` | **unverified**; after sign-in |
| `Gemini_sessions` (`~/.gemini/tmp`) | `Agy_sessions` | after sign-in (storage format unknown) |
| `!gemini-sessions`, `list_gemini_sessions` | `!agy-sessions`, `list_agy_sessions` | with `Agy_sessions` |
| `login_help_json Gemini` / `start_login_flow` enum | agy entry: run `agy` interactively on the host | scaffold PR |
| agent enums (`mcp_tool.ml`, `control_api.ml`, Python oracle) | add `"agy"` | scaffold PR |

### MCP: not the global file

Don't write `~/.gemini/config/mcp_config.json`. It is user-global and
shared with the IDE, and concurrent sessions in different worktrees
would race on one file. The per-worktree candidate is the workspace
customization root `<project>/.agents/`. The embedded docs say agy
discovers skills, agents and plugins there, and a plugin may carry an
`mcp_config.json`. Whether `agy -p` loads a workspace plugin's MCP
servers without first running `agy plugin install`/`enable` is the
thing to test after sign-in. If it doesn't, agy sessions start without
MCP tools, the same degradation as an unresolved MCP server, rather
than touching global config.

## After sign-in (blocked on a human at a terminal)

1. Capture a full `stream-json` run that uses shell, read, edit and MCP
   tools. Commit the transcript as a test fixture, then write the
   delta, tool and init cases against it.
2. Locate on-disk conversations. Write `Agy_sessions.discover` /
   `find_by_prefix` against what is actually there.
3. Test whether the `.agents/plugins` MCP route loads in print mode.
4. Confirm that `--conversation <id>` resumes in print mode with the id
   from `result.conversation_id`.
5. Decide whether to detect a signed-out run early (stderr
   `Authentication required`) instead of waiting out agy's 60s auth
   timeout.
