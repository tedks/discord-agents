# agy (Antigravity CLI) harness — design

Status: agent kind merged (#157); session discovery and resume in
progress. Facts below were checked against agy 1.1.3 on tower0, signed
out on 2026-10-23 and signed in on 2026-10-24. Every signed-in run so
far ended in an account quota error ("Individual quota reached", on
every listed model) until the quota reset; successful turns were captured later that day.
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

In those quota-failed runs, `step_update` carried no text or tool detail. A successful run (captured 2026-10-24, once the quota reset) shows where they are:

```json
{"event":"step_update","step_update":{"step_index":3,"state":"ACTIVE","step_type":"tool","tool_name":"call_mcp_tool","tool_info":{"name":"call_mcp_tool","parameters":{"Arguments":{},"ServerName":"discord_agents","ToolName":"list_projects"}}}}
{"event":"step_update","step_update":{"step_index":3,"state":"DONE","step_type":"tool","tool_name":"call_mcp_tool","tool_info":{"name":"call_mcp_tool","parameters":{…},"output":"1. …"}}}
{"event":"step_update","step_update":{"step_index":9,"state":"ACTIVE","step_type":"agent_response","text_delta":"`list_projects` returned 73 projects; …"}}
{"event":"step_update","step_update":{"step_index":9,"state":"DONE","step_type":"agent_response","text_delta":"…`zotero-scholar-citations`.\n"}}
{"event":"result","result":{"conversation_id":"…","status":"SUCCESS","response":"<the same text>", …}}
```

- **Text:** `agent_response` steps carry `text_delta`. Planner responses that only call tools carry none.
- **Tools:** each tool step is `ACTIVE`, with name and parameters, then `DONE`, with `tool_info.output`. Edit tools give no output and only `TargetFile`.
- **Result:** `result.response` repeats the streamed text. When it is exactly the last reply step, or the reply steps in order with only whitespace between them, nothing more is shown. It fills in a lost tail. If the stream diverged, it follows in full, labelled. Whether `response` is the whole turn or only its last reply step after text, tool, text is unverified; both are handled.
- **Status:** the success value is `SUCCESS`.

Parameter names seen on the wire:

| Tool | Parameters |
|---|---|
| `run_command` | `CommandLine` |
| `view_file` | `AbsolutePath` |
| `write_to_file`, `replace_file_content` | `TargetFile` |
| `grep_search` | `Query`, `SearchPath` |
| `list_dir` | `DirectoryPath` |
| `call_mcp_tool` | `ServerName`, `ToolName`, `Arguments` |

MCP tools aren't listed in `init`; they're reached through `call_mcp_tool`.

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
| `parse_gemini_stream_json_line` | `agy_stream_parser` (per run): `init` id, text deltas, tool steps, `result` error/id | done |
| `gemini_tool_name_of` | `agy_tool_of` (names and parameters) | done |
| `compose_session_prompt` inline `<bot-context>` | same (agy has no system-prompt flag) | scaffold PR |
| `setup_gemini_mcp` (`<cwd>/.gemini/settings.json`) | no per-worktree route exists; see below | **operator decision** |
| `Gemini_sessions` (`~/.gemini/tmp`) | `Agy_sessions` (`brain/*/…/transcript.jsonl` + `last_conversations.json`) | done |
| `!gemini-sessions`, `list_gemini_sessions` | `!agy-sessions`, `list_agy_sessions` | done |
| `login_help_json Gemini` / `start_login_flow` enum | agy entry: run `agy` interactively on the host | scaffold PR |
| agent enums (`mcp_tool.ml`, `control_api.ml`, Python oracle) | add `"agy"` | scaffold PR |

### MCP: the user-global route, gated by an env var

Measured on 2026-10-24 with a wrapper that records being launched and then execs the real `discord-agents-mcp`:

| Route | Loaded by `agy -p`? |
|---|---|
| `<cwd>/.agents/plugins/<name>/{plugin.json,mcp_config.json}` | **no** (`agy plugin list` says "No imported plugins") |
| `<cwd>/.agents/mcp_config.json` | **no** |
| `~/.gemini/config/mcp_config.json` (global) | **yes**: launched at startup, cwd = workspace, agy's env inherited |

Operator approved (2026-10-24, issue #161) giving agy the MCP tools. The bot keeps one static, inert entry in the global file:

```json
{"mcpServers":{"discord_agents":{"command":"/bin/sh","args":["-c","[ -n \"$DISCORD_AGENTS_MCP_FOR_AGY\" ] || exit 0; eval \"exec $DISCORD_AGENTS_MCP_FOR_AGY\"","discord-agents-mcp"]}}}
```

- The entry is merged in, and other servers and keys are kept. The file is rewritten only when our entry is missing or out of date. Extra keys on our entry, from the user or the IDE, are kept and don't trigger a rewrite. A rewrite pretty-prints the whole file, so whitespace changes.
- The file is left alone, and the session runs without MCP, when it:
  - is a symlink;
  - won't parse, isn't an object, or has a non-object `mcpServers`;
  - has duplicate `mcpServers` keys, duplicate `discord_agents` entries, or duplicate keys inside ours;
  - has comments, which a rewrite would drop;
  - holds a number that wouldn't re-serialise as JSON (e.g. `1e400`), even if no rewrite is needed;
  - names someone else's server `discord_agents`;
  - has our entry marked `"disabled": true`.

  The reason is logged once.
- Writes follow the dotfiles installer for `config.json`. The bot holds its own lock (`~/.config/discord-agents/agy-mcp-config.lock`), keeps the raw bytes, re-reads just before the rename, and refuses if anything changed.
  - The temp file is fsynced, and so is the directory.
  - The original mode is kept, and a new file is created `0600`.
  - Temps use a bot-only prefix (`.discord-agents.mcp_config.json.tmp.*`), so agy's own `*.tmp.<n>` files are never reaped.
  - Outcomes aren't fed to Disk_health: a failure on this foreign path would otherwise hold the bot read-only.
  - agy and the IDE don't take that lock, so a change in the final instant before the rename can still be lost.
- `~/.gemini/config/` must already exist: run agy once. The bot doesn't create agy's state.
- The bot sets `DISCORD_AGENTS_MCP_FOR_AGY` to the resolved server, and only on its local agy children. Every child's inherited copy is stripped first.
- With the variable unset (the user's own agy), the server exits at once, and agy's turn still succeeds (verified).
- Processes agy itself starts inherit the variable. A nested `agy` run from inside an agy session also gets the tools, under the parent's thread id. That's acceptable under the single-tenant trust model.
- MCP availability is decided per turn. The first turn's bot-context carries the full notice when the tools are missing. Later turns of a channel session carry a one-line status, so a first-turn "unavailable" doesn't stick once the tools come back.
- `--add-dir` together with `--conversation` on a resumed turn: verified (same conversation id, tools in the workspace).
- Verified end to end: agy called `list_sessions` on the live bot through `call_mcp_tool`, with the caller's thread id intact.
- The variable holds a shell-quoted command line. Locally it is the server path. On an executor it is the writer-ssh command that the Claude, Codex and Gemini remote paths use (`remote_mcp_command`), which carries the thread id back to the writer.
- **Remote agy turns:** the executor's agy reads its own `~/.gemini/config/mcp_config.json`. Before each remote agy turn, the bot installs the same entry there under the same rules:
  - it reads the file through a small remote script (missing config dir, symlink and non-regular file are all refused);
  - it decides with the same pure merge as locally;
  - it writes through a remote script that rewrites only if the file's MD5 is still what was read;
  - the file's mode is kept, and a new file is 0600;
  - temps use the bot-only prefix;
  - a bot-owned local lock serialises writes per host.

  The variable is forwarded through the remote run script's environment.
- The first released entry (`exec "$DISCORD_AGENTS_MCP_FOR_AGY"`, #164) is matched exactly and updated in place, keeping any keys added to it.
- **Remote read and write scripts.** The read comes back as one framed line (`DISCORD-AGENTS-AGY-CONFIG <md5> <base64>`), which the bot decodes and checks. Capture merges ssh's own stderr into the output, so a warning can't pass for the file. The write:
  - is checked against the new content's MD5, so truncated input is never installed;
  - takes the original's mode via `stat` (GNU or BSD), applied after the bytes are written and verified, so a read-only 0400 config works;
  - uses an exclusive `mktemp` temp and reaps only bot temps older than 10 minutes;
  - does all its work before the compare, so the race with the user's own writers is just the rename (`sync` is best-effort).

  The scripts need only POSIX sh plus `md5sum`/`md5`/`openssl`, `base64`, `mktemp`, GNU-or-BSD `stat`, and `find -maxdepth -mmin`, so macOS executors work.
- **Deploy order.** An older bot (#164 code) treats the new `eval` entry as someone else's and declines MCP for its own local agy until it's upgraded. Nothing is damaged, and rolling back is safe.
- **Cost.** A remote agy turn adds an ssh round trip (the read) before spawning, and a second one only when the entry needs writing.

### Workspace: `--add-dir` is required

With only the cwd set, agy reports "No workspace is open" and its file tools work in `~/.gemini/antigravity-cli/scratch` instead of the worktree (verified). `agy_args` passes `--add-dir <working_dir>`, and with that the tools act on the worktree.

## Remaining

1. Decide whether to detect a signed-out run early (stderr
   `Authentication required`) instead of waiting out agy's 60s auth
   timeout.
