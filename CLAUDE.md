# Discord Agents

Discord harness for AI coding agents, written in OCaml 5.3 with Eio.

## Building

```bash
nix develop --command dune build
```

## Running

```bash
# Config at ~/.config/discord-agents/config.json
# Or use DISCORD_BOT_TOKEN env var
nix develop --command dune exec discord-agents

# Smoke test
nix develop --command dune exec discord-agents -- --test [CHANNEL_ID]
```

### Switching between branches at runtime

The bot acquires an exclusive lock on `~/.config/discord-agents/discord-agents.pid` (`bin/main.ml:24`). When a second instance starts, it reads the PID, sends SIGTERM (then SIGKILL after 2s if still alive), takes the lock, and runs. So switching branches doesn't require a manual stop — just launch the bot from the target worktree.

```bash
# Switch to a feature branch and run from it
scripts/run-branch.sh feat/codex-support

# Switch back
scripts/run-branch.sh master

# Path also works
scripts/run-branch.sh /home/me/Projects/claude-discord/feat/foo
```

`scripts/run-branch.sh` builds first so a compile error doesn't kill the running bot, then `exec`s the bot in the foreground. The handover is intentionally abrupt — any in-flight Claude/Codex/Gemini child processes attached to the previous bot get orphaned, and any active session threads (including the one you're typing in if you triggered the switch from inside the bot) lose their parent. Use `!restart` instead when you want a graceful drain on the same code.

## Testing

```bash
nix develop --command dune runtest
```

Always run tests after changes to formatting, command parsing, or agent_process.ml.

## Project layout

```
bin/main.ml              — Entry point, Eio_main.run, signal handling, pidfile
lib/bot.ml               — Top-level orchestrator, command routing, channel management
lib/agent_runner.ml      — Agent lifecycle, streaming output to Discord, typing indicator
lib/agent_process.ml     — Subprocess management, per-agent JSON parsing (Claude/Codex/Gemini), formatting, escaping
lib/session_store.ml     — Session state, persistence to disk, message queue
lib/command.ml           — Pure command parsing (no I/O)
lib/claude_sessions.ml   — Claude Code session discovery on disk (~/.claude/projects/)
lib/codex_sessions.ml    — Codex CLI session discovery on disk (~/.codex/sessions/)
lib/gemini_sessions.ml   — Gemini CLI session discovery on disk (~/.gemini/tmp/)
lib/channel_manager.ml   — Channel creation, ordering, cleanup
lib/config.ml            — Configuration (JSON file + DISCORD_BOT_TOKEN env)
lib/project.ml           — Project discovery, dedup by remote, worktree management
lib/discord_rest.ml      — Discord REST client (cohttp-eio + TLS)
lib/discord_gateway.ml   — Discord WebSocket gateway (heartbeat, resume)
lib/discord_types.ml     — Discord API types (ppx_yojson_conv)
lib/websocket.ml         — Minimal RFC 6455 WebSocket client over tls-eio
test/test_formatting.ml  — Tests for formatting, wrapping, escaping, command parsing
```

## Commands

Commands require a `!` prefix:

- `start <project> [agent]` — start a session (agent ∈ {claude, codex, gemini}; defaults to claude)
- `start` — show numbered project list
- `resume [agent] <session_id>` — resume an existing session (agent defaults to none; tries Claude → Codex → Gemini)
- `projects` — list discovered projects with numbers
- `sessions` — list active bot sessions
- `claude-sessions` — list recent Claude Code sessions on this machine
- `codex-sessions` — list recent Codex CLI sessions on this machine
- `gemini-sessions` — list recent Gemini CLI sessions on this machine
- `stop <thread_id>` — stop a session
- `rename [thread_id] <name>` — rename a thread
- `desktop` — set wrapping to desktop width (120 chars)
- `mobile` — set wrapping to mobile width (60 chars)
- `wrapping [n]` — show or set line wrap width
- `status` — bot status and running processes
- `refresh` — re-scan for new projects without restarting
- `cleanup` — delete stale project channels
- `restart` — rebuild and restart the bot
- `help` — command reference

Non-command messages in control/project channels are routed to a Claude session automatically.

## Key behaviors

- **Pidfile lock** (`~/.config/discord-agents/discord-agents.pid`): Only one instance runs.
- **Session persistence** (`~/.config/discord-agents/sessions.json`): Thread-session mappings survive restarts.
- **Streaming output**: Agent responses appear incrementally, message edited every 2s.
- **Typing indicator**: Background fiber sends typing every 8s for entire processing duration, stops within 1s of completion.
- **Emoji lifecycle**: 👀 on receipt, ✅ on success, ❌ on error. Messages sent during processing get ⏳ and are queued.
- **Message queue**: Queued messages drain sequentially via tail-recursive `process_message`. No depth limit.
- **Tool display**: Tool use events show as emoji status lines with syntax-highlighted detail blocks (diffs, commands, file content).
- **Channel reordering**: Active project channels float to the top of the category.
- **Worktrees**: Each `start` command creates an isolated git worktree (`agent/<kind>-<uuid>`).
- **Graceful restart**: `!restart` drains active sessions, rebuilds, and respawns.

## Discord formatting gotchas

- **Underscores**: `__text__` renders as underline, `_text_` as italic. Escape with `\_` in tool names and dynamic content.
- **Triple backticks**: ``` inside a code block closes the fence. Use zero-width space (`\xE2\x80\x8B`) between second and third backtick to escape. Applied automatically by `escape_code_fences` and `escape_nested_fences`.
- **GitHub shorthand**: `owner/repo#N` does NOT render as a clickable link. Always use full URLs (`https://github.com/...`).
- **Tables**: Discord doesn't render markdown tables. `reformat_tables` wraps them in padded code blocks.

## Agent JSON event schemas

Each agent has its own event schema; per-agent parsers in `lib/agent_process.ml` translate them all into a shared `stream_event` type (`Text_delta` / `Tool_use` / `Tool_result` / `Result` / `Error` / `Other`).

**Claude Code** (`--output-format stream-json`, parsed by `parse_stream_json_line`):
- `{"type": "assistant", "message": {"content": [...]}}` — text and tool_use blocks
- `{"type": "result", "result": "...", "session_id": "..."}` — final result

Tool results are NOT emitted as separate events; they're internal to Claude Code. We display tool inputs (diffs, commands) from tool_use blocks but cannot show tool output.

**Codex** (`exec --json`, parsed by `parse_codex_json_line`):
- `{"type":"thread.started","thread_id":"<uuid>"}` — server-allocated session id
- `{"type":"item.completed","item":{"type":"agent_message","text":"..."}}` — assistant text
- `{"type":"item.started/completed","item":{"type":"command_execution",...}}` — bash tool lifecycle
- `{"type":"item.completed","item":{"type":"file_change","changes":[...]}}` — files modified
- `{"type":"turn.failed","error":{"message":"..."}}` / `{"type":"error","message":"..."}` — surfaced as Error events
- `{"type":"turn.completed","usage":{...}}` — flush

**Gemini** (`-o stream-json --yolo`, parsed by `parse_gemini_stream_json_line`):
- `{"type":"init","session_id":"<uuid>","model":"..."}` — server-allocated session id
- `{"type":"message","role":"assistant","content":"...","delta":true}` — incremental text
- `{"type":"tool_use","tool_name":"run_shell_command","parameters":{...}}` — tool call
- `{"type":"tool_result","status":"success|error","output":"..."}` — Gemini *does* emit these
- `{"type":"result","status":"...","stats":{...}}` — flush

Gemini tool names (`run_shell_command`, `read_file`, `write_file`, `replace`, `search_file_content`, `glob`, `web_fetch`, `google_web_search`) are translated to Claude-equivalents (`Bash`, `Read`, `Write`, `Edit`, `Grep`, `Glob`, `WebFetch`, `WebSearch`) by `gemini_tool_name_of` so the runner's emoji/verb table stays agent-agnostic.

## Session id origin

`Config.caller_pinned_session_id` records, per agent, who allocates the session id:
- **Claude** — the bot pins it via `--session-id <uuid>`; the id is authoritative from creation.
- **Codex / Gemini** — the agent allocates server-side and emits it on first run (Codex's `thread.started`, Gemini's `init`); we capture it via `on_session_id` in `agent_runner` and persist via `Session_store.set_session_id`. The `session_id_confirmed` flag gates resume so a first-turn failure that occurred before/after the id was emitted is handled correctly.

## MCP configuration

The bot exposes MCP tools (start_session, list_sessions, etc.) via `scripts/mcp-server.py`. All three agents now pick it up — each through a different mechanism:

- **Claude** — `--mcp-config <path>` flag; we write the config to `~/.config/discord-agents/mcp-generated.json`.
- **Codex** — TOML overrides via `-c key=value` per invocation: `mcp_servers.discord_agents.command="python3"` and `mcp_servers.discord_agents.args=["..."]`. Doesn't touch the user's `~/.codex/config.toml`.
- **Gemini** — has no `--mcp-config` flag; loads `mcpServers` from `<cwd>/.gemini/settings.json`. The bot merges its entry into any existing settings file (preserving the user's other MCP servers and unrelated keys) at session start and appends `.gemini/` to the worktree's `.git/info/exclude`.

For non-default install layouts where the script doesn't live next to the executable, set `DISCORD_AGENTS_MCP_SCRIPT=/path/to/mcp-server.py` to override the heuristic search.

## System prompts

Per-session system prompts (set via `Session_store.session.system_prompt`) tell the agent what role it's playing and which MCP tools it has. Forwarded to each agent through its native mechanism:

- **Claude** — `--append-system-prompt <text>` flag (persistent across all turns).
- **Codex / Gemini** — neither CLI exposes a system-instruction flag, so the bot prepends the prompt as a `<bot-context>...</bot-context>` block to the user's first-turn message. The conversation history carries it forward on subsequent turns. See `Agent_process.compose_session_prompt`.

Today only the control/project channel sessions (created by `ensure_channel_session`) set a system prompt, and those are hardcoded to Claude. The Codex/Gemini paths are wired and tested so a future feature could back a control/project channel with a different agent without further plumbing.

## Bare repo / worktree setup

This repo uses a bare repo at `~/Projects/claude-discord/` with worktrees:
```
~/Projects/claude-discord/
├── .git/                    # bare git repo
├── master/                  # worktree for master branch
└── <feature-branch>/        # worktrees for feature branches
```

Always work inside a worktree, not the bare repo root. Use `git worktree add` to create new ones.
