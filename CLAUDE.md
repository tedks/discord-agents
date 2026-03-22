# Discord Agents

Discord harness for AI agents (Claude, Codex, Gemini), written in OCaml 5.3 with Eio.

## Architecture

- **discord-agents binary**: Long-running Discord bot process
- **Project discovery**: Scans configured base directories for git repos, deduplicates by remote URL
- **Agent sessions**: Each session = Discord thread + Claude subprocess + git worktree
- **I/O bridge**: Messages stream Discord ↔ agent via Eio fibers with real-time output
- **WebSocket gateway**: Custom RFC 6455 implementation over tls-eio (not httpun-ws)
- **REST client**: cohttp-eio with TLS, rate limiting, all Discord endpoints

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

## Project layout

```
bin/main.ml              — Entry point, Eio_main.run, signal handling, pidfile
lib/config.ml            — Configuration (JSON file + DISCORD_BOT_TOKEN env)
lib/project.ml           — Project discovery, dedup by remote, worktree management
lib/agent_process.ml     — Eio.Process subprocess, stream-json parsing
lib/session.ml           — Session state types
lib/discord_types.ml     — Discord API types (ppx_yojson_conv)
lib/discord_rest.ml      — Discord REST client (cohttp-eio + TLS)
lib/discord_gateway.ml   — Discord WebSocket gateway (heartbeat, resume)
lib/websocket.ml         — Minimal RFC 6455 WebSocket client over tls-eio
lib/bot.ml               — Top-level orchestrator, command routing, channel management
```

## Discord channel layout

- **Control channel**: Natural language chat + commands. Claude session auto-created with system prompt describing all capabilities.
- **Project channels**: Created on demand under "Agent Projects" category. Chat auto-creates a Claude session scoped to that project.
- **Session threads**: Created by `start` command. Each gets its own worktree and Claude session.

## Commands

Commands work with or without `!` prefix, from any channel:

- `start <project> [agent]` — start a session (agent defaults to claude, fuzzy matches project name)
- `start` — show numbered project list
- `resume <session_id>` — resume an existing Claude Code session
- `projects` — list discovered projects with numbers
- `sessions` — list active bot sessions
- `claude-sessions` — list recent Claude Code sessions on this machine
- `stop <thread_id>` — stop a session
- `cleanup` — delete stale project channels
- `restart` — rebuild and restart the bot
- `help` — command reference

In the control channel and project channels, non-command messages are routed to a Claude session automatically. No need for explicit commands to just chat.

## Key behaviors

- **Pidfile lock** (`~/.config/discord-agents/discord-agents.pid`): Only one instance runs. New startup kills the old one.
- **Session persistence** (`~/.config/discord-agents/sessions.json`): Thread ↔ session mappings survive restarts.
- **Streaming output**: Agent responses appear incrementally, message edited every 2s, typing indicator refreshed every 8s.
- **Channel reordering**: Active project channels float to the top of the category.
- **Worktrees**: Each `start` command creates an isolated git worktree (`agent/<kind>-<uuid>`).
- **Graceful shutdown**: SIGINT/SIGTERM posts "Bot shutting down" and closes cleanly.

## Bare repo / worktree setup

This repo uses a bare repo at `~/Projects/claude-discord/` with worktrees:
```
~/Projects/claude-discord/
├── .git/                    # bare git repo
├── master/                  # worktree for master branch
└── <feature-branch>/        # worktrees for feature branches
```

Always work inside a worktree, not the bare repo root. Use `git worktree add` to create new ones.
