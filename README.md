# Discord Agents

A Discord bot that bridges AI coding agents (Claude, Codex, Gemini) into Discord channels. Each project gets its own channel, each task gets its own thread with an isolated git worktree, and agent output streams to Discord in real time.

Written in OCaml 5.3 with [Eio](https://github.com/ocaml-multicore/eio) for structured concurrency.

## Features

- **Multi-agent support** -- Claude, Codex, and Gemini, selectable per session
- **Project discovery** -- Scans configured directories for git repos, deduplicates by remote URL
- **Isolated worktrees** -- Each session gets its own git worktree so agents don't stomp on each other
- **Streaming output** -- Agent responses appear incrementally, edited in place every 2 seconds
- **Session persistence** -- Thread-to-session mappings survive bot restarts
- **Channel management** -- Project channels created on demand, active ones float to the top
- **Tool display** -- Syntax-highlighted diffs, commands, and file contents shown inline with emoji status indicators
- **Message queueing** -- Messages sent during processing are queued and handled in order
- **Graceful restart** -- `!restart` drains active sessions, rebuilds from source, and respawns

## Requirements

- [Nix](https://nixos.org/) (for development environment)
- A Discord bot token with message content intent enabled
- Claude Code CLI installed (for Claude agent sessions)

## Building

```bash
nix develop --command dune build
```

## Running

```bash
nix develop --command dune exec discord-agents
```

Configuration lives at `~/.config/discord-agents/config.json`:

```json
{
  "discord_token": "your-bot-token",
  "guild_id": "your-server-id",
  "base_dirs": ["~/Projects"],
  "control_channel_id": "optional-channel-id"
}
```

Or set `DISCORD_BOT_TOKEN` as an environment variable.

### Smoke test

```bash
nix develop --command dune exec discord-agents -- --test CHANNEL_ID
```

## Commands

All commands use a `!` prefix:

| Command | Description |
|---------|-------------|
| `!start <project> [agent]` | Start a session (fuzzy-matches project name, agent defaults to Claude) |
| `!start` | Show numbered project list |
| `!resume <session_id>` | Resume an existing Claude Code session |
| `!projects` | List discovered projects |
| `!sessions` | List active bot sessions |
| `!claude-sessions` | List recent Claude Code sessions on this machine |
| `!stop <thread_id>` | Stop a session |
| `!rename [thread_id] <name>` | Rename a thread |
| `!desktop` | Set line wrapping to desktop width (120 chars) |
| `!mobile` | Set line wrapping to mobile width (60 chars) |
| `!wrapping [n]` | Show or set line wrap width |
| `!status` | Bot status and running processes |
| `!cleanup` | Delete stale project channels |
| `!restart` | Rebuild and restart the bot |
| `!help` | Command reference |

In the control channel and project channels, non-command messages are routed to a Claude session automatically.

## Architecture

```
bin/main.ml              -- Entry point, signal handling, pidfile
lib/bot.ml               -- Top-level orchestrator, command routing
lib/agent_runner.ml      -- Agent lifecycle, streaming output to Discord
lib/agent_process.ml     -- Subprocess management, stream-json parsing, formatting
lib/session_store.ml     -- Session state, persistence, message queue
lib/command.ml           -- Command parsing (pure, no I/O)
lib/discord_rest.ml      -- Discord REST client (cohttp-eio + TLS)
lib/discord_gateway.ml   -- Discord WebSocket gateway (heartbeat, resume)
lib/websocket.ml         -- Minimal RFC 6455 WebSocket client over tls-eio
lib/discord_types.ml     -- Discord API types (ppx_yojson_conv)
lib/config.ml            -- Configuration (JSON file + env vars)
lib/project.ml           -- Project discovery, dedup, worktree management
lib/channel_manager.ml   -- Channel creation, ordering, cleanup
```

### How it works

1. Bot connects to Discord via a custom WebSocket gateway implementation
2. On startup, scans `base_dirs` for git repos and creates project channels
3. `!start` creates a thread + git worktree + agent subprocess
4. Messages in threads are forwarded to the agent with context headers
5. Agent output streams back via `--output-format stream-json`, parsed and formatted for Discord
6. Tool use events show as emoji status lines with syntax-highlighted detail blocks
7. Tables are reformatted into padded code blocks since Discord doesn't render markdown tables

### Discord channel layout

- **Control channel** -- Natural language chat with a Claude session that has system-level capabilities
- **Project channels** -- One per discovered project, under an "Agent Projects" category
- **Session threads** -- Created by `!start`, each with its own worktree and agent subprocess

## Testing

```bash
nix develop --command dune runtest
```

## License

[GNU Affero General Public License v3.0](LICENSE)
