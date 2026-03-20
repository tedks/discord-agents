# Discord Agents

Discord harness for AI agents (Claude, Codex, Gemini), written in OCaml.

## Architecture

- **discord-agents binary**: Long-running Discord bot process
- **Project discovery**: Scans configured base directories for git repos (bare or normal)
- **Agent sessions**: Each session = Discord thread + agent subprocess + git worktree
- **I/O bridge**: Messages flow Discord thread <-> agent stdin/stdout

## Building

```bash
nix develop --command dune build
```

## Testing

```bash
nix develop --command dune test
```

## Project layout

```
bin/main.ml              — Entry point
lib/config.ml            — Configuration (JSON file + env)
lib/project.ml           — Project discovery + worktree management
lib/agent_process.ml     — Agent subprocess lifecycle
lib/session.ml           — Session (thread + process bridge)
lib/discord_types.ml     — Discord API types
lib/discord_rest.ml      — Discord REST client
lib/discord_gateway.ml   — Discord WebSocket gateway
lib/bot.ml               — Top-level orchestrator
```

## Discord channel layout

- **Control channel**: Server-wide commands (`!projects`, `!start`, `!sessions`)
- **Project channels**: One per project (auto-created or manually mapped)
- **Session threads**: One per agent session, created in project channels

## Commands

- `!projects` — list discovered projects
- `!sessions` — list active agent sessions
- `!start <project> <claude|codex|gemini>` — start an agent
- `!stop <thread_id>` — stop a session
- `!help` — command reference
