# discord-agents

discord-agents is a Discord server interface for coding agents. Every channel is a project, and every agent session is a thread. Channels are connected to their own management agent sessions that can spawn new threads conversationally. You can drop in your existing projects or resume the sessions that are already on your machine. New sessions automatically get their own worktrees.

There is a `!command` interface to server management, but control channels also have access to an MCP that can perform all commands. See the commands section for a full list or ask your agent.

discord-agents is intended to be a simple interface to agentic coding on your personal machines and is not intended to be used in shared Discord servers. There is no authentication, and it has the full capabilities of a coding agent launched as the user you started it as. There is no sandboxing in discord-agents itself; use Unix and Claude sandboxing if you require it.

Only tested on GNU+Linux. Environment managed by Nix. Built with OCaml 5.3.

## Quickstart

```bash
# Build
nix develop --command dune build

# Configure
mkdir -p ~/.config/discord-agents
cat > ~/.config/discord-agents/config.json << 'EOF'
{
  "discord_token": "your-bot-token",
  "guild_id": "your-server-id",
  "base_dirs": ["~/Projects"]
}
EOF

# Run
nix develop --command dune exec discord-agents
```

Then in Discord:

```
!projects              # see what it found
!start myproject       # start a session (fuzzy-matches)
```

Or just chat in any project channel -- the agent will respond directly.

See [SETUP.md](SETUP.md) for detailed installation and Discord bot setup instructions.

## Commands

All commands use a `!` prefix:

| Command | Description |
|---------|-------------|
| `!start <project>` | Start a session (fuzzy-matches project name) |
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

All channels -- control and project -- have the same capabilities. The agent knows which channel it's in and has context about the associated project. Non-command messages are routed to the channel's Claude session automatically.

## Testing

```bash
nix develop --command dune runtest
```

## License

[GNU Affero General Public License v3.0](LICENSE)
