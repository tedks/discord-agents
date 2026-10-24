# Setup Guide

## 1. Install Nix

If you don't have Nix installed:

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Enable flakes (if not already):

```bash
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

Log out and back in (or restart your shell) for the changes to take effect.

## 2. Install Claude Code

discord-agents uses the Claude Code CLI to run agent sessions. Install it:

```bash
npm install -g @anthropic-ai/claude-code
```

Make sure `claude` is on your PATH and you've authenticated (`claude` will prompt on first run).

Gemini sessions are optional and **unmaintained**: Google no longer
offers the consumer OAuth sign-in Gemini CLI relied on, so only API-key
auth (`GEMINI_API_KEY` exported in the environment that starts the bot)
works. If you still want them, install `@google/gemini-cli` version
0.40.0 or newer; older Gemini CLI releases reject the `--skip-trust`
flag that discord-agents uses for headless worktree runs.

## 3. Create a Discord bot

1. Go to the [Discord Developer Portal](https://discord.com/developers/applications)
2. Click **New Application**, give it a name
3. Go to **Bot** in the sidebar
4. Click **Reset Token** and copy the token -- you'll need it for configuration
5. Under **Privileged Gateway Intents**, enable:
   - **Message Content Intent** (required -- the bot reads message text)
   - **Server Members Intent** (optional)
6. Go to **OAuth2 > URL Generator**
7. Select scopes: `bot`
8. Select permissions: `Send Messages`, `Manage Messages`, `Manage Channels`, `Manage Threads`, `Read Message History`, `Add Reactions`, `Use Slash Commands`
9. Copy the generated URL and open it in your browser to invite the bot to your server

## 4. Get your guild ID

1. In Discord, go to **User Settings > Advanced** and enable **Developer Mode**
2. Right-click your server name and click **Copy Server ID**

## 5. Configure discord-agents

```bash
mkdir -p ~/.config/discord-agents
```

Create `~/.config/discord-agents/config.json`:

```json
{
  "discord_token": "your-bot-token-from-step-3",
  "guild_id": "your-server-id-from-step-4",
  "base_directories": ["~/Projects"]
}
```

- `discord_token` -- The bot token from step 3. Alternatively, set the `DISCORD_BOT_TOKEN` environment variable.
- `guild_id` -- Your Discord server ID from step 4.
- `base_directories` -- List of directories to scan for git repos. Each repo becomes a project channel. `base_dirs` is accepted as a legacy alias.
- `control_channel_id` (optional) -- A specific channel ID for the control channel. If omitted, the bot creates one.
- `hosts` (optional) -- Multi-host setup (see `docs/multi-host.md`). Leave it out for a single machine, and the bot treats the machine it runs on as the only host. When it is set, `hosts.writer` names the one machine allowed to run the bot. The bot refuses to start anywhere else, so two bots never answer the same Discord message. Override that with `--force-writer` or `DISCORD_AGENTS_FORCE_WRITER=1` (also `true`, `yes` or `on`). Sessions started under an override are recorded as running on the machine that actually ran them, not on the configured writer. Remote executors are listed under `hosts.executors` but cannot run sessions yet; that arrives in later PRs. A binary older than this field refuses to start with a `hosts` block present. The bot rewrites `config.json` when it creates channels, and the rewrite normalises this block: a local entry for the writer appears under `executors` if there wasn't one.

  Host names compare case-insensitively. A short name matches a fully-qualified one by its first label (`tower0` = `tower0.local`), but two fully-qualified names must match exactly. So if this machine's `hostname` prints `tower0.lan`, write `"writer": "tower0"`, not `tower0.local`. The bot also refuses to start if the machine's hostname isn't a usable host name (letters, digits, `.`, `_`, `-`, at most 64 characters).

  ```json
  "hosts": {
    "writer": "tower0",
    "executors": {
      "framework0": { "ssh": "framework0", "writer_ssh": "tower0" }
    }
  }
  ```

## 6. Build and run

```bash
cd /path/to/discord-agents
nix develop --command dune build
nix develop --command dune exec discord-agents
```

The bot will:
1. Connect to Discord
2. Create an "Agent Projects" category if it doesn't exist
3. Scan your `base_directories` for git repos and create a channel for each
4. Set up a control channel with a Claude session

### Smoke test

To verify the bot can connect and send messages without running the full gateway:

```bash
nix develop --command dune exec discord-agents -- --test CHANNEL_ID
```

Replace `CHANNEL_ID` with any channel ID from your server.

## 7. Running as a service

discord-agents uses a pidfile at `~/.config/discord-agents/discord-agents.pid` to ensure only one instance runs. Starting a new instance will signal the old one to shut down.

### systemd (recommended)

Create `~/.config/systemd/user/discord-agents.service`:

```ini
[Unit]
Description=Discord Agents Bot
After=network-online.target

[Service]
Type=simple
WorkingDirectory=/path/to/discord-agents
ExecStart=/path/to/discord-agents/nix-run.sh
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

Create a wrapper script (`nix-run.sh`):

```bash
#!/bin/bash
nix develop --command dune build
exec nix develop --command dune exec discord-agents
```

```bash
chmod +x nix-run.sh
systemctl --user daemon-reload
systemctl --user enable --now discord-agents
```

### Self-restart

The bot supports `!restart` from Discord, which:
1. Drains active sessions (waits for in-progress work to finish)
2. Rebuilds from source (`dune build`)
3. Spawns a new process and exits

This means you can deploy code changes by pushing to the repo and typing `!restart` in Discord.
