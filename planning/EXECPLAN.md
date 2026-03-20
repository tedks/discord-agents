# Discord Agents: Full Implementation ExecPlan

This ExecPlan is a living document. The sections `Progress`, `Surprises & Discoveries`, `Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work proceeds. This document must be maintained in accordance with `PLANS.md` at the repository root.

## Purpose / Big Picture

After this work is complete, a user can run a single compiled binary (`discord-agents`) on any machine where Claude, Codex, and/or Gemini CLIs are installed. The binary connects to a Discord server and lets the user manage AI coding agent sessions entirely from Discord. The user types `!start myproject claude` in a control channel, and the bot creates a git worktree, spawns a Claude Code subprocess, creates a Discord thread, and bridges all messages between the thread and the agent. The user drives the agent from their phone, tablet, or any Discord client. Multiple agents can run concurrently across different projects.

The system scans configured base directories (like `~/Projects`) to discover git repositories automatically. Each project gets a Discord channel. Each agent session gets a Discord thread inside that channel. The control channel provides a birds-eye view of all projects and sessions.

## Progress

- [x] (2026-03-20 19:30Z) Project scaffolding: bare repo, Nix flake, dune build, module skeleton
- [x] (2026-03-20 19:35Z) Type definitions for Discord types, config, agent kinds
- [x] (2026-03-20 19:35Z) Stub implementations for all modules (compiles, no runtime behavior)
- [ ] Milestone 1: Discord REST client (cohttp-eio HTTP, JSON round-trip)
- [ ] Milestone 2: Discord Gateway client (httpun-ws WebSocket, heartbeat, identify)
- [ ] Milestone 3: Agent subprocess management (Eio-native process spawning, stream-json parsing)
- [ ] Milestone 4: Session I/O bridge (Eio fibers bridging agent output to Discord threads)
- [ ] Milestone 5: Bot orchestration (channel/thread creation, command routing, project channels)
- [ ] Milestone 6: End-to-end integration and hardening

## Surprises & Discoveries

(None yet — to be updated as work proceeds.)

## Decision Log

- Decision: Use OCaml 5.3 with Eio for concurrency instead of Lwt.
  Rationale: Eio provides direct-style (no monadic) structured concurrency on OCaml 5 multicore. All key libraries (cohttp-eio, httpun-ws-eio, tls-eio) have Eio backends in nixpkgs. Direct style is easier to read and debug.
  Date/Author: 2026-03-20 / Ted + Claude

- Decision: Build a thin custom Discord client instead of using discordml.
  Rationale: discordml is dormant (last commit Nov 2023), has heavy voice/sodium/ctypes dependencies we do not need, and is not on opam. Our needs are text-only: messages, channels, threads, reactions. The cohttp-eio + httpun-ws stack covers HTTP and WebSocket. Writing the Discord protocol layer ourselves gives us full control and avoids dead dependencies.
  Date/Author: 2026-03-20 / Ted + Claude

- Decision: Use stream-json output format for Claude and Gemini CLIs, and pipe-based I/O for Codex.
  Rationale: Claude and Gemini both support `--output-format stream-json` which emits newline-delimited JSON objects as the agent works. This lets us relay partial output to Discord in real time instead of waiting for the full response. Codex uses a different model (interactive terminal) and will need a PTY-based approach.
  Date/Author: 2026-03-20 / Ted + Claude

## Outcomes & Retrospective

(To be filled at completion.)

## Context and Orientation

This is an OCaml project that builds a Discord bot for orchestrating AI coding agents. The repository lives at `~/Projects/claude-discord/` as a bare git repo, with the working tree at `~/Projects/claude-discord/master/` on the `main` branch. The remote is `github.com/tedks/discord-agents`.

The build system is dune 3.14, with a Nix flake (`flake.nix`) managing the OCaml 5.3 toolchain and all dependencies. To build: `nix develop --command dune build`. The compiled binary lands at `_build/default/bin/main.exe`.

The project has two dune packages: a library (`lib/`, named `discord_agents`) and a binary (`bin/`, named `discord-agents`). The library contains these modules:

- `lib/config.ml` — Configuration type and JSON serialization. Defines `agent_kind` (Claude, Codex, Gemini), `project` (name, path, channel_id), and the top-level config (discord_token, base_directories, guild_id, control_channel_id, projects). Loads from `~/.config/discord-agents/config.json`.

- `lib/project.ml` — Discovers git repositories in base directories. A "project" is a directory containing `.git` (normal repo) or `HEAD`+`objects/`+`refs/` (bare repo). Provides `list_worktrees` (parses `git worktree list --porcelain`) and `create_worktree` (runs `git worktree add -b`).

- `lib/discord_types.ml` — Discord API types with `ppx_yojson_conv` deriving for automatic JSON serialization. Defines `user`, `channel`, `message`, `attachment`, `guild`, gateway opcodes, intent bitflags, and channel types. The `yojson.allow_extra_fields` attribute means we can deserialize Discord's verbose JSON without listing every field.

- `lib/discord_rest.ml` — REST API client. Currently a stub: the `request` function returns `Error "not yet implemented"`. Has the right function signatures for `create_message`, `create_channel`, `create_thread`, `create_thread_no_message`, `get_messages`, and `create_reaction`. Each constructs the correct JSON body shape.

- `lib/discord_gateway.ml` — WebSocket gateway client. Currently a stub: `connect` logs and returns. Has working payload parsing (`handle_payload`) that dispatches READY, MESSAGE_CREATE, and THREAD_CREATE events. Has `identify_payload` and `heartbeat_payload` builders. Tracks sequence numbers and session_id.

- `lib/agent_process.ml` — Subprocess management using Unix pipes. Spawns a process with `Unix.create_process_env`, provides `send_input` (write to stdin), `read_output` (non-blocking read via `Unix.select`), `check_status` (non-blocking `waitpid`), and `stop` (close stdin, wait). Currently uses raw `Unix` module; needs conversion to Eio.

- `lib/session.ml` — Bridges a Discord thread to an agent subprocess. Owns a `Session.t` with project name, agent kind, thread ID, worktree path, process handle, and state machine (Starting -> Active -> Stopping -> Stopped).

- `lib/bot.ml` — Top-level orchestrator. Parses `!commands` from the control channel, routes thread messages to sessions, manages session lifecycle. Uses `SessionMap` (thread_id -> session).

- `bin/main.ml` — Entry point. Sets up logging, loads config, creates bot, calls `Bot.run`.

Key libraries available in the Nix shell (all have Eio backends):

- `cohttp-eio` (6.2.1) — HTTP client. `Cohttp_eio.Client.make` creates a client; `get`/`post`/`put`/`patch`/`delete` make requests; responses give `Http.Response.t` and `Eio.Flow.source` body.

- `httpun-ws-eio` — WebSocket client over Eio. `Httpun_ws_eio.Client.connect` takes a socket, nonce, host, port, resource, error_handler, and websocket_handler callback. The callback receives a `Wsd.t` for sending and returns `input_handlers` with `frame` and `eof` callbacks.

- `tls-eio` — TLS for Eio sockets. `Tls_eio.client_of_flow` wraps a raw socket into a TLS socket.

- `yojson` + `ppx_yojson_conv` — JSON serialization with compile-time deriving.

- `eio_main` — Eio event loop. `Eio_main.run` starts the scheduler, providing `env` with access to `net`, `clock`, `fs`, etc.

### Discord Protocol Summary

The Discord Gateway uses WebSocket at `wss://gateway.discord.gg/?v=10&encoding=json`. Every message is a JSON envelope: `{"op": <int>, "d": <payload>, "s": <seq|null>, "t": <event_name|null>}`.

Connection flow: (1) connect WebSocket, (2) receive Hello (op 10) with `heartbeat_interval`, (3) start heartbeat fiber sending op 1 every interval, (4) send Identify (op 2) with token and intents, (5) receive Ready (op 0, t="READY") with session_id and resume_gateway_url. Cache both for reconnection.

Key events: MESSAGE_CREATE (op 0, t="MESSAGE_CREATE") delivers messages, THREAD_CREATE delivers new threads, GUILD_CREATE fires on connect with full guild data.

REST base: `https://discord.com/api/v10`. All requests need `Authorization: Bot <token>`. Key endpoints:
- `POST /channels/{id}/messages` — send message (body: `{content, message_reference}`)
- `POST /guilds/{id}/channels` — create channel (body: `{name, type, parent_id, topic}`)
- `POST /channels/{id}/threads` — create thread without message (body: `{name, type, auto_archive_duration}`)
- `POST /channels/{id}/messages/{id}/threads` — create thread from message
- `GET /channels/{id}/messages?limit=N` — fetch history
- `PUT /channels/{id}/messages/{id}/reactions/{emoji}/@me` — add reaction
- `PATCH /channels/{id}/messages/{id}` — edit message

Messages over 2000 characters must be split before sending.

Intents bitfield for our needs: `GUILDS (1) | GUILD_MESSAGES (512) | GUILD_MESSAGE_REACTIONS (1024) | DIRECT_MESSAGES (4096) | MESSAGE_CONTENT (32768) = 38401`. MESSAGE_CONTENT is a privileged intent that must be enabled in the Discord developer portal.

### Agent CLI Summary

**Claude Code** (`claude`): Use `claude -p "<prompt>" --output-format stream-json` for non-interactive single-prompt mode. The `stream-json` format emits newline-delimited JSON objects: `{"type":"assistant","message":{"content":[{"type":"text","text":"..."}]}}` as the model streams. For interactive sessions, `claude --output-format stream-json` reads from stdin line by line. Use `--resume <session_id>` to continue a previous session. Key flags: `--allowedTools`, `--disallowedTools`, `--model`, `--max-turns`.

**Gemini CLI** (`gemini`): Use `gemini -p "<prompt>" --output-format stream-json` — same pattern as Claude. Flags: `--model`, `--sandbox`, `--yolo` (auto-approve), `--approval-mode`. Supports `--resume`.

**Codex** (`codex`): Use `codex --quiet --full-auto "<prompt>"` for non-interactive mode. Does not support stream-json; outputs plain text to stdout. For ongoing interaction, needs a PTY (pseudo-terminal) since it uses a TUI. We will start with non-interactive one-shot mode for Codex and iterate.

## Plan of Work

The work is organized into six milestones, each independently verifiable. Each milestone builds on the previous one but can be tested in isolation using the validation steps described.

### Milestone 1: Discord REST Client

The goal is to make the `discord_rest.ml` module functional: it should make real HTTP requests to Discord's API and return parsed responses. After this milestone, we can send messages, create channels, and create threads from OCaml.

The `request` function in `lib/discord_rest.ml` currently returns `Error "not yet implemented"`. We need to replace it with a real HTTP client using `cohttp-eio`. The key change is that `discord_rest.t` must carry an `Eio` environment and HTTP client handle, and all functions become Eio-aware (they run inside `Eio_main.run`).

Concrete changes:

In `lib/discord_rest.ml`, change the type `t` to include a cohttp-eio client:

    type t = {
      token : string;
      client : Cohttp_eio.Client.t;
    }

Add a `create` function that takes `env` (the Eio environment):

    let create ~token env =
      let authenticator = Ca_certs.authenticator () |> Result.get_ok in
      let tls_config = Tls.Config.client ~authenticator () |> Result.get_ok in
      let https uri raw =
        let host = Uri.host uri |> Option.map (fun x ->
          Domain_name.(host_exn (of_string_exn x))) in
        Tls_eio.client_of_flow ?host tls_config raw
      in
      let client = Cohttp_eio.Client.make ~https:(Some https) (Eio.Stdenv.net env) in
      { token; client }

Replace the `request` function with a real implementation that:
1. Builds the full URL from `api_base ^ path`
2. Sets Authorization, Content-Type, and User-Agent headers
3. Optionally serializes the JSON body via `Yojson.Safe.to_string`
4. Calls `Cohttp_eio.Client.post` (or get, put, patch, delete based on `meth`)
5. Reads the response body via `Eio.Buf_read.parse_exn take_all`
6. Checks the HTTP status; on 2xx, parses the body as JSON and returns `Ok json`
7. On 429 (rate limited), logs the Retry-After header and retries after sleeping
8. On other errors, returns `Error` with status code and body

Each endpoint function (`create_message`, `create_channel`, etc.) calls `request` and deserializes the response JSON into the appropriate `discord_types` record using the existing `_of_yojson` functions.

This milestone also requires adding `ca-certs` and `tls` to the dune library dependencies (they are already in the Nix shell via `tls-eio`).

**Validation**: Write a small test binary (or inline test) that loads a Discord token from the environment, sends a message to a known channel, and prints the response. Run it and observe the message appearing in Discord.

### Milestone 2: Discord Gateway Client

The goal is to connect to Discord's WebSocket gateway, authenticate, maintain heartbeats, and receive real-time events (messages, thread creates). After this milestone, the bot can log in, show as "online" in Discord, and receive messages.

The `connect` function in `lib/discord_gateway.ml` currently does nothing. We need to implement the full WebSocket connection lifecycle using `httpun-ws-eio`.

Concrete changes:

Add `resume_gateway_url` to the gateway state type:

    type t = {
      ...
      mutable resume_gateway_url : string option;
      mutable heartbeat_interval_ms : int;
      mutable last_heartbeat_acked : bool;
    }

The `connect` function must:

1. Resolve `gateway.discord.gg` via DNS using `Eio.Net.getaddrinfo`.
2. Open a TCP socket with `Eio.Net.connect`.
3. Wrap it in TLS using `Tls_eio.client_of_flow` (Discord requires WSS).
4. Call `Httpun_ws_eio.Client.connect` with the TLS socket, providing:
   - `host: "gateway.discord.gg"`, `port: 443`, `resource: "/?v=10&encoding=json"`
   - A `websocket_handler` callback that receives `Wsd.t` and returns `input_handlers`
5. Inside `websocket_handler`:
   a. Store the `Wsd.t` in the gateway state for sending.
   b. Fork a heartbeat fiber using `Eio.Fiber.fork_daemon ~sw`.
   c. Return `input_handlers` where the `frame` callback reassembles text frames, parses JSON via `Yojson.Safe.from_string`, and calls `handle_payload`.
6. In `handle_payload`, when Hello (op 10) is received:
   a. Extract `heartbeat_interval` from `d`.
   b. Send the first heartbeat after `interval * random_jitter` milliseconds.
   c. Send Identify (op 2) with the token and intents.
7. The heartbeat daemon fiber sleeps for `heartbeat_interval` ms, sends op 1 with the current sequence number, checks that the previous heartbeat was ACKed. If not ACKed, close the connection and trigger reconnection.
8. When Heartbeat ACK (op 11) is received, set `last_heartbeat_acked = true`.
9. When Reconnect (op 7) or Invalid Session (op 9) is received, trigger reconnection.

For reconnection (can be deferred to Milestone 6 for hardening, but the plumbing should exist):
1. Connect to `resume_gateway_url` instead of the default gateway.
2. After Hello, send Resume (op 6) with `token`, `session_id`, and last `sequence`.
3. If resume fails (op 9 with `d: false`), re-identify on the original gateway.

The gateway `connect` function should take `env` and `sw` (the Eio switch) so it can fork fibers and use the network.

The `Wsd.t` sending interface uses `Wsd.send_bytes wsd ~kind:\`Text msg ~off:0 ~len`. Wrap this in a `send_json` helper:

    let send_json wsd json =
      let s = Yojson.Safe.to_string json in
      let b = Bytes.of_string s in
      Httpun_ws.Wsd.send_bytes wsd ~kind:`Text b ~off:0 ~len:(Bytes.length b)

Frame reassembly: Discord sends text frames that may be split across multiple WebSocket frames. Buffer incoming text in a `Buffer.t` and parse when `is_fin` is true.

**Validation**: Run the binary with a valid Discord token and guild. Observe: (1) the bot appears online in Discord, (2) log output shows "gateway: received Hello", "gateway: connected as BotName#1234", (3) sending a message in an opted-in channel produces a log line showing the message content.

### Milestone 3: Agent Subprocess Management

The goal is to replace the current Unix-pipe-based `agent_process.ml` with an Eio-native implementation that properly handles Claude, Codex, and Gemini CLIs in non-interactive mode with stream-json output parsing.

Currently `agent_process.ml` uses raw `Unix.pipe`, `Unix.create_process_env`, and `Unix.select`. This is incompatible with Eio's event loop (blocking calls on raw file descriptors will block the Eio scheduler). We need to use Eio's process spawning.

Concrete changes:

Replace `agent_process.ml` with Eio process management. Eio provides `Eio.Process.spawn` for running subprocesses with managed I/O:

    let proc = Eio.Process.spawn ~sw mgr
      ~cwd:(Eio.Path.(env#fs / working_dir))
      ~stdin:child_stdin
      ~stdout:child_stdout
      ~stderr:child_stderr
      [cmd; args...]

For Claude and Gemini, the command is:

    claude -p "" --output-format stream-json --verbose
    gemini -p "" --output-format stream-json

These run in streaming mode, emitting newline-delimited JSON to stdout. Each line is a JSON object with a `type` field. The key types are:
- `{"type":"assistant","message":{"content":[{"type":"text","text":"partial output"}]}}` — model output
- `{"type":"result","result":"final text","session_id":"..."}` — completion

For Codex:

    codex --quiet --full-auto "<prompt>"

Codex writes plain text to stdout. We treat its entire stdout as the response.

The `t` type changes to carry Eio handles instead of Unix file descriptors:

    type t = {
      kind : Config.agent_kind;
      process : Eio.Process.t;
      stdin_sink : Eio.Flow.sink_ty Eio.Resource.t;
      stdout_source : Eio.Flow.source_ty Eio.Resource.t;
      stderr_source : Eio.Flow.source_ty Eio.Resource.t;
      working_dir : string;
      mutable status : status;
    }

Add a `stream_json_parser` that reads lines from stdout and yields parsed events:

    let read_stream_json source =
      let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) source in
      fun () ->
        match Eio.Buf_read.line reader with
        | line -> Some (Yojson.Safe.from_string line)
        | exception End_of_file -> None

For plain text output (Codex), read chunks directly.

**Validation**: Write a test that spawns `claude -p "say hello" --output-format stream-json`, parses the stream-json output, and prints each event type. Verify it captures the assistant response and final result.

### Milestone 4: Session I/O Bridge

The goal is to connect the agent subprocess output to Discord threads in real time, and route Discord thread messages back to the agent's stdin. After this milestone, typing in a Discord thread sends input to the agent, and the agent's output appears in the thread.

This is the core value of the system: bidirectional I/O bridging between Discord and agent subprocesses, running as concurrent Eio fibers.

Concrete changes:

In `lib/session.ml`, the `start` function should fork two fibers:

1. **Output relay fiber** (agent stdout -> Discord thread): Reads from the agent's stdout continuously. For stream-json agents (Claude, Gemini), parses each JSON line and extracts the text content. Buffers output and flushes to Discord at natural boundaries (paragraph breaks, or every 1-2 seconds, or when the buffer exceeds ~1500 chars to stay under Discord's 2000-char limit). Uses `Discord_rest.create_message` to post each chunk to the session's thread. For progress/tool-use events, optionally posts a condensed status update.

2. **Input relay fiber** (Discord thread -> agent stdin): When `Bot.handle_thread_message` receives a message in this session's thread, it writes the message content to the agent's stdin via `Agent_process.send_input`. This fiber is event-driven: it reads from an `Eio.Stream.t` that the bot pushes messages into.

The session type gains a message stream:

    type t = {
      ...
      input_stream : string Eio.Stream.t;
    }

The output relay must handle Discord's 2000-character message limit. Split long output at paragraph boundaries (double newline), then at single newlines, then at spaces, then hard-cut at 2000 chars. This reuses the chunking logic from the reference Discord plugin.

For real-time feel, use an output buffer with a flush timer: accumulate output for up to 2 seconds, then send. If the agent is producing output rapidly (streaming), this batches it into readable chunks rather than flooding Discord with tiny messages. If the agent pauses (waiting for tool approval, thinking), the buffer flushes immediately on the next output after the pause.

The output relay should also handle agent completion: when the agent process exits, post a final message to the thread with the exit status and session summary.

**Validation**: Start an agent session via `!start <project> claude`. In the created Discord thread, type a prompt. Observe: (1) the prompt is forwarded to Claude, (2) Claude's response appears in the thread within a few seconds, (3) long responses are split across multiple messages, (4) when Claude finishes, a completion message appears.

### Milestone 5: Bot Orchestration

The goal is to implement the full Discord channel layout: auto-created project channels, thread creation for sessions, and the control channel command set. After this milestone, the full user experience works end-to-end.

Concrete changes:

**Project channel management**: When the bot starts, for each discovered project that doesn't have a `channel_id` in the config, create a Discord text channel in the guild using `Discord_rest.create_channel`. Store the mapping (project name -> channel_id) back to the config file. Group project channels under a category channel called "Agent Projects" (create it if it doesn't exist, using channel type 4).

**Thread creation on `!start`**: In `Bot.handle_control_message` for `Start_agent`, after creating the worktree:
1. Find the project's Discord channel.
2. Post a starter message: "Starting claude session for **myproject** (branch: agent/claude-abc123)".
3. Create a thread from that message using `Discord_rest.create_thread` with name like "claude-abc123".
4. Use the thread's channel ID as the session's `thread_id`.

**Control channel auto-discovery**: On READY, if `control_channel_id` is not set in config, look for a channel named "agent-control" in the guild. If it doesn't exist, create it. Save the ID to config.

**Enhanced commands**:
- `!start <project> <agent> [prompt]` — optional initial prompt passed to the agent
- `!status` — shows all sessions with their current state and last activity time
- `!logs <session_id>` — fetches recent thread messages for a session

**Message routing refinement**: The bot must distinguish between:
- Messages in the control channel -> parse as commands
- Messages in a session thread -> forward to the agent
- Messages in a project channel (not a thread) -> could be future project-level commands
- All other messages -> ignore

**Typing indicator**: When the agent is processing (output relay is active), call Discord's typing indicator endpoint (`POST /channels/{id}/typing`) every 8 seconds to show "Bot is typing..." in the thread.

**Ack reaction**: When a message is received in a session thread, react with a configurable emoji (default: eyes) to acknowledge receipt before forwarding to the agent.

**Validation**: Start the bot fresh with no config. Observe: (1) it creates an "Agent Projects" category and project channels, (2) "agent-control" channel appears, (3) `!projects` in control channel lists all discovered projects, (4) `!start myproject claude` creates a thread in the project's channel, (5) conversation works in the thread, (6) `!sessions` shows the active session, (7) `!stop` terminates it.

### Milestone 6: Hardening and Polish

The goal is to make the system robust for real use: gateway reconnection, graceful shutdown, error handling, and operational niceties.

**Gateway reconnection**: Implement the full reconnect/resume protocol. On disconnect, connect to `resume_gateway_url`, send Resume (op 6). On invalid session, re-identify. Add exponential backoff for repeated failures. The bot should survive network blips without losing sessions.

**Graceful shutdown**: On SIGINT/SIGTERM, stop all agent sessions (send EOF to their stdin, wait for exit), post a "shutting down" message to the control channel, disconnect from the gateway cleanly (close code 1000).

**Rate limiting**: Discord returns HTTP 429 with a `Retry-After` header. The REST client must respect this: on 429, sleep for the specified duration and retry. Track per-route rate limit buckets using the `X-RateLimit-Bucket` header.

**Error recovery**: If an agent process crashes, detect it (check_status returns Stopped or Failed), post the exit status to the session thread, and mark the session as stopped. Offer to restart via a `!restart <session_id>` command.

**Message splitting**: Ensure code blocks are not split mid-block. If a chunk would break inside triple backticks, extend the chunk to include the closing backticks (or split before the opening backticks).

**Config hot-reload**: Watch the config file for changes (or add a `!reload` command) so the user can add projects or change settings without restarting.

**Logging**: All agent subprocess output should be logged to a file (one per session) for debugging. Log path: `~/.local/share/discord-agents/logs/<session-id>.log`.

**Validation**: (1) Kill the network briefly and observe the bot reconnects and resumes, (2) Kill an agent process and observe the error message in the thread, (3) Send a very long message (>2000 chars) and observe correct splitting, (4) Run `!start` on 3 different projects simultaneously and verify all three sessions work independently, (5) Send SIGINT to the binary and observe graceful shutdown messages.

## Concrete Steps

All commands are run from the working directory `/home/tedks/Projects/claude-discord/master/`.

To build at any point:

    nix develop --command dune build

To run the binary:

    nix develop --command dune exec discord-agents

To run with a specific config:

    DISCORD_BOT_TOKEN=<token> nix develop --command dune exec discord-agents

The config file lives at `~/.config/discord-agents/config.json`. A minimal config:

    {
      "discord_token": "MTIz...",
      "base_directories": ["/home/tedks/Projects"],
      "guild_id": "123456789",
      "control_channel_id": null,
      "projects": []
    }

To create a Discord bot and get the token:
1. Go to https://discord.com/developers/applications
2. Create a new application
3. Go to Bot settings, click "Reset Token", copy the token
4. Enable "Message Content Intent" under Privileged Gateway Intents
5. Go to OAuth2 > URL Generator, select scope "bot", permissions: Send Messages, Create Public Threads, Read Message History, Add Reactions, Manage Channels, Read Messages/View Channels
6. Open the generated URL to invite the bot to your server

These steps will be updated as each milestone is completed with the actual commands run and their output.

## Validation and Acceptance

The system is complete when all of these behaviors can be demonstrated:

1. Run `discord-agents` with a valid config. The bot appears online in Discord.
2. Type `!help` in the control channel. The bot replies with the command list.
3. Type `!projects`. The bot lists projects found in the base directories.
4. Type `!start <project> claude`. A thread is created. The bot posts a startup message.
5. Type a prompt in the thread. Claude's response appears in the thread within seconds.
6. Type follow-up messages. The conversation continues naturally.
7. Type `!sessions` in the control channel. The active session is listed.
8. Type `!stop <thread_id>`. The session stops and the thread gets a completion message.
9. Repeat steps 4-8 with `codex` and `gemini` instead of `claude`.
10. Start 3 sessions simultaneously. All three work independently.
11. Kill the bot with Ctrl-C. All sessions shut down gracefully.

## Idempotence and Recovery

Building is idempotent: `dune build` can be run any number of times. The Nix shell is hermetic.

If the bot crashes while sessions are running, the agent subprocesses become orphans. On next startup, the bot should detect stale worktrees (branches matching `agent/*` with no running session) and offer to clean them up via `!cleanup`.

Config changes are additive: adding a new project to `base_directories` requires only a restart (or `!reload`). Removing a project while sessions are running should be avoided; the bot should warn if it detects this.

Git worktrees created for sessions persist after the session ends. They can be cleaned up with `git worktree remove` or via a future `!cleanup` command. This is intentional: it preserves the agent's work for review.

## Artifacts and Notes

Example stream-json output from Claude:

    {"type":"system","subtype":"init","session_id":"abc123","tools":["Read","Write",...]}
    {"type":"assistant","message":{"id":"msg_01","type":"message","role":"assistant","content":[{"type":"text","text":"I'll help you with that."}],"stop_reason":null}}
    {"type":"assistant","message":{"id":"msg_01","type":"message","role":"assistant","content":[{"type":"text","text":"I'll help you with that. Let me read the file first."}],"stop_reason":"tool_use"}}
    {"type":"result","subtype":"success","result":"Done. I've updated the file.","session_id":"abc123","cost_usd":0.05}

The output relay should extract `content[].text` from `type:"assistant"` events and `result` from `type:"result"` events.

Example config.json for a machine with projects in ~/Projects:

    {
      "discord_token": "MTIz...",
      "base_directories": ["/home/tedks/Projects"],
      "guild_id": "1234567890",
      "control_channel_id": null,
      "projects": []
    }

The `projects` list is auto-populated from base_directories on startup. Explicitly listing projects allows overriding the channel_id mapping for projects that were auto-discovered.

## Interfaces and Dependencies

### Nix/opam dependencies (all in `flake.nix`)

    eio_main (>= 1.0)      — Eio event loop
    cohttp-eio (>= 6.0)    — HTTP client
    httpun-ws               — WebSocket protocol
    httpun-eio              — WebSocket Eio integration
    tls-eio (>= 1.0)       — TLS for WSS and HTTPS
    ca-certs                — System CA certificate store
    yojson (>= 2.0)        — JSON parsing
    ppx_yojson_conv         — JSON deriving
    ppx_deriving            — show/eq deriving
    logs (>= 0.7)          — Logging
    fmt                     — Formatting
    uri                     — URI parsing

### Key type signatures that must exist after all milestones

In `lib/discord_rest.ml`:

    type t
    val create : token:string -> Eio_unix.Stdenv.base -> t
    val create_message : t -> channel_id:string -> content:string -> ?reply_to:string -> unit -> (Discord_types.message, string) result
    val create_channel : t -> guild_id:string -> name:string -> ?parent_id:string -> ?topic:string -> unit -> (Discord_types.channel, string) result
    val create_thread : t -> channel_id:string -> message_id:string -> name:string -> unit -> (Discord_types.channel, string) result
    val create_thread_no_message : t -> channel_id:string -> name:string -> unit -> (Discord_types.channel, string) result
    val get_messages : t -> channel_id:string -> ?limit:int -> unit -> (Discord_types.message list, string) result
    val create_reaction : t -> channel_id:string -> message_id:string -> emoji:string -> unit -> (unit, string) result
    val edit_message : t -> channel_id:string -> message_id:string -> content:string -> unit -> (Discord_types.message, string) result
    val send_typing : t -> channel_id:string -> unit -> (unit, string) result

In `lib/discord_gateway.ml`:

    type t
    type event = Connected of Discord_types.user | Message_received of Discord_types.message | Thread_created of Discord_types.channel | Disconnected of string
    val create : token:string -> intents:int -> handler:(event -> unit) -> t
    val connect : t -> env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> unit
    val disconnect : t -> unit

In `lib/agent_process.ml`:

    type t
    type status = Running | Stopped of int | Failed of string
    val spawn : sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr -> fs:_ Eio.Path.t -> kind:Config.agent_kind -> working_dir:string -> initial_prompt:string -> t
    val send_input : t -> string -> unit
    val read_events : t -> (Yojson.Safe.t option -> unit) -> unit  (* stream-json event callback *)
    val read_text : t -> (string option -> unit) -> unit  (* plain text callback *)
    val check_status : t -> status
    val stop : t -> unit

In `lib/session.ml`:

    type t
    type state = Starting | Active | Stopping | Stopped
    val create : project_name:string -> agent_kind:Config.agent_kind -> thread_id:string -> worktree_path:string -> rest:Discord_rest.t -> t
    val start : t -> sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr -> fs:_ Eio.Path.t -> (unit, string) result
    val send_message : t -> string -> unit
    val stop : t -> unit
    val state : t -> state

In `lib/bot.ml`:

    type t
    val create : Config.t -> env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> t
    val run : t -> unit
