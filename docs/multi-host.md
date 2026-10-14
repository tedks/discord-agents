# Multi-host sessions: one Discord writer, remote executors over SSH

Status: design, first pass (2026-10-14). Nothing here is implemented yet.
Tracking issue: https://github.com/tedks/discord-agents/issues/74.
Open product questions are filed as separate issues and linked in
[Open questions](#open-questions).

## Intent

Today every agent session runs on the machine that runs the bot. A
project that only exists on framework0 is invisible from Discord, and a
session cannot use framework0's CPU, disk or agent logins. After this
work, one bot process still owns the Discord connection, but any session
can be started on any of the operator's hosts (drynwyn, tower0,
framework0, splinter0, splinter1) and later resumed on the host where it
was created, from the same Discord thread.

Issue #74 asked whether to build a cluster of cooperating bots or one
writer with remote executors. The operator has settled that: **one
writer, remote executors, using the lightweight remote-launch shape.**
This doc designs that shape and does not revisit the choice.

Terms used throughout:

- **Writer**: the single bot process that holds the Discord gateway
  connection, owns `sessions.json`, and serves the control socket. It
  runs on one host, the *writer host* (tower0 today).
- **Executor host**: any host where agent CLIs run. The writer host is
  also an executor host. Its executor is the existing local spawn path.
- **Remote run**: one agent invocation (one Discord message's turn) that
  runs on an executor host other than the writer host. Each turn is a
  separate `claude -p` / `codex exec` / `gemini` process today, and stays
  that way.

## What is host-local today

All of the following assume "the agent runs here", and each needs a
host-aware answer:

- **Process spawn**: `Agent_process.run_streaming`
  (`lib/agent_process.ml`) spawns `setsid --wait <agent> ...` with
  `Eio.Process.spawn`, reads stdout line-by-line and parses each line
  with the per-agent parser.
- **Child tracking**: `Bot.pid_ownership` and the
  `Agent_checkpoint.child_process_identity` (pid plus `/proc` start
  ticks) are used for stop, restart drain, and startup reaping
  (`Bot.reconcile_interrupted_active_runs`).
- **Session identity**: `Session_store.session` has `working_dir` and
  `session_id` but no host. The transcripts behind `session_id` live in
  `~/.claude/projects`, `~/.codex/sessions` and `~/.gemini/tmp` on the
  host that ran the agent (`lib/claude_sessions.ml`,
  `lib/codex_sessions.ml`, `lib/gemini_sessions.ml`). Claude also keys
  its transcript directory by the working directory, so a resume must
  run in the recorded cwd on the recorded host.
- **Projects and worktrees**: `Project.discover` scans
  `Config.base_directories` on the local filesystem and dedups by remote
  URL (`Project.remote_key`). `Project.create_worktree`,
  `remove_worktree`, `prune_merged_worktrees` and
  `clean_worktree_build_artifacts` all run git or unlink locally.
- **MCP callback**: agents get the bot's tools from `discord-agents-mcp`,
  a stdio server (`bin/mcp_server.ml`) that forwards to the writer's
  Unix control socket (`Control_api.socket_path`, `control.sock` in the
  app config dir). The caller's thread id reaches it through the
  `DISCORD_AGENTS_THREAD_ID` environment variable.
  `Agent_process.resolve_mcp_command` finds the executable locally.
  Claude gets a generated config file, Codex gets `-c` overrides, and
  Gemini gets `.gemini/settings.json` written into the worktree.
- **Attachments**: `Control_api.load_attachment_uploads` reads files from
  `session.working_dir` inside the writer process. That is a writer-side
  read of an executor-side path, whatever MCP transport we pick.
- **Disk pressure and rescue**: `Disk_health` probes local paths with
  statvfs. `Bot.effective_top_level_agent` swaps in the rescue agent when
  the local disk is under pressure.
- **Agent auth and model availability**: implicitly "whatever this
  machine is logged into". The Codex model a ChatGPT-plan account
  rejected is the first concrete case of this varying per host.

## Ground truth (probed from tower0, 2026-10-14)

- The bot runs on **tower0**. Root disk is 92% full (37 GB free).
- **framework0**: claude, codex and gemini are on the non-login PATH.
  It has 635 Claude project dirs and no discord-agents checkout.
- **drynwyn**: no agent CLIs on either the login or the non-login PATH,
  and no Claude transcripts. It is not a viable executor until the
  agents are installed there. It runs OpenSSH 7.6.
- **splinter0** (`10.0.0.2`, no route) and **splinter1** (timed out) were
  unreachable. Laptops being away is the normal case, not an outage.
- `<host>.local` mDNS names did **not** resolve from tower0 for
  framework0 or the splinters, but the `~/.ssh/config` aliases
  (`framework0`, `drynwyn`, ...) did. Host addressing must use
  configured SSH destinations, not a hard-coded `.local` suffix.
- Reverse SSH works: `framework0 → tower0` and `drynwyn → tower0` both
  succeed with `BatchMode=yes`.
- Remote SSH configs are noisy on stderr: "Pseudo-terminal will not be
  allocated" (a `RequestTTY` setting), and VisualHostKey art from
  drynwyn. **stderr cannot carry protocol.** Always pass `-T` and
  `-o LogLevel=ERROR`, and treat stderr as diagnostics only.
- `/bin/sh` is dash on the Ubuntu hosts. dash's builtin `kill` rejects
  `kill -s TERM -PGID` and `kill -TERM -- -PGID`. Only the numeric form
  `kill -15 -PGID` signals a process group. (The prototype watchdog
  silently failed until this was fixed.)

## Architecture

    Discord ── gateway ── Writer (tower0)
                            │  sessions.json, control.sock, Discord REST
                            │
              ┌─────────────┼──────────────────────┐
              │ local spawn │ ssh -T framework0 …  │ ssh -T drynwyn …
              ▼             ▼                      ▼
          agent CLI    remote wrapper ─ agent   remote wrapper ─ agent
                            │ MCP: ssh -T tower0 discord-agents-mcp
                            └──────────► control.sock on the writer

Everything Discord-facing stays in the writer. An executor host gets
nothing installed beyond what it already needs to run agents: `sh`,
`setsid`, `flock`, `base64`, `git`, the agent CLIs, and SSH keys to and
from the writer. The writer drives each remote run over one SSH
connection, and the agent's stdout returns over that same connection.

### Host registry

Hosts are declared statically in `config.json` and addressed by SSH
destination. mDNS discovery is not part of this design (see the probe
results above, and open question Q4).

    "hosts": {
      "writer": "tower0",
      "executors": {
        "tower0":     { "local": true },
        "framework0": { "ssh": "framework0", "writer_ssh": "tower0",
                        "base_directories": ["~/Projects"] },
        "drynwyn":    { "ssh": "drynwyn", "writer_ssh": "tower0",
                        "enabled": false }
      }
    }

A missing `hosts` block means exactly today's behaviour: one local
executor named after `Unix.gethostname ()`. `writer_ssh` is the
destination the *executor* uses to reach the writer (for the MCP
callback). It defaults to the writer's name. Per-host optional fields
come later: an agent allowlist, model denials, and extra PATH entries.

A new `Host` module owns the type (`name`, `transport = Local | Ssh of
destination`) and config parsing. Host names are opaque, stable strings.
The writer's name is persisted into session records (below), so moving
the writer to another machine does not silently re-home old sessions.

### Host_exec: one interface for "run this on that host"

Every host-local operation goes through a small module:

    val run_capture : Host.t -> ?cwd:string -> string list -> (string, error) result
    val run_streaming : Host.t -> ~cwd -> ~argv -> ~on_line -> ... -> exit
    (* error distinguishes Unreachable / Transport_lost / Exited of int *)

`Local` uses today's code paths unchanged. `Ssh` builds an `ssh`
command line. It reports SSH exit status 255 as a transport failure, not
an agent failure, and the prototype confirmed an unreachable host fails
with 255 within the 5 s connect timeout. The transport command is
injectable, so tests run the SSH path through a fake `ssh` that executes
the payload locally (see [Testing](#testing)).

SSH options for every call:

    -T -o BatchMode=yes -o ConnectTimeout=5 -o LogLevel=ERROR
    -o ServerAliveInterval=10 -o ServerAliveCountMax=3

Connection multiplexing (`ControlMaster=auto`, a `ControlPath` under the
app config dir, `ControlPersist=600`) is an optimization that comes
later, in PR 9. It cuts per-turn handshake cost, but a dead master takes
every run on that host down with it, so it waits until the basic path
is proven.

### Remote run protocol (prototyped)

The constraint that shapes the protocol: sshd hands the remote command
to the user's login shell as **one argument string**, and Linux caps a
single argument at 128 KiB (`MAX_ARG_STRLEN`). Prompts and system
prompts must not travel on the SSH command line. The command line is a
fixed, shell-agnostic bootstrap instead:

    ssh -T ... <dest> sh -c 'IFS= read -r l; eval "$(printf %s "$l" | base64 -d)"'

The writer writes **one line to the SSH channel's stdin**: a base64
encoding of a POSIX `sh` script it generated. The script sets `WD`,
`SID`, the environment and `set -- <argv>`, with every word quoted by
the writer (`Filename.quote` semantics), followed by the wrapper below.
POSIX requires `read` on a pipe to consume byte-by-byte, so the rest of
stdin stays available to the wrapper. The writer then **keeps stdin
open for the life of the run**, and that open pipe is the liveness
signal.

The wrapper, as validated on framework0:

    set -u
    exec 3<&0
    cd "$WD" || exit 76
    mkdir -p "$HOME/.cache/discord-agents/run"
    exec 9>"$HOME/.cache/discord-agents/run/$SID.lock"
    flock -n 9 || { echo "session already running on this host" >&2; exit 75; }
    setsid --wait "$@" </dev/null 9>&- &
    child=$!
    printf '{"type":"discord_agents.remote_started","pid":%s}\n' "$child"
    ( cat <&3 >/dev/null 2>&1
      kill -15 "-$child" 2>/dev/null; sleep 3; kill -9 "-$child" 2>/dev/null
    ) </dev/null >/dev/null 2>&1 9>&- &
    dog=$!
    exec 3<&-
    wait "$child"; rc=$?
    kill "$dog" 2>/dev/null
    exit "$rc"

What each part buys, all observed in the prototype:

- **Watchdog on stdin EOF.** When the local `ssh` client dies (the bot
  stops the run, the bot exits or crashes, or the connection drops),
  sshd closes the channel, `cat` sees EOF, and the agent's whole process
  group is killed. Grandchildren were killed too. The writer therefore
  stops a remote run exactly as it stops a local one: it signals the
  local `ssh` process it already tracks by pid and start ticks. The
  existing restart-reap and stop code needs no remote-specific kill
  path.
- **Per-session `flock`.** A second run of the same session on the same
  host exits 75 immediately ("busy") instead of two agents appending to
  one transcript. This matters after a network partition, when the old
  remote run may still be alive until its sshd notices.
- **`remote_started` line.** It is the first stdout line, and the runner
  consumes it rather than handing it to the agent parsers. It records
  the remote pid for `!status` and for the startup reaper.
- **Exit codes.** The agent's exit status propagates through `ssh`. 75
  means busy, 76 means the working dir is missing, 255 means a transport
  failure, and anything else means the agent exited.
- **Streaming latency.** Agent lines arrived at the writer within a few
  milliseconds of being printed on framework0. The 2 s Discord edit
  cadence is unaffected. When the agent exits normally, `ssh` returned
  promptly with its status; the orphaned watchdog `cat` does not hold
  the channel open.

Known gap: a wedged partition where neither TCP side notices. The local
side bounds it with `ServerAliveInterval` × `ServerAliveCountMax` (about
30 s), after which the writer marks the run failed. The remote side
depends on that host's sshd `ClientAlive*` settings. An orphaned remote
agent can keep running and writing its transcript. The flock stops it
racing a retry, and the startup reaper below kills it.

Out of scope here: the existing local limit that a single argv string
(such as a huge prompt) must be under 128 KiB, which applies to local
spawns today. Passing prompts on the agent's stdin would lift it for
both paths and belongs in a separate change.

### MCP callback: reverse SSH to the writer's own MCP server

A remote agent reaches the bot's tools by running the **writer's**
`discord-agents-mcp` on the writer, over SSH from the executor:

    command: ssh
    args:    -T -o BatchMode=yes -o LogLevel=ERROR <writer_ssh>
             env DISCORD_AGENTS_THREAD_ID=<thread> <writer's resolved mcp path>

Per agent:

- **Claude**: `--mcp-config` accepts an inline JSON string, so no file
  is written remotely. (The variadic-flag caveat in `run_streaming`
  still applies.)
- **Codex**: the existing `-c mcp_servers.discord_agents.command="ssh"`
  and `args=[...]` overrides.
- **Gemini**: the existing `.gemini/settings.json` merge, performed on
  the executor through `Host_exec`.

Why this and not the alternatives:

- **Forward the control socket** (`ssh -R remote.sock:control.sock`)
  and run an MCP binary on the executor. This needs the binary on every
  host. tower0's build links against Nix store paths that don't exist on
  drynwyn, which has no Nix. It also invites version skew between the
  writer and remote MCP binaries, and stale forwarded sockets need
  `StreamLocalBindUnlink` in every host's sshd config.
- **Serve MCP over HTTP** and forward a TCP port. All three CLIs support
  HTTP MCP with headers, and a per-session bearer token would be a
  cleaner caller identity than an env var. But it means implementing
  streamable-HTTP MCP and allocating remote ports. Worth revisiting if
  reverse SSH proves too slow; not needed to ship.

Reverse SSH needs nothing installed on the executor and has no skew,
because the MCP server is always the writer's current build. Its costs:

- One SSH handshake per agent turn, run concurrently with agent startup.
- A requirement that the executor can SSH to the writer. The probe
  confirmed this works from framework0 and drynwyn, and the `!hosts`
  check verifies it per host.

It adds no new trust: any process on an executor that can SSH to the
writer can already run anything there.

If the callback fails, the agent runs without `discord-agents` tools,
the same degraded mode as an unresolved local MCP command. `!hosts`
surfaces the failure so it is not silent.

### Session identity and persistence

`Session_store.session` gains `host : string`. The routing key for
resume becomes `(host, session_id)`, and `working_dir` is interpreted on
that host.

Migration: a `sessions.json` entry without `host` is assigned the
configured writer name **at load time**, and the next save writes it
back explicitly. Persisting the concrete name rather than a symbolic
`"local"` means a later writer move does not re-route old sessions to a
host that never ran them.

The `active_run` checkpoint gains an optional remote child: `(host,
remote_pid)`, captured from `remote_started`, alongside the tracked
local `ssh` pid. The `Agent_checkpoint` GADT shape is unchanged.

Thread names show the host when it is not the writer, for example
`codex@framework0 / discord-agents`. The exact display format is
covered by Q2.

### Projects and worktrees per host

A **logical project** is still the dedup unit: the same remote URL key
`Project.remote_key` uses today. It gains a per-host location map
`host → path`. There is one Discord channel per logical project, and
the host appears per thread. A repo that exists only on framework0 gets
a channel like any other.

Remote discovery runs one SSH command per host. It lists candidate repo
dirs under that host's `base_directories` and prints each one's remote
URL. The writer parses the output with the existing `Project` logic.
Results are cached per host in `hosts-inventory.json` with a
`last_seen` time. An offline host's projects stay listed, marked
offline, rather than vanishing (an offline laptop is normal).

`!start <project>@<host> [agent]` creates the worktree on that host via
`Host_exec` (`git worktree add`, same `agent/<kind>-<uuid>` branch
naming), and the MCP `start_session` gains an optional `host`. Which
host a bare `!start <project>` picks is Q1. What happens when the
project is absent on the requested host is Q3.

`!stop` worktree removal and artifact cleanup, and the `!cleanup`
merged-worktree sweep, run per host through `Host_exec`. Unreachable
hosts are skipped and reported, never treated as "nothing to clean".

### Transcript discovery and resume routing

The three `*_sessions.ml` modules are split into:

1. A pure part that turns `(path, mtime, head bytes)` records into
   `info` values.
2. A source that produces those records: local filesystem today, or one
   SSH command per host that prints the newest transcript files under a
   byte cap.

`!claude-sessions` and its siblings, and `!resume <prefix>`, query
enabled hosts in parallel with a per-host timeout. Each result is
labelled with its host, and unreachable hosts are listed as such.
`!resume [agent] <prefix>[@host]` resolves to exactly one
`(host, session_id, working_dir)`. If a prefix matches on two hosts, the
writer asks the user to qualify it rather than guessing. Before
spawning, the writer checks that the working dir still exists on that
host (exit 76 from the wrapper is the backstop), and reports a missing
worktree or transcript by name.

Cross-host resume (copying a transcript to another host) is not
designed here; see Q7.

### Attachments and other writer-side reads of session files

`send_attachments` becomes a host operation. Path canonicalization and
the `within worktree / regular file / size cap` checks currently in
`Control_api.load_attachment_uploads` run **on the executor**, in one
remote script (`realpath`, `stat`, then the bytes). The writer then
re-checks the size it actually received. The checks keep the same
semantics; only their location moves. Any other control-API handler
that touches `session.working_dir` gets the same treatment. The
implementation PR includes a grep audit for this.

### Agent availability, auth and models per host

Each executor reports which agent CLIs it has: a `command -v` probe at
`!hosts` / refresh time, cached in the inventory. Login status per host
is #80's probe run through `Host_exec`.

Model rejections are **learned per `(host, agent, model)`**. When a run
fails with a recognisable "model not supported for this account" error,
the writer records it and names the host in the error message. A later
`set_model` or `start_session` naming that combination warns up front.
Config can pre-declare denials. There is no silent fallback to another
model or host (Q6).

### Disk pressure and rescue per host

The writer host's pressure continues to gate **state mutation**
(`sessions.json`, settings) globally, because those writes happen on
the writer no matter where the agent runs.

Executor pressure is per host. `Disk_health` observations are keyed by
`(host, path)`. Remote probes are `df -Pk <path>` through `Host_exec`,
throttled to one per host per 60 s and run off the request path. The
effective top-level agent, normally the default agent but the rescue
agent under pressure, is computed against the **target host's**
snapshot. Whether pressure on one host should steer new sessions to
another is Q5.

The health-check cron (`scripts/health-check-cron.sh`) is already
per host. It keeps running on each host independently.

### Stop, restart and startup reconciliation

- **Stop**: signal the tracked local `ssh` process, exactly like a
  local child. The stdin-EOF watchdog kills the remote group.
- **`!restart` / drain**: unchanged. Remote runs drain like local ones.
- **Startup reconcile** (`reconcile_interrupted_active_runs`): reaps
  tracked local `ssh` pids as today. That alone kills remote agents
  whose connection is still up. For checkpoints that carry a remote
  child, the writer then asks that host to kill the process group if it
  still exists and still holds the session's lock (`flock -n` fails).
  This covers a partition-orphaned agent. If the host is unreachable,
  the run is marked failed anyway and a note is left in the thread. The
  flock prevents a later double-run either way.

### Writer exclusivity

The pidfile lock (`bin/main.ml`) is host-local, so it cannot stop a
second bot starting on another host with the same token. Discord would
deliver every event to both, producing duplicate replies and racing
session writes. Two cheap guards:

1. On start, if `hosts.writer` is set and is not this machine's name,
   refuse to start unless `--force-writer` is given.
2. On start, check each reachable configured host for a live
   discord-agents pidfile (`kill -0` on the recorded pid, start time
   compared). Refuse if another writer is alive, naming the host.

Neither is a distributed lock, and neither needs to be: the goal is to
stop accidents like `run-branch.sh` on the wrong machine or a stale
config. Writer placement and failover are Q8.

### Failure modes

- **Host unreachable at start or resume**: the connect fails within
  5 s. The thread gets "framework0 is unreachable", and the message gets
  ❌. There is no queueing until the host returns (Q7).
- **Host lost mid-run**: `ssh` exits 255, or a keepalive timeout fires
  within about 30 s. Partial streamed output stays, the message gets ❌
  with "lost connection to framework0", and the session remains bound to
  that host for a later resume.
- **Worktree missing on resume**: the preflight or exit 76 produces a
  message naming the path and host, and suggests `!start`.
- **Transcript missing**: the agent's own resume error is surfaced with
  the host name.
- **Concurrent run of the same session**: exit 75, "a previous run of
  this session is still alive on <host>", with no second agent started.
- **MCP callback unavailable**: the agent runs without bot tools, and
  `!hosts` shows the callback as failing.
- **Two writers**: refused at startup as described above.
- **Executor stderr noise**: always treated as diagnostics. It never
  breaks parsing because protocol uses stdout only.

## User-visible surface

New and changed commands and MCP tools, final syntax subject to Q2:

- `!hosts`: per host, reachability, agents present, disk mode, callback
  status, last seen.
- `!start <project>[@host] [agent]` and `start_session { host? }`.
- `!resume [agent] <prefix>[@host]` and `resume_session { host? }`.
- `!projects` / `list_projects`: host attribution per project, with
  offline hosts marked.
- `!sessions` / `list_sessions`: host column.
- `!*-sessions` / `list_*_sessions`: aggregated across hosts, labelled,
  optional host filter.
- `!status`: running processes grouped by host.

## Stacked PR plan

Each PR builds and passes `dune runtest` on its own, and each leaves
single-host behaviour unchanged. Only the bottom PR targets master.

1. **docs: multi-host design** (this PR). The doc and the filed open
   questions. No code.
2. **Host model, config and session host field.** Adds the `Host`
   module and the `hosts` config block (absent means today's
   behaviour). Adds `Session_store.session.host` with the load-time
   migration, the `--force-writer` check, and thread-name display.
   Tests: config parsing, migration round-trip, and old `sessions.json`
   fixtures.
3. **Host_exec with Local and Ssh transports.** Adds `run_capture` and
   `run_streaming`, the bootstrap encoder, the wrapper script as an
   embedded resource, and exit classification. Moves `Project`'s git
   calls onto `Host_exec` with `Local`, with no behaviour change. Tests:
   a fake-ssh harness that runs the bootstrap locally, quoting
   round-trips (quotes, newlines, non-UTF-8 bytes, a 200 KB payload),
   flock busy, the watchdog killing a grandchild, and exit
   classification.
4. **Remote agent runs.** Makes `run_streaming` host-aware: wrapper,
   the `remote_started` handling, stdin kept open, and the reverse-SSH
   MCP config for all three agents. The checkpoint gains the remote
   child. Stop and restart work via the local `ssh` pid. Usable end to
   end with a manually created session record. Acceptance: a Claude,
   Codex and Gemini turn on framework0 streams into a thread, `!stop`
   mid-run leaves no process on framework0, and an MCP `send_message`
   from the remote agent lands in the thread.
5. **Remote projects and worktrees.** Adds inventory discovery,
   `hosts-inventory.json`, the union with host attribution,
   `!start <project>@<host>`, `start_session.host`, remote
   worktree create, stop cleanup and `!cleanup`, and the `!hosts`
   command. Acceptance: a repo only on framework0 appears in
   `!projects`, and `!start` on it works.
6. **Transcript discovery and resume routing.** Splits the
   `*_sessions` modules into pure and source parts and adds remote
   sources, aggregated listings, and `!resume …@host` with the
   ambiguity prompt and preflight. Acceptance: resume a framework0
   Codex session from Discord.
7. **Host-routed attachments and session file reads.** Moves
   `send_attachments` validation to the executor and audits the other
   `working_dir` readers.
8. **Per-host health: disk, rescue, agents, models.** Adds keyed
   `Disk_health` observations, remote probes, per-host rescue
   evaluation, the agent presence probe, and learned model rejections.
9. **Hardening.** Adds the startup remote reaper, the peer-writer
   check, ControlMaster multiplexing behind a flag, and fault-injection
   tests (killed transport, partition simulated with a fake ssh that
   stalls).

Depending on answers, Q1–Q3 shape PR 5, Q5–Q6 shape PR 8, and Q7–Q8
shape PR 9. PRs 2–4 do not depend on any open question, so they can
start as soon as this design is accepted.

## Testing

- **Unit**: config and migration, the bootstrap and quoting encoder
  (round-trip through a real `sh` locally), exit-code classification,
  the inventory and transcript-record parsers, and the effective agent
  per host snapshot.
- **Fake-ssh harness**: a test `ssh` executable that ignores its
  destination and runs the remote command locally. This exercises the
  entire SSH code path, including the wrapper, flock and watchdog, in
  `dune runtest` with no network. Each test that spawns gets a
  wall-clock bound, in line with #30's warning about hangs.
- **Live (manual, documented in the PR)**: framework0 for all three
  agents, the unreachable path against an offline splinter, and the
  drynwyn MCP callback once its agents are installed.

## Open questions

Each one is filed with options, a recommendation, and what it blocks:

- Q1 Default host for a bare `!start <project>`: #TBD
- Q2 Host-targeting syntax and thread naming: #TBD
- Q3 Project absent on the requested host (auto-clone or refuse): #TBD
- Q4 Host inventory: static config or discovery: #TBD
- Q5 Disk pressure scope and cross-host steering: #TBD
- Q6 Model and auth mismatch handling per host: #TBD
- Q7 Unreachable host with an active session (fail, queue or migrate): #TBD
- Q8 Writer placement and writer failover: #TBD

## Decision log

- 2026-10-14: one writer with remote executors; no cluster mode.
  Operator ruling on #74.
- 2026-10-14: SSH as the only transport, with nothing installed on
  executors beyond agent CLIs and standard tools. This keeps the
  lightweight shape the operator asked for.
- 2026-10-14: payload over stdin with a fixed bootstrap command, not
  over argv. Forced by sshd's single-string remote command and
  `MAX_ARG_STRLEN`; the prototype carried a 266 KB line through.
- 2026-10-14: remote cleanup by stdin-EOF watchdog plus a per-session
  flock. It reuses all existing local child tracking. Validated on
  framework0, including grandchild cleanup.
- 2026-10-14: MCP callback by reverse SSH to the writer's own MCP
  binary. Zero executor provisioning and no version skew. Socket
  forwarding and HTTP MCP were rejected for now (see above).
- 2026-10-14: persist concrete host names, never a symbolic "local".
  This keeps a writer move from mis-routing old sessions.
- 2026-10-14: address hosts by SSH destination from config. `.local`
  did not resolve for half the fleet from tower0.
