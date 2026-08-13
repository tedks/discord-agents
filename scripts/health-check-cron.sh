#!/usr/bin/env bash
# Standalone health-check + restart + disk-cleanup for the discord-agents
# daemon. Meant to be installed in the user's persistent crontab so it
# runs independently of any Claude Code / cron-tool session.
#
# Healthy = the bot's control-API Unix socket answers a "health" request
# with gateway_connected: true. This is a purpose-built status field the
# bot exposes (lib/control_api.ml, Option.is_some bot.gateway.ws), not a
# process/fd heuristic — the bot also runs an always-on control-API
# listener socket independent of the Discord gateway, so "does this pid
# have any open socket fd" would report healthy even with a dead gateway.
#
# On unhealthy: restarts via run-branch.sh master, backgrounded with
# setsid+nohup+disown so it survives this script (and cron's parent)
# exiting. The lock fd (200) is explicitly closed before backgrounding —
# otherwise it gets inherited across setsid/nohup/exec all the way into
# the long-running daemon, which then holds the flock forever and every
# subsequent cron tick silently no-ops.
#
# The daemon's own launch/runtime output goes to a freshly timestamped
# file per restart attempt under DAEMON_LOG_DIR, never a shared path this
# script rewrites — truncating/rotating a file a long-lived process still
# has open makes it keep writing to the old unlinked inode, silently
# losing output and disk space. Old attempt logs are pruned by count.
#
# Also runs the same safe, bounded disk cleanup used interactively:
# nix-collect-garbage -d, docker image/builder prune (dangling/untagged
# only — never touches named/tagged images or volumes, which hold live
# project data), and npm cache clean (npm re-fetches on demand; nothing
# is lost). Does NOT do any forced recursive deletion of files on its
# own; stale /tmp cruft is left for a human to review, matching the
# safety-hook constraint on that class of command.
#
# What this cleanup deliberately does NOT cover, because it needs a human
# decision or root: journald (needs root to vacuum; ask the operator to
# run `journalctl --vacuum-size=...` or cap SystemMaxUse in
# journald.conf), docker images/volumes that are tagged but unused (may
# be kept deliberately — e.g. a postgres image for occasional local dev),
# and general project-directory growth under the user's home directory
# (the actual largest consumer on this box; not this script's call to
# make).
#
# Before GC'ing, refresh a persistent GC root for the repo's nix devShell
# (GCROOT_PROFILE). Without this, nix-collect-garbage is free to evict the
# devShell's own dependency closure between cron ticks (nothing else roots
# it once the shell that fetched it has exited), so run-branch.sh's build
# step has to re-fetch those paths from the network on the next restart —
# exactly when disk is tightest and least able to spare room for a fetch.
# This is what actually happened on 2026-08-04: the daemon went unhealthy
# at 21:20, and every restart attempt for the next ~9.5 hours failed with
# "No space left on device" inside `nix develop`, because the paths it
# needed to build weren't in the store anymore and there was no room to
# refetch them (see also DAEMON_LOG_DIR below, without which this was
# hard to diagnose after the fact). Freeing space by hand (a stale ~12GB
# npm cache) broke the loop, and this GC root prevents it recurring.

set -uo pipefail
# No -e: failures are handled explicitly throughout (restart/cleanup are
# each best-effort and independently logged) so one failing step doesn't
# skip the rest of the run.

# Cron's environment is minimal (no ~/.bashrc / ~/.profile sourcing), so
# make sure nix and docker are actually reachable.
export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH"

REPO_ROOT="/home/tedks/Projects/claude-discord/master"
CONFIG_DIR="$HOME/.config/discord-agents"
PIDFILE="$CONFIG_DIR/discord-agents.pid"
CONTROL_SOCK="$CONFIG_DIR/control.sock"
LOGFILE="$CONFIG_DIR/health-check.log"
DAEMON_LOG_DIR="$CONFIG_DIR/daemon-logs"
LOCKFILE="$CONFIG_DIR/health-check.lock"
GCROOT_PROFILE="$HOME/.local/state/discord-agents/devshell-gcroot"
MAX_LOG_LINES=2000
MAX_DAEMON_LOGS=10

exec 200>"$LOCKFILE"
flock -n 200 || exit 0

log() {
  printf '%s %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$1" >> "$LOGFILE"
}

is_healthy() {
  [[ -S "$CONTROL_SOCK" ]] || return 1
  python3 - "$CONTROL_SOCK" <<'PY' >/dev/null 2>&1
import json, socket, sys

path = sys.argv[1]
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.settimeout(10)
try:
    s.connect(path)
    s.sendall(b'{"method":"health"}\n')
    buf = b""
    while b"\n" not in buf:
        chunk = s.recv(4096)
        if not chunk:
            break
        buf += chunk
    resp = json.loads(buf.decode().splitlines()[0])
    sys.exit(0 if resp.get("ok") and resp.get("gateway_connected") else 1)
except Exception:
    sys.exit(1)
finally:
    s.close()
PY
}

mkdir -p "$DAEMON_LOG_DIR" || log "WARNING: mkdir $DAEMON_LOG_DIR failed — daemon launch output won't be captured"

PID=$(cat "$PIDFILE" 2>/dev/null || true)

if is_healthy; then
  log "OK pid=${PID:-unknown} healthy (gateway_connected)"
else
  log "UNHEALTHY pid=${PID:-none} — restarting"
  # Each attempt gets its own file — never reuse one path across attempts.
  # A prior version redirected every attempt into one fixed path with `>`
  # (truncate); if an earlier attempt's daemon was still alive and holding
  # that path open (as happened during the 2026-08-04 outage, where every
  # 20-minute retry re-truncated the file the still-running old daemon had
  # open since its last real launch), each new truncate silently orphaned
  # the running daemon's own log output onto an unlinked/sparse file.
  DAEMON_LOGFILE="$DAEMON_LOG_DIR/launch-$(date '+%Y%m%d-%H%M%S').log"
  if cd "$REPO_ROOT"; then
    (
      exec 200>&-
      setsid nohup ./scripts/run-branch.sh master > "$DAEMON_LOGFILE" 2>&1 < /dev/null &
      disown
    )
  else
    log "RESTART FAILED cd $REPO_ROOT failed"
  fi
  sleep 20
  NEWPID=$(cat "$PIDFILE" 2>/dev/null || true)
  if is_healthy; then
    log "RESTARTED ok new_pid=$NEWPID"
  else
    log "RESTART FAILED new_pid=${NEWPID:-none} — check $DAEMON_LOGFILE for build/launch errors"
  fi
  # Keep only the most recent attempts, but never prune one a process still
  # has open — a restart attempt long past MAX_DAEMON_LOGS ago may still be
  # the file the currently-running (if unhealthy-but-alive) daemon is
  # writing into during a long outage.
  ls -1t "$DAEMON_LOG_DIR"/launch-*.log 2>/dev/null \
    | tail -n "+$((MAX_DAEMON_LOGS + 1))" \
    | while IFS= read -r old_log; do
        fuser "$old_log" >/dev/null 2>&1 || rm -f "$old_log"
      done
fi

# Refresh the devShell GC root before collecting garbage, so
# nix-collect-garbage can never evict what run-branch.sh needs to rebuild —
# see the top-of-file comment on GCROOT_PROFILE for why this matters. Nix
# profile updates are atomic (the new generation only swaps in on success),
# so a failed refresh leaves the previous generation — still a valid root —
# in place; but a *chronically* failing refresh needs to be visible rather
# than silently doing nothing, and skipping the GC pass on failure avoids
# collecting anything on the strength of a root we just failed to confirm.
mkdir -p "$(dirname "$GCROOT_PROFILE")"
if (cd "$REPO_ROOT" && nix develop --profile "$GCROOT_PROFILE" --command true) >/dev/null 2>&1; then
  # Profile generations are themselves GC roots and nix-collect-garbage -d
  # only prunes generations of profiles under its own well-known
  # directories, not this custom path — so without this, old generations
  # (and the closures they root) would accumulate forever every time the
  # devShell's inputs change. Keep only the current one.
  nix-env --delete-generations --profile "$GCROOT_PROFILE" old >/dev/null 2>&1 \
    || log "WARNING: pruning old $GCROOT_PROFILE generations failed"
  GCROOT_OK=1
else
  log "GC-root refresh FAILED — skipping this cycle's nix-collect-garbage"
  GCROOT_OK=0
fi

BEFORE=$(df -h / | awk 'NR==2')
if [[ "$GCROOT_OK" -eq 1 ]]; then
  GC_SUMMARY=$(nix-collect-garbage -d 2>&1 | tail -1)
else
  GC_SUMMARY="skipped (GC-root refresh failed)"
fi
IMAGE_RECLAIMED=$(docker image prune -f 2>&1 | grep "^Total reclaimed space:" || echo "Total reclaimed space: unknown")
BUILDER_RECLAIMED=$(docker builder prune -f 2>&1 | grep "^Total:" || echo "Total: unknown")
# npm re-fetches whatever it needs on the next install; nothing here is
# durable state, so this is safe to clear unconditionally every cycle.
NPM_BEFORE=$(du -sh "$HOME/.npm" 2>/dev/null | awk '{print $1}')
npm cache clean --force > /dev/null 2>&1
NPM_AFTER=$(du -sh "$HOME/.npm" 2>/dev/null | awk '{print $1}')
AFTER=$(df -h / | awk 'NR==2')
log "gc: $GC_SUMMARY"
log "docker: images $IMAGE_RECLAIMED | builder $BUILDER_RECLAIMED"
log "npm cache: ${NPM_BEFORE:-0} -> ${NPM_AFTER:-0}"
log "disk before: $BEFORE | after: $AFTER"

if [[ -f "$LOGFILE" ]]; then
  tail -n "$MAX_LOG_LINES" "$LOGFILE" > "$LOGFILE.tmp" && mv "$LOGFILE.tmp" "$LOGFILE"
fi
