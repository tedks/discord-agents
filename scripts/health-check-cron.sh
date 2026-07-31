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
# The daemon's own launch/runtime output goes to a separate, unrotated
# file (DAEMON_LOGFILE) that this script never rotates — rotating a file
# a long-lived process still has open (via tail+mv) makes it keep writing
# to the old unlinked inode, silently losing output and disk space.
#
# Also runs the same safe, bounded disk cleanup used interactively:
# nix-collect-garbage -d, docker image/builder prune (dangling/untagged
# only — never touches named/tagged images or volumes, which hold live
# project data). Does NOT do any forced recursive deletion of files on
# its own; stale /tmp cruft is left for a human to review, matching the
# safety-hook constraint on that class of command.

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
DAEMON_LOGFILE="$CONFIG_DIR/daemon-launch.log"
LOCKFILE="$CONFIG_DIR/health-check.lock"
MAX_LOG_LINES=2000

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

PID=$(cat "$PIDFILE" 2>/dev/null || true)

if is_healthy; then
  log "OK pid=${PID:-unknown} healthy (gateway_connected)"
else
  log "UNHEALTHY pid=${PID:-none} — restarting"
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
fi

BEFORE=$(df -h / | awk 'NR==2')
GC_SUMMARY=$(nix-collect-garbage -d 2>&1 | tail -1)
docker image prune -f > /dev/null 2>&1
docker builder prune -f > /dev/null 2>&1
AFTER=$(df -h / | awk 'NR==2')
log "gc: $GC_SUMMARY"
log "disk before: $BEFORE | after: $AFTER"

if [[ -f "$LOGFILE" ]]; then
  tail -n "$MAX_LOG_LINES" "$LOGFILE" > "$LOGFILE.tmp" && mv "$LOGFILE.tmp" "$LOGFILE"
fi
