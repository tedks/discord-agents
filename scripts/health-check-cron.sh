#!/usr/bin/env bash
# Standalone health-check + restart + disk-cleanup for the discord-agents
# daemon. Meant to be installed in the user's persistent crontab so it
# runs independently of any Claude Code / cron-tool session.
#
# Healthy = pidfile PID is alive AND has at least one open socket fd
# (catches the "process alive but silently disconnected from the Discord
# gateway" failure mode, not just "does a process exist").
#
# On unhealthy: restarts via run-branch.sh master, backgrounded with
# setsid+nohup+disown so it survives this script (and cron's parent)
# exiting.
#
# Also runs the same safe, bounded disk cleanup used interactively:
# nix-collect-garbage -d, docker image/builder prune (dangling/untagged
# only — never touches named/tagged images or volumes, which hold live
# project data). Does NOT do any rm -rf of its own; stale /tmp cruft is
# left for a human to review, matching the safety-hook constraint that
# blocks automated rm -rf.

set -uo pipefail

REPO_ROOT="/home/tedks/Projects/claude-discord/master"
PIDFILE="$HOME/.config/discord-agents/discord-agents.pid"
LOGFILE="$HOME/.config/discord-agents/health-check.log"
LOCKFILE="/tmp/discord-agents-healthcheck.lock"
MAX_LOG_LINES=2000

exec 200>"$LOCKFILE"
flock -n 200 || exit 0

log() {
  printf '%s %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$1" >> "$LOGFILE"
}

is_healthy() {
  local pid="$1"
  [[ -n "$pid" ]] || return 1
  kill -0 "$pid" 2>/dev/null || return 1
  ls -la "/proc/$pid/fd" 2>/dev/null | grep -qi socket || return 1
  return 0
}

PID=$(cat "$PIDFILE" 2>/dev/null || true)

if is_healthy "$PID"; then
  log "OK pid=$PID healthy"
else
  log "UNHEALTHY pid=${PID:-none} — restarting"
  (
    cd "$REPO_ROOT" && \
    setsid nohup ./scripts/run-branch.sh master >> "$LOGFILE" 2>&1 < /dev/null &
    disown
  )
  sleep 20
  NEWPID=$(cat "$PIDFILE" 2>/dev/null || true)
  if is_healthy "$NEWPID"; then
    log "RESTARTED ok new_pid=$NEWPID"
  else
    log "RESTART FAILED new_pid=${NEWPID:-none} — check $LOGFILE for build/launch errors"
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
