#!/usr/bin/env bash
# Run the discord-agents bot from a feature worktree, replacing any
# currently-running instance.
#
# Mechanism: the pidfile lock in bin/main.ml acquires
# ~/.config/discord-agents/discord-agents.pid; if another bot already
# holds it, that bot gets SIGTERM (then SIGKILL after 2s) and the new
# bot takes over. So switching branches = launch the bot from the
# branch's worktree.
#
# Usage:
#   scripts/run-branch.sh <worktree-name-or-path>
#
# Examples:
#   scripts/run-branch.sh feat/codex-support
#   scripts/run-branch.sh master
#   scripts/run-branch.sh /home/me/Projects/claude-discord/feat/foo
#
# Builds first so a compile error doesn't kill the running bot.
# Runs in the foreground; Ctrl-C stops the new bot, after which you'll
# need to re-launch the previous one (this script doesn't restore).

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <worktree-name-or-path>" >&2
  echo "  e.g. $0 feat/codex-support" >&2
  echo "  e.g. $0 master" >&2
  echo >&2
  echo "Available worktrees:" >&2
  git worktree list 2>/dev/null | sed 's/^/  /' >&2 || true
  exit 1
fi

target="$1"

# If not absolute, look up via `git worktree list`. Match either by
# branch name (refs/heads/<name>) or by path basename. Skip bare-repo
# entries — porcelain emits "bare" for them and we can't `dune exec`
# from a bare path. The match decision is deferred to end-of-block
# (blank line) so we know whether the entry was bare before printing.
if [[ "$target" != /* ]]; then
  resolved=$(git worktree list --porcelain | awk -v name="$target" '
    function flush() {
      if (path != "" && !bare && basename_match) { print path; found = 1; exit }
      path = ""; bare = 0; basename_match = 0
    }
    /^worktree / { flush(); path = $2; basename_match = (path ~ ("/" name "$")) }
    /^bare/      { bare = 1 }
    /^branch / {
      branch = $2
      sub(/^refs\/heads\//, "", branch)
      if (branch == name && !bare) { print path; found = 1; exit }
    }
    /^$/ { flush() }
    END  { if (!found) flush() }
  ')
  if [[ -z "${resolved:-}" ]]; then
    echo "No worktree matching '$target'" >&2
    echo "Available:" >&2
    git worktree list 2>/dev/null | sed 's/^/  /' >&2
    exit 1
  fi
  target="$resolved"
fi

if [[ ! -d "$target" ]]; then
  echo "No directory at $target" >&2
  exit 1
fi

cd "$target"
echo "[run-branch] worktree: $target"
echo "[run-branch] branch:   $(git branch --show-current 2>/dev/null || echo '(detached)')"
echo "[run-branch] building (won't replace the running bot if this fails)..."
nix develop --command dune build

echo "[run-branch] launching — pidfile takeover will replace any running bot."
exec nix develop --command dune exec discord-agents
