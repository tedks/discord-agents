#!/usr/bin/env python3
"""MCP server for discord-agents bot control.

Exposes bot operations as tools that Claude can call natively via
the tool-use protocol. Communicates with Discord via REST API and
manages sessions via the shared sessions.json file.

Protocol: JSON-RPC 2.0 over stdio (MCP standard).
"""

import json
import os
import signal
import sys
import subprocess
import urllib.request
import uuid
import time
from pathlib import Path

# --- Configuration ---

CONFIG_DIR = Path.home() / ".config" / "discord-agents"
CONFIG_FILE = CONFIG_DIR / "config.json"
SESSIONS_FILE = CONFIG_DIR / "sessions.json"

def load_config():
    if CONFIG_FILE.exists():
        return json.loads(CONFIG_FILE.read_text())
    return {}

SESSIONS_LOCK = CONFIG_DIR / "sessions.json.lock"

import fcntl

def _with_sessions_lock(fn):
    """Execute fn while holding an exclusive lock on sessions.json.lock.
    Prevents lost-update races between the bot and MCP server."""
    SESSIONS_LOCK.parent.mkdir(parents=True, exist_ok=True)
    with open(SESSIONS_LOCK, 'w') as lock_fd:
        fcntl.flock(lock_fd, fcntl.LOCK_EX)
        try:
            return fn()
        finally:
            fcntl.flock(lock_fd, fcntl.LOCK_UN)

def load_sessions():
    def _load():
        if SESSIONS_FILE.exists():
            try:
                return json.loads(SESSIONS_FILE.read_text())
            except json.JSONDecodeError:
                return []
        return []
    return _with_sessions_lock(_load)

def save_sessions(sessions):
    """Atomic write with file locking."""
    def _save():
        tmp = SESSIONS_FILE.with_suffix('.json.tmp')
        tmp.write_text(json.dumps(sessions, indent=2) + "\n")
        tmp.rename(SESSIONS_FILE)
    _with_sessions_lock(_save)

# --- Discord REST helpers ---

def discord_request(method, path, token, body=None):
    url = f"https://discord.com/api/v10{path}"
    data = json.dumps(body).encode() if body else None
    req = urllib.request.Request(url, data=data, method=method, headers={
        "Authorization": f"Bot {token}",
        "Content-Type": "application/json",
        "User-Agent": "DiscordBot (discord-agents-mcp/0.1.0)",
    })
    try:
        resp = urllib.request.urlopen(req, timeout=30)
        if resp.status == 204:
            return {"ok": True}
        return json.loads(resp.read())
    except urllib.error.HTTPError as e:
        try:
            body = e.read().decode()[:200]
        except Exception:
            body = "(unreadable)"
        return {"error": f"HTTP {e.code}: {body}"}
    except Exception as e:
        return {"error": f"request failed: {e}"}

# --- Project discovery ---

def discover_projects(base_directories):
    """Discover git projects, deduplicate by remote URL."""
    projects = []
    seen_remotes = {}

    for base_dir in base_directories:
        base = Path(base_dir)
        if not base.is_dir():
            continue
        for entry in sorted(base.iterdir()):
            if not entry.is_dir():
                continue
            try:
                is_bare = (entry / "HEAD").exists() and (entry / "objects").is_dir()
                is_git = (entry / ".git").exists()
            except OSError:
                continue
            if not (is_bare or is_git):
                continue

            # Get remote URL
            remote = None
            for try_dir in ([entry] if not is_bare else
                           [entry / "master", entry / "main", entry]):
                if try_dir.is_dir():
                    try:
                        result = subprocess.run(
                            ["git", "-C", str(try_dir), "remote", "get-url", "origin"],
                            capture_output=True, text=True, timeout=5)
                        if result.returncode == 0:
                            remote = result.stdout.strip()
                            break
                    except (subprocess.TimeoutExpired, OSError):
                        pass

            # Deduplicate by remote (normalize URL for comparison)
            if remote:
                # Normalize: strip .git suffix, convert SSH to HTTPS-like key
                norm = remote.rstrip("/")
                if norm.endswith(".git"):
                    norm = norm[:-4]
                norm = norm.replace("git@github.com:", "github.com/")
                norm = norm.replace("https://github.com/", "github.com/")
                norm = norm.lower()
                remote_key = norm

                if remote_key in seen_remotes:
                    # Prefer bare repos
                    if is_bare and not seen_remotes[remote_key]["is_bare"]:
                        seen_remotes[remote_key] = {
                            "name": repo_name_from_url(remote),
                            "path": str(entry), "is_bare": is_bare,
                            "remote_url": remote
                        }
                    continue
                name = repo_name_from_url(remote)
                seen_remotes[remote_key] = {
                    "name": name, "path": str(entry),
                    "is_bare": is_bare, "remote_url": remote
                }
            else:
                projects.append({
                    "name": entry.name, "path": str(entry),
                    "is_bare": is_bare, "remote_url": None
                })

    return sorted(list(seen_remotes.values()) + projects, key=lambda p: p["name"])

def repo_name_from_url(url):
    """Extract repo name from git URL."""
    url = url.rstrip("/")
    if url.endswith(".git"):
        url = url[:-4]
    # Handle SSH format: git@github.com:user/repo
    if ":" in url.split("/")[-1] if "/" in url else url:
        parts = url.split(":")
        if len(parts) == 2:
            url = parts[1]
    return url.split("/")[-1]

def find_project_fuzzy(projects, query):
    """Find a project by exact, case-insensitive, prefix, or substring match."""
    q = query.lower()
    # Exact
    for p in projects:
        if p["name"] == query:
            return p
    # Case-insensitive
    for p in projects:
        if p["name"].lower() == q:
            return p
    # Numeric index
    try:
        n = int(query)
        if 1 <= n <= len(projects):
            return projects[n - 1]
    except ValueError:
        pass
    # Prefix
    prefix_matches = [p for p in projects if p["name"].lower().startswith(q)]
    if len(prefix_matches) == 1:
        return prefix_matches[0]
    # Substring
    substr_matches = [p for p in projects if q in p["name"].lower()]
    if len(substr_matches) == 1:
        return substr_matches[0]
    return None

def find_working_dir(project):
    """Find a usable working directory for a project."""
    path = Path(project["path"])
    if project["is_bare"]:
        for name in ["master", "main"]:
            candidate = path / name
            if candidate.is_dir():
                return str(candidate)
    return str(path)

def create_worktree(project, branch_name):
    """Create a git worktree for an agent session."""
    path = Path(project["path"])
    worktree_path = path / branch_name

    # Find default branch
    start_point = "HEAD"
    for branch in ["main", "master"]:
        result = subprocess.run(
            ["git", "-C", str(path), "rev-parse", "--verify", branch],
            capture_output=True, timeout=5)
        if result.returncode == 0:
            start_point = branch
            break

    result = subprocess.run(
        ["git", "-C", str(path), "worktree", "add", "-b",
         branch_name, str(worktree_path), start_point],
        capture_output=True, text=True, timeout=30)
    if result.returncode == 0:
        return str(worktree_path)
    else:
        return None

# --- Claude session discovery ---

def discover_claude_sessions(hours=24):
    """Find recent Claude Code sessions on disk."""
    claude_dir = Path.home() / ".claude" / "projects"
    if not claude_dir.exists():
        return []

    cutoff = time.time() - hours * 3600
    sessions = []

    for proj_dir in claude_dir.iterdir():
        if not proj_dir.is_dir():
            continue
        for f in proj_dir.iterdir():
            if not f.name.endswith(".jsonl") or "/" in f.name:
                continue
            try:
                stat = f.stat()
            except OSError:
                continue
            if stat.st_mtime < cutoff:
                continue

            session_id = f.stem
            # Get first user message as summary
            summary = ""
            try:
                with open(f) as fh:
                    for line in fh:
                        try:
                            d = json.loads(line)
                            if d.get("type") == "user":
                                msg = d.get("message", {})
                                content = msg.get("content", "") if isinstance(msg, dict) else ""
                                if isinstance(content, list):
                                    for item in content:
                                        if isinstance(item, dict) and item.get("type") == "text":
                                            summary = item["text"][:80]
                                            break
                                elif isinstance(content, str):
                                    summary = content[:80]
                                if summary:
                                    break
                        except json.JSONDecodeError:
                            continue
            except OSError:
                pass

            age_min = int((time.time() - stat.st_mtime) / 60)
            if age_min < 60:
                age_str = f"{age_min}m ago"
            else:
                age_str = f"{age_min // 60}h ago"

            sessions.append({
                "session_id": session_id,
                "project_dir": proj_dir.name,
                "age": age_str,
                "summary": summary or "(no summary)",
            })

    return sorted(sessions, key=lambda s: s["age"])[:15]

# --- Tool implementations ---

TOOLS = [
    {
        "name": "start_session",
        "description": "Start a new Claude agent session for a project. Creates a Discord thread and git worktree. Returns the thread info.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "project": {
                    "type": "string",
                    "description": "Project name, number from the list, or a prefix/substring to fuzzy match"
                },
                "agent": {
                    "type": "string",
                    "description": "Agent type: claude, codex, or gemini",
                    "default": "claude",
                    "enum": ["claude", "codex", "gemini"]
                },
                "thread_name": {
                    "type": "string",
                    "description": "Short descriptive name for the thread (max 100 chars). If omitted, uses a default name."
                }
            },
            "required": ["project"]
        }
    },
    {
        "name": "list_projects",
        "description": "List all discovered projects that can have agent sessions started on them.",
        "inputSchema": {
            "type": "object",
            "properties": {}
        }
    },
    {
        "name": "list_sessions",
        "description": "List active bot sessions (Discord threads with agent sessions attached).",
        "inputSchema": {
            "type": "object",
            "properties": {}
        }
    },
    {
        "name": "list_claude_sessions",
        "description": "List recent Claude Code sessions running on this machine (last 24h). Useful for finding sessions to resume.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "hours": {
                    "type": "integer",
                    "description": "How many hours back to search",
                    "default": 24
                }
            }
        }
    },
    {
        "name": "resume_session",
        "description": "Resume an existing Claude Code session in a new Discord thread. Use list_claude_sessions first to find the session ID.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "session_id": {
                    "type": "string",
                    "description": "Claude session ID or prefix (at least 8 characters)"
                }
            },
            "required": ["session_id"]
        }
    },
    {
        "name": "restart_bot",
        "description": "Rebuild the discord-agents bot from source and restart it. Use after code changes.",
        "inputSchema": {
            "type": "object",
            "properties": {}
        }
    },
    {
        "name": "rename_thread",
        "description": "Rename a Discord thread. Use from the control channel to rename any thread by ID, or specify the thread_id of the thread to rename.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID (snowflake) to rename"
                },
                "name": {
                    "type": "string",
                    "description": "New name for the thread (max 100 characters)"
                }
            },
            "required": ["thread_id", "name"]
        }
    },
    {
        "name": "cleanup_channels",
        "description": "Delete stale Discord channels that don't match any current project.",
        "inputSchema": {
            "type": "object",
            "properties": {}
        }
    }
]

def handle_tool_call(name, arguments, config, projects):
    token = config.get("discord_token", "")
    guild_id = config.get("guild_id", "")
    control_channel = config.get("control_channel_id")

    if name == "list_projects":
        lines = [f"{i+1}. **{p['name']}** — `{p['path']}`"
                 + (" [bare]" if p["is_bare"] else "")
                 for i, p in enumerate(projects)]
        return "\n".join(lines) if lines else "No projects found."

    elif name == "list_sessions":
        sessions = load_sessions()
        if not sessions:
            return "No active sessions."
        lines = [f"- **{s['project_name']}** / {s['agent_kind']} — {s['message_count']} messages (thread: <#{s['thread_id']}>)"
                 for s in sessions]
        return "\n".join(lines)

    elif name == "list_claude_sessions":
        hours = arguments.get("hours", 24)
        sessions = discover_claude_sessions(hours)
        if not sessions:
            return "No recent Claude sessions found."
        lines = [f"- `{s['session_id'][:8]}` {s['age']} — {s['summary']}"
                 for s in sessions]
        return "\n".join(lines) + f"\n\nUse resume_session with a session ID prefix to attach."

    elif name == "start_session":
        project_query = arguments.get("project", "")
        agent = arguments.get("agent", "claude")
        proj = find_project_fuzzy(projects, project_query)
        if not proj:
            suggestions = [p for p in projects if project_query.lower() in p["name"].lower()]
            if suggestions:
                return f"No unique match for '{project_query}'. Did you mean:\n" + \
                       "\n".join(f"- {p['name']}" for p in suggestions[:5])
            return f"No project matching '{project_query}'. Use list_projects to see available projects."

        # Create worktree
        branch_name = f"agent/{agent}-{uuid.uuid4().hex[:8]}"
        worktree_path = create_worktree(proj, branch_name)
        if not worktree_path:
            worktree_path = find_working_dir(proj)

        # Create Discord thread
        channel_id = control_channel or ""
        # Try to find project channel
        if guild_id and token:
            channels = discord_request("GET", f"/guilds/{guild_id}/channels", token)
            if isinstance(channels, list):
                for ch in channels:
                    if (ch.get("name", "").lower() == proj["name"].lower()
                            and ch.get("type") == 0):
                        channel_id = ch["id"]
                        break

        if not channel_id:
            return f"No channel found for thread creation. Start the session manually."

        thread_name = arguments.get("thread_name", "").strip()
        if not thread_name or len(thread_name) > 100:
            thread_name = f"{agent} / {proj['name']}"
        result = discord_request("POST", f"/channels/{channel_id}/threads", token, {
            "name": thread_name,
            "type": 11,  # PUBLIC_THREAD
            "auto_archive_duration": 1440,
        })
        if "error" in result:
            return f"Failed to create thread: {result['error']}"

        thread_id = result.get("id", "")
        session_id = str(uuid.uuid4())

        # Add to sessions
        sessions = load_sessions()
        sessions.append({
            "project_name": proj["name"],
            "working_dir": worktree_path,
            "agent_kind": agent,
            "session_id": session_id,
            "thread_id": thread_id,
            "message_count": 0,
        })
        save_sessions(sessions)

        # Post welcome message
        discord_request("POST", f"/channels/{thread_id}/messages", token, {
            "content": f"**{agent}** session started for **{proj['name']}**\n"
                      f"Branch: `{branch_name}`\n"
                      f"Working in: `{worktree_path}`\n"
                      f"Send a message to interact with the agent."
        })

        return f"Started {agent} session for **{proj['name']}** in <#{thread_id}>.\nWorking in: `{worktree_path}`"

    elif name == "resume_session":
        sid_prefix = arguments.get("session_id", "")
        # Find matching session
        claude_dir = Path.home() / ".claude" / "projects"
        found = None
        for proj_dir in claude_dir.iterdir():
            if not proj_dir.is_dir():
                continue
            for f in proj_dir.iterdir():
                if f.name.endswith(".jsonl") and f.stem.startswith(sid_prefix):
                    found = (f.stem, proj_dir.name)
                    break
            if found:
                break

        if not found:
            return f"No Claude session found matching `{sid_prefix}`."

        full_sid, proj_dir_name = found
        # Resolve working dir from project dir name
        working_dir = resolve_project_dir(proj_dir_name)

        channel_id = control_channel or ""
        if not channel_id:
            return "No control channel configured."

        thread_name = f"resume / {full_sid[:8]}"
        result = discord_request("POST", f"/channels/{channel_id}/threads", token, {
            "name": thread_name,
            "type": 11,
            "auto_archive_duration": 1440,
        })
        if "error" in result:
            return f"Failed to create thread: {result['error']}"

        thread_id = result.get("id", "")

        sessions = load_sessions()
        sessions.append({
            "project_name": Path(working_dir).name,
            "working_dir": working_dir,
            "agent_kind": "claude",
            "session_id": full_sid,
            "thread_id": thread_id,
            "message_count": 1,  # >0 so bot uses --resume
        })
        save_sessions(sessions)

        discord_request("POST", f"/channels/{thread_id}/messages", token, {
            "content": f"**Resumed** Claude session `{full_sid[:8]}`\n"
                      f"Working in: `{working_dir}`\n"
                      f"Send a message to continue."
        })

        return f"Resumed session `{full_sid[:8]}` in <#{thread_id}>."

    elif name == "restart_bot":
        # Signal the bot to restart via SIGUSR1, which triggers the full
        # drain → reap → build → spawn pipeline in the bot process.
        # Read the bot's PID from the pidfile and send the signal.
        pidfile = Path.home() / ".config/discord-agents/discord-agents.pid"
        try:
            pid = int(pidfile.read_text().strip())
            os.kill(pid, signal.SIGUSR1)
            return f"Restart signal (SIGUSR1) sent to bot (pid {pid}). It will drain active sessions, then rebuild and restart."
        except FileNotFoundError:
            return "Pidfile not found — is the bot running?"
        except ProcessLookupError:
            return f"Bot process (pid {pid}) not found — it may have already exited."
        except Exception as e:
            return f"Failed to signal restart: {e}"

    elif name == "rename_thread":
        thread_id = arguments.get("thread_id", "")
        new_name = arguments.get("name", "")
        if not thread_id or not new_name:
            return "Both thread_id and name are required."
        if len(new_name) > 100:
            new_name = new_name[:100]
        result = discord_request("PATCH", f"/channels/{thread_id}", token, {
            "name": new_name,
        })
        if "error" in result:
            return f"Failed to rename thread: {result['error']}"
        return f"Renamed thread <#{thread_id}> to **{new_name}**."

    elif name == "cleanup_channels":
        if not guild_id or not token:
            return "No guild configured."
        channels = discord_request("GET", f"/guilds/{guild_id}/channels", token)
        if not isinstance(channels, list):
            return f"Failed to get channels: {channels}"

        # Find Agent Projects category
        cat_id = None
        for ch in channels:
            if ch.get("type") == 4 and ch.get("name") == "Agent Projects":
                cat_id = ch["id"]
                break
        if not cat_id:
            return "No Agent Projects category found."

        project_names = {p["name"].lower() for p in projects}
        seen = set()
        to_delete = []
        for ch in channels:
            if ch.get("parent_id") != cat_id or ch.get("type") != 0:
                continue
            name = ch.get("name", "")
            if name.lower() in seen or name.lower() not in project_names:
                to_delete.append(ch)
            seen.add(name.lower())

        if not to_delete:
            return "No stale channels to clean up."

        deleted = 0
        for ch in to_delete:
            result = discord_request("DELETE", f"/channels/{ch['id']}", token)
            if "error" not in result:
                deleted += 1
            time.sleep(0.5)

        return f"Deleted {deleted} stale channels."

    return f"Unknown tool: {name}"

def resolve_project_dir(proj_name):
    """Resolve Claude's project dir name to a filesystem path.
    e.g. '-home-tedks-Projects-claude-discord' -> '/home/tedks/Projects/claude-discord'
    by greedy filesystem walk."""
    s = proj_name.lstrip("-")
    parts = s.split("-")

    def resolve(path, remaining_parts):
        if not remaining_parts:
            return path
        # Try longest prefix that exists
        for n in range(len(remaining_parts), 0, -1):
            candidate = "-".join(remaining_parts[:n])
            candidate_path = os.path.join(path, candidate)
            if os.path.exists(candidate_path):
                return resolve(candidate_path, remaining_parts[n:])
        # Nothing matched, just take first part
        return resolve(os.path.join(path, remaining_parts[0]), remaining_parts[1:])

    return resolve("/", parts)

# --- MCP JSON-RPC server ---

def send_response(id, result):
    msg = {"jsonrpc": "2.0", "id": id, "result": result}
    out = json.dumps(msg)
    sys.stdout.write(out + "\n")
    sys.stdout.flush()

def send_error(id, code, message):
    msg = {"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}}
    out = json.dumps(msg)
    sys.stdout.write(out + "\n")
    sys.stdout.flush()

def main():
    config = load_config()
    base_dirs = config.get("base_directories", [])
    projects = discover_projects(base_dirs)

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue

        id = msg.get("id")
        method = msg.get("method", "")

        if method == "initialize":
            send_response(id, {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "serverInfo": {
                    "name": "discord-agents-mcp",
                    "version": "0.1.0"
                }
            })
        elif method == "notifications/initialized":
            pass  # No response needed for notifications
        elif method == "tools/list":
            send_response(id, {"tools": TOOLS})
        elif method == "tools/call":
            params = msg.get("params", {})
            tool_name = params.get("name", "")
            arguments = params.get("arguments", {})
            try:
                result_text = handle_tool_call(tool_name, arguments, config, projects)
                send_response(id, {
                    "content": [{"type": "text", "text": result_text}]
                })
            except Exception as e:
                send_response(id, {
                    "content": [{"type": "text", "text": f"Error: {e}"}],
                    "isError": True
                })
        elif method == "ping":
            send_response(id, {})
        elif id is not None:
            send_error(id, -32601, f"Unknown method: {method}")

if __name__ == "__main__":
    main()
