#!/usr/bin/env python3
"""MCP server for discord-agents bot control.

Thin stdio-to-UDS proxy: receives MCP tool calls via JSON-RPC over stdin,
forwards them to the bot's control API over a Unix domain socket, and
returns the formatted response.

All session state, Discord REST calls, and worktree management are owned
by the bot process. This server never touches sessions.json or Discord
directly.

Protocol: JSON-RPC 2.0 over stdio (MCP standard).
"""

import json
import socket
import sys
from pathlib import Path

# --- Configuration ---

CONFIG_DIR = Path.home() / ".config" / "discord-agents"
CONTROL_SOCKET = CONFIG_DIR / "control.sock"

# --- Bot control API client ---

def control_request(method, params=None, timeout=60):
    """Send a JSON request to the bot's control API over Unix domain socket.
    Returns the parsed JSON response, or {"error": "..."} on failure."""
    request = {"method": method}
    if params:
        request["params"] = params
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        sock.connect(str(CONTROL_SOCKET))
        sock.sendall((json.dumps(request) + "\n").encode())
        # Read response (one JSON line)
        data = b""
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                break
            data += chunk
            if b"\n" in data:
                break
        sock.close()
        return json.loads(data.decode().strip())
    except FileNotFoundError:
        return {"error": "Bot is not running (control socket not found)."}
    except ConnectionRefusedError:
        return {"error": "Bot is not running (connection refused)."}
    except socket.timeout:
        return {"error": "Bot did not respond in time."}
    except Exception as e:
        return {"error": f"Control API error: {e}"}

# --- Tool definitions ---

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
                    "description": "Short descriptive name for the thread (max 80 chars). If omitted, uses a default name.",
                    "maxLength": 80
                },
                "initial_prompt": {
                    "type": "string",
                    "description": "Context for the new session agent about what task to work on. Passed to the agent on its first interaction. Keep it concise — describe the goal, not step-by-step instructions.",
                    "maxLength": 4000
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
        "name": "list_gemini_sessions",
        "description": "List recent Gemini CLI sessions on this machine (last 24h). Useful for finding sessions to resume.",
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
        "description": "Resume an existing Claude or Gemini session in a new Discord thread. Use list_claude_sessions / list_gemini_sessions to find a session ID. With kind unspecified, the bot tries Claude first, then Gemini.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "session_id": {
                    "type": "string",
                    "description": "Session ID or prefix (at least 8 characters)"
                },
                "kind": {
                    "type": "string",
                    "enum": ["claude", "gemini"],
                    "description": "Which session store to search. Omit to try Claude then Gemini."
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
    },
    {
        "name": "refresh_projects",
        "description": "Re-scan for new projects without restarting the bot. Use when a new project has been added to the base directories.",
        "inputSchema": {
            "type": "object",
            "properties": {}
        }
    }
]

# --- Tool handlers (thin UDS proxies) ---

def handle_tool_call(name, arguments):
    """Forward tool call to the bot's control API and format the response."""

    if name == "list_projects":
        result = control_request("list_projects")
        if "error" in result:
            return result["error"]
        projects = result.get("projects", [])
        lines = [f"{i+1}. **{p['name']}** — `{p['path']}`"
                 + (" [bare]" if p.get("is_bare") else "")
                 for i, p in enumerate(projects)]
        return "\n".join(lines) if lines else "No projects found."

    elif name == "list_sessions":
        result = control_request("list_sessions")
        if "error" in result:
            return result["error"]
        sessions = result.get("sessions", [])
        if not sessions:
            return "No active sessions."
        lines = [f"- **{s['project_name']}** / {s['agent_kind']} — {s['message_count']} messages (thread: <#{s['thread_id']}>)"
                 for s in sessions]
        return "\n".join(lines)

    elif name == "list_claude_sessions":
        result = control_request("list_claude_sessions", arguments)
        if "error" in result:
            return result["error"]
        sessions = result.get("sessions", [])
        if not sessions:
            return "No recent Claude sessions found."
        lines = []
        for s in sessions:
            age = s.get("age_minutes", 0)
            age_str = f"{age}m ago" if age < 60 else f"{age // 60}h ago"
            summary = s.get("summary", "(no summary)")
            lines.append(f"- `{s['session_id_short']}` {age_str} — {summary}")
        return "\n".join(lines) + "\n\nUse resume_session with a session ID prefix to attach."

    elif name == "list_gemini_sessions":
        result = control_request("list_gemini_sessions", arguments)
        if "error" in result:
            return result["error"]
        sessions = result.get("sessions", [])
        if not sessions:
            return "No recent Gemini sessions found."
        lines = []
        for s in sessions:
            age = s.get("age_minutes", 0)
            age_str = f"{age}m ago" if age < 60 else f"{age // 60}h ago"
            summary = s.get("summary", "(no summary)")
            wd = s.get("working_dir", "") or "(unknown project)"
            lines.append(f"- `{s['session_id_short']}` {age_str} — {wd} — {summary}")
        return "\n".join(lines) + "\n\nUse resume_session with kind=gemini to attach."

    elif name == "start_session":
        result = control_request("start_session", arguments, timeout=120)
        if "error" in result and "no project matching" in result["error"].lower():
            # Project not found — try refreshing project list first
            control_request("refresh_projects")
            result = control_request("start_session", arguments, timeout=120)
        if "error" in result:
            return result["error"]
        tid = result.get("thread_id", "")
        wd = result.get("working_dir", "")
        pname = result.get("project_name", "")
        return f"Started session for **{pname}** in <#{tid}>.\nWorking in: `{wd}`"

    elif name == "resume_session":
        result = control_request("resume_session", arguments, timeout=120)
        if "error" in result:
            return result["error"]
        tid = result.get("thread_id", "")
        sid = result.get("session_id", "")[:8]
        return f"Resumed session `{sid}` in <#{tid}>."

    elif name == "restart_bot":
        result = control_request("restart")
        if "error" in result:
            return result["error"]
        return result.get("message", "Restart initiated.")

    elif name == "rename_thread":
        result = control_request("rename_thread", arguments)
        if "error" in result:
            return result["error"]
        return result.get("message", "Renamed.")

    elif name == "cleanup_channels":
        result = control_request("cleanup_channels")
        if "error" in result:
            return result["error"]
        return result.get("message", "Done.")

    elif name == "refresh_projects":
        result = control_request("refresh_projects")
        if "error" in result:
            return result["error"]
        total = result.get("total", 0)
        delta = result.get("delta", 0)
        if delta > 0:
            return f"Refreshed: found {delta} new project{'s' if delta != 1 else ''} ({total} total)."
        return f"Refreshed: no new projects ({total} total)."

    return f"Unknown tool: {name}"

# --- MCP JSON-RPC server ---

def send_response(id, result):
    msg = {"jsonrpc": "2.0", "id": id, "result": result}
    sys.stdout.write(json.dumps(msg) + "\n")
    sys.stdout.flush()

def send_error(id, code, message):
    msg = {"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}}
    sys.stdout.write(json.dumps(msg) + "\n")
    sys.stdout.flush()

def main():
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
                    "version": "0.2.0"
                }
            })
        elif method == "notifications/initialized":
            pass
        elif method == "tools/list":
            send_response(id, {"tools": TOOLS})
        elif method == "tools/call":
            params = msg.get("params", {})
            tool_name = params.get("name", "")
            arguments = params.get("arguments", {})
            try:
                result_text = handle_tool_call(tool_name, arguments)
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
