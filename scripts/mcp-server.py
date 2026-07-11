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
import os
import socket
import sys
import tempfile
from pathlib import Path

# --- Configuration ---

def legacy_config_dir():
    home = os.environ.get("HOME", "")
    if home:
        return Path(home) / ".config" / "discord-agents"
    return None

def config_dir_has_state(config_dir):
    if not config_dir.exists():
        return False
    state_files = ("config.json", "settings.json", "sessions.json", "control.sock")
    return any((config_dir / name).exists() for name in state_files)

def app_config_dir():
    xdg = os.environ.get("XDG_CONFIG_HOME", "")
    if xdg:
        xdg_dir = Path(xdg) / "discord-agents"
        legacy_dir = legacy_config_dir()
        if not config_dir_has_state(xdg_dir) and legacy_dir and legacy_dir.exists():
            return legacy_dir
        return xdg_dir
    legacy_dir = legacy_config_dir()
    if legacy_dir:
        return legacy_dir
    return Path(tempfile.gettempdir()) / f"discord-agents-{os.getuid()}"

CONFIG_DIR = app_config_dir()
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
        "description": "Start a new agent session for a project (claude, codex, or gemini). Creates a Discord thread and git worktree. Returns the thread info.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "project": {
                    "type": "string",
                    "description": "Project name, number from the list, or a prefix/substring to fuzzy match"
                },
                "agent": {
                    "type": "string",
                    "description": "Agent type: claude, codex, or gemini. If omitted, uses the bot's current effective top-level agent (default agent unless a rescue agent is active under disk pressure).",
                    "enum": ["claude", "codex", "gemini"]
                },
                "thread_name": {
                    "type": "string",
                    "description": "Short descriptive name for the thread (max 80 chars). If omitted, uses a default name.",
                    "maxLength": 80
                },
                "initial_prompt": {
                    "type": "string",
                    "description": "First message to send to the new agent. Posted visibly in the thread, then the agent starts working on it immediately — the user does not need to send a follow-up. Keep it concise: describe the goal and any key context, not step-by-step instructions. Capped at 1900 *bytes* on the server (UTF-8 codepoint-aware truncation, so multi-byte characters can shorten the effective char count); the maxLength below is the worst case (all-ASCII).",
                    "maxLength": 1900
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
        "name": "import_project",
        "description": "Clone a GitHub HTTPS or SSH URL into the bot's project registry, create its Discord project channel, and make the channel session-ready. Reuses local git credentials; no token is accepted.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "url": {
                    "type": "string",
                    "description": "GitHub repository URL, e.g. https://github.com/owner/repo.git or git@github.com:owner/repo.git"
                },
                "name": {
                    "type": "string",
                    "description": "Optional local project directory name under the configured base directory. Defaults to the GitHub repository name.",
                    "maxLength": 100
                }
            },
            "required": ["url"]
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
        "name": "send_message",
        "description": "Send a visible user-style message to another active session thread, then route it through the same handler as a Discord user message. Use list_sessions to find thread IDs. The message is queued if the target session is busy. Do not send command-looking messages. If replying to an inter-agent message, pass its remaining_hops value so loops terminate.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID of the destination active session"
                },
                "message": {
                    "type": "string",
                    "description": "Plain message text to send. Must not start with ! and must fit in one Discord message. The bot enforces a 1600-byte cap, so non-ASCII text may allow fewer characters.",
                    "maxLength": 1600
                },
                "source_thread_id": {
                    "type": "string",
                    "description": "Optional claimed Discord thread ID of the sending session, if known. This is displayed as an untrusted claimed source until caller context is wired in."
                },
                "remaining_hops": {
                    "type": "integer",
                    "description": "Loop guard. Defaults to 3 for a new chain. If replying to an inter-agent message, pass the remaining_hops value shown in that message.",
                    "minimum": 1,
                    "maximum": 5,
                    "default": 3
                }
            },
            "required": ["thread_id", "message"]
        }
    },
    {
        "name": "stop_session",
        "description": "Stop an active bot session by Discord thread ID. Idle sessions stop immediately; busy sessions terminate the active agent run and then clean up the session.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID of the session to stop"
                }
            },
            "required": ["thread_id"]
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
        "name": "list_codex_sessions",
        "description": "List recent Codex CLI sessions on this machine (last 24h). Useful for finding sessions to resume.",
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
        "name": "default_agent",
        "description": "Show or set the default agent used for new top-level sessions. Existing idle top-level sessions reset to a fresh session immediately; busy ones reset after their queued work finishes.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "agent": {
                    "type": "string",
                    "description": "Agent type to make the default. Omit to read the current default.",
                    "enum": ["claude", "codex", "gemini"]
                }
            }
        }
    },
    {
        "name": "rescue_agent",
        "description": "Show or set the rescue agent automatically used for new top-level sessions under disk pressure. Use agent=off to disable it.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "agent": {
                    "type": "string",
                    "description": "Agent type to use as the rescue agent, or off to disable rescue mode. Omit to read the current setting.",
                    "enum": ["claude", "codex", "gemini", "off"]
                }
            }
        }
    },
    {
        "name": "get_agent_config",
        "description": "Show the per-session agent configuration for a Discord thread: current values, every supported config value, a single-command briefing, and login repair hint.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID of the session"
                }
            },
            "required": ["thread_id"]
        }
    },
    {
        "name": "set_model",
        "description": "Set or clear the model override for an existing Discord agent session. Pass model explicitly; use model=default or an empty/null value to clear.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID of the session"
                },
                "model": {
                    "type": ["string", "null"],
                    "description": "Model name to pass to the agent CLI, or default/null to clear"
                }
            },
            "required": ["thread_id", "model"]
        }
    },
    {
        "name": "set_effort",
        "description": "Set or clear the reasoning effort override for an existing Discord agent session. Pass effort explicitly. Claude supports low/medium/high/xhigh/max; Codex supports low/medium/high/xhigh here; Gemini effort is unsupported.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID of the session"
                },
                "effort": {
                    "type": ["string", "null"],
                    "description": "Effort: low, medium, high, xhigh, max, or default/null to clear",
                    "enum": ["low", "medium", "high", "xhigh", "max", "default", None]
                }
            },
            "required": ["thread_id", "effort"]
        }
    },
    {
        "name": "set_goal",
        "description": "Set, update, or clear a persisted Codex session goal. With current codex exec integration this is injected as prompt context; native Codex /goal requires app-server.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID of the session"
                },
                "objective": {
                    "type": ["string", "null"],
                    "description": "Goal objective, max 4000 bytes. Required for a new goal; omit to update status/token_budget on an existing goal."
                },
                "status": {
                    "type": "string",
                    "description": "Goal status",
                    "enum": ["active", "paused", "blocked", "usageLimited", "budgetLimited", "complete"]
                },
                "token_budget": {
                    "type": ["integer", "null"],
                    "description": "Optional positive token budget"
                },
                "clear": {
                    "type": "boolean",
                    "description": "Clear the stored goal"
                }
            },
            "required": ["thread_id"]
        }
    },
    {
        "name": "start_login_flow",
        "description": "Return the local command needed to repair login for an agent or session. The bot does not run OAuth itself.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "thread_id": {
                    "type": "string",
                    "description": "Discord thread ID whose agent needs login"
                },
                "agent": {
                    "type": "string",
                    "description": "Agent kind when no thread_id is supplied",
                    "enum": ["claude", "codex", "gemini"]
                }
            }
        }
    },
    {
        "name": "resume_session",
        "description": "Resume an existing Claude, Codex, or Gemini session in a new Discord thread. Use list_claude_sessions / list_codex_sessions / list_gemini_sessions to find a session ID. With kind unspecified, the bot tries the current effective top-level agent first, then the others.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "session_id": {
                    "type": "string",
                    "description": "Session ID or prefix (at least 8 characters)"
                },
                "kind": {
                    "type": "string",
                    "enum": ["claude", "codex", "gemini"],
                    "description": "Which session store to search. Omit to try the current effective top-level agent first, then the others."
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

    elif name == "import_project":
        result = control_request("import_project", arguments, timeout=300)
        if "error" in result:
            return result["error"]
        pname = result.get("project_name", "")
        cid = result.get("channel_id", "")
        wd = result.get("working_dir", "")
        existing = result.get("existing", False)
        action = "already existed" if existing else "imported"
        return f"Project **{pname}** {action} in <#{cid}>.\nWorking in: `{wd}`"

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

    elif name == "send_message":
        result = control_request("send_message", arguments)
        if "error" in result:
            return result["error"]
        tid = result.get("thread_id", "")
        hops = result.get("remaining_hops", 0)
        state = result.get("state", "sent")
        if state == "posted_not_routed":
            return f"Posted message to <#{tid}>, but the target session disappeared before routing. remaining_hops={hops}."
        return f"Sent message to <#{tid}>. remaining_hops={hops}."

    elif name == "stop_session":
        result = control_request("stop_session", arguments)
        if "error" in result:
            return result["error"]
        return result.get("message", "Stop requested.")

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

    elif name == "list_codex_sessions":
        result = control_request("list_codex_sessions", arguments)
        if "error" in result:
            return result["error"]
        sessions = result.get("sessions", [])
        if not sessions:
            return "No recent Codex sessions found."
        lines = []
        for s in sessions:
            age = s.get("age_minutes", 0)
            age_str = f"{age}m ago" if age < 60 else f"{age // 60}h ago"
            summary = s.get("summary", "(no summary)")
            wd = s.get("working_dir", "") or "(unknown project)"
            lines.append(f"- `{s['session_id_short']}` {age_str} — {wd} — {summary}")
        return "\n".join(lines) + "\n\nUse resume_session with kind=codex to attach."

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
        kind = result.get("agent_kind", "")
        kind_label = f"{kind.capitalize()} " if kind else ""
        return f"Resumed {kind_label}session `{sid}` in <#{tid}>."

    elif name == "default_agent":
        result = control_request("default_agent", arguments)
        if "error" in result:
            return result["error"]
        agent = result.get("agent", "")
        effective = result.get("effective_top_level_agent", agent)
        rescue = result.get("rescue_agent")
        rescue_active = result.get("disk_rescue_active", False)
        if (arguments or {}).get("agent") is None:
            parts = [f"Default agent: `{agent}`."]
            if rescue:
                suffix = " (active)" if rescue_active else ""
                parts.append(f"Rescue agent: `{rescue}`{suffix}.")
            if effective and effective != agent:
                parts.append(f"Effective top-level agent: `{effective}`.")
            return " ".join(parts)
        reset_count = result.get("reset_count", 0)
        busy_count = result.get("busy_count", 0)
        parts = [f"Default agent set to `{agent}`."]
        if reset_count:
            noun = "session" if reset_count == 1 else "sessions"
            parts.append(f"Reset {reset_count} idle top-level {noun} immediately.")
        if busy_count:
            noun = "session" if busy_count == 1 else "sessions"
            parts.append(f"{busy_count} busy top-level {noun} will switch after queued work finishes.")
        if rescue_active and effective and effective != agent:
            parts.append(f"Disk pressure is active, so top-level sessions currently use rescue agent `{effective}`.")
        return " ".join(parts)

    elif name == "rescue_agent":
        result = control_request("rescue_agent", arguments)
        if "error" in result:
            return result["error"]
        agent = result.get("agent")
        effective = result.get("effective_top_level_agent", "")
        rescue_active = result.get("disk_rescue_active", False)
        if "agent" not in (arguments or {}):
            if agent:
                parts = [f"Rescue agent: `{agent}`."]
            else:
                parts = ["Rescue agent: disabled."]
            if rescue_active and effective:
                parts.append(f"Disk pressure is active, so top-level sessions currently use `{effective}`.")
            return " ".join(parts)
        reset_count = result.get("reset_count", 0)
        busy_count = result.get("busy_count", 0)
        parts = [f"Rescue agent set to `{agent}`." if agent else "Rescue agent disabled."]
        if reset_count:
            noun = "session" if reset_count == 1 else "sessions"
            parts.append(f"Reset {reset_count} idle top-level {noun} immediately.")
        if busy_count:
            noun = "session" if busy_count == 1 else "sessions"
            parts.append(f"{busy_count} busy top-level {noun} will switch after queued work finishes.")
        if rescue_active and effective:
            parts.append(f"Disk pressure is active, so top-level sessions currently use `{effective}`.")
        return " ".join(parts)

    elif name == "get_agent_config":
        result = control_request("get_agent_config", arguments)
        if "error" in result:
            return result["error"]

        def render_values(values):
            if isinstance(values, list):
                rendered = []
                for value in values:
                    if value is None:
                        rendered.append("`null`")
                    elif value == "":
                        rendered.append('`""`')
                    else:
                        rendered.append(f"`{value}`")
                return ", ".join(rendered)
            return str(values)

        model = result.get("model") or "default"
        effort = result.get("effort") or "default"
        goal = result.get("goal")
        lines = [
            f"Agent: `{result.get('agent_kind', '')}`",
            f"Model: `{model}`",
            f"Effort: `{effort}`",
        ]
        if goal:
            objective = goal.get("objective", "")
            status = goal.get("status", "active")
            budget = goal.get("token_budget")
            suffix = f", token budget {budget}" if budget else ""
            lines.append(f"Goal: `{status}`{suffix} — {objective}")
        else:
            lines.append("Goal: none")
        login = result.get("login_help") or {}
        if login:
            lines.append(f"Login repair: run `{login.get('command', '')}` on the bot host.")
        mechanism = result.get("goal_mechanism")
        if mechanism:
            lines.append(f"Goal mechanism: {mechanism}.")
        options = result.get("configuration_options") or {}
        if options:
            lines.append("")
            lines.append("Potential values:")
            agent_options = options.get("agent_kind") or {}
            if agent_options.get("values"):
                set_with = agent_options.get("set_with", "chosen when the session starts")
                lines.append(
                    f"- Agent kind: {render_values(agent_options.get('values'))}; "
                    f"current thread is read-only here ({set_with})"
                )
            model_options = options.get("model") or {}
            if model_options:
                model_values = model_options.get("values", "any non-empty model string")
                clear_values = render_values(model_options.get("clear_values", []))
                lines.append(
                    f"- Model: {model_values}; clear with {clear_values}; "
                    f"max {model_options.get('max_bytes', 200)} bytes"
                )
            effort_options = options.get("effort") or {}
            if effort_options:
                if effort_options.get("supported"):
                    lines.append(
                        f"- Effort: {render_values(effort_options.get('values', []))}; "
                        f"clear with {render_values(effort_options.get('clear_values', []))}"
                    )
                else:
                    lines.append(f"- Effort: unsupported for `{result.get('agent_kind', '')}`")
            goal_options = options.get("goal") or {}
            if goal_options:
                if goal_options.get("supported"):
                    objective = goal_options.get("objective") or {}
                    lines.append(
                        f"- Goal: objective is {objective.get('values', 'any non-empty string')} "
                        f"(max {objective.get('max_bytes', 4000)} bytes); "
                        f"status {render_values(goal_options.get('status_values', []))}; "
                        f"token_budget {goal_options.get('token_budget', {}).get('values', 'positive integer or null')}; "
                        f"clear with `{goal_options.get('clear_values', 'clear=true')}`"
                    )
                else:
                    lines.append("- Goal: unsupported for this agent")
        briefing = result.get("command_briefing")
        if briefing:
            lines.append("")
            lines.append(f"Briefing: {briefing}")
        return "\n".join(lines)

    elif name == "set_model":
        result = control_request("set_model", arguments)
        if "error" in result:
            return result["error"]
        model = result.get("model") or "default"
        return f"Model override for <#{result.get('thread_id', '')}> is now `{model}`."

    elif name == "set_effort":
        result = control_request("set_effort", arguments)
        if "error" in result:
            return result["error"]
        effort = result.get("effort") or "default"
        return f"Effort override for <#{result.get('thread_id', '')}> is now `{effort}`."

    elif name == "set_goal":
        result = control_request("set_goal", arguments)
        if "error" in result:
            return result["error"]
        goal = result.get("goal")
        if not goal:
            return f"Goal cleared for <#{result.get('thread_id', '')}>."
        budget = goal.get("token_budget")
        suffix = f" Token budget: `{budget}`." if budget else ""
        mechanism = result.get("goal_mechanism")
        mechanism_text = f" Mechanism: {mechanism}." if mechanism else ""
        return (
            f"Goal set for <#{result.get('thread_id', '')}>: "
            f"`{goal.get('status', 'active')}` — {goal.get('objective', '')}."
            f"{suffix}{mechanism_text}"
        )

    elif name == "start_login_flow":
        result = control_request("start_login_flow", arguments)
        if "error" in result:
            return result["error"]
        login = result.get("login") or {}
        command = login.get("command", "")
        note = login.get("note", "")
        message = result.get("message", "")
        return f"{message}\n\nRun on bot host: `{command}`\n{note}"

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
