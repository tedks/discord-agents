(** Shell scripts that run a command on an executor host over SSH.

    Pure: builds strings and parses marker lines; runs nothing. The
    transport lives in [Host_exec]. The protocol is described in
    docs/multi-host.md ("Remote run protocol"); the short version:

    - sshd hands the remote command to the user's login shell as one
      argument string, and Linux caps a single argument at 128 KiB. So
      the SSH command line is always the fixed {!bootstrap}, and the
      real script travels as one base64 line on the channel's stdin.
      POSIX [read] consumes a pipe byte by byte, so the rest of stdin
      is still there for the script.
    - The script reports progress on stdout with marker lines that
      carry a per-call nonce, so command output cannot forge them and
      the caller can tell "the remote never started" from "the command
      ran and exited 255" from "the connection dropped mid-run".
    - In [Run] mode the command gets its own session (process group);
      a watchdog kills that group when stdin reaches EOF, which happens
      whenever the local ssh process dies. A per-key [flock] stops a
      second run with the same key on the same host.

    Every word that comes from a caller is single-quoted, so the only
    shell syntax in a script is what this module writes. The one
    unquoted interpolation is the nonce, which [fresh_nonce] makes here
    from hex digits only.

    What the nonce does and does not buy: it keeps the command's output
    from colliding with a marker by accident or by echoing text it saw,
    and the reader accepts markers only in protocol order (error before
    start, one start, one exit). It is not authentication: a hostile
    process running as the same user could read it from the wrapper's
    memory and forge an exit status. The command is our own agent CLI,
    and a hostile same-user process already owns the account.

    Known limits, shared with local runs and tracked separately: one
    argument (a long prompt) cannot exceed 128 KiB, which [script]
    refuses up front; a command that exits while a background process
    still holds its stdout keeps the turn open until that process exits;
    and the delayed KILL could in principle reach a reused process-group
    id. *)

let bootstrap =
  {|sh -c 'IFS= read -r l; eval "$(printf %s "$l" | base64 -d)"'|}

type mode =
  | Capture
      (** Run to completion; stdout and stderr merged, as
          [Project.run_capture] does locally. *)
  | Run of { lock_key : string }
      (** Long-running agent turn: own process group, stdin-EOF
          watchdog, and a per-[lock_key] flock. *)

(* Shell variables the run script uses itself. An env entry with one of
   these names would be overwritten, or (lockdir, lockfile) could redirect
   the lock, so they are refused. *)
let reserved_env_names =
  ["lockdir"; "lockfile"; "donefile"; "lockrc"; "child"; "rc"; "i"]

let is_env_name s =
  not (List.mem s reserved_env_names)
  && s <> ""
  && (match s.[0] with 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false)
  && String.for_all (function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
    | _ -> false) s

let is_lock_key s =
  s <> ""
  && String.length s <= 128
  && s.[0] <> '.'
  && String.for_all (function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
    | _ -> false) s

(** NUL cannot appear in an argv string or environment value: exec
    would silently truncate at it. *)
let has_nul s = String.contains s '\000'

(** POSIX single-quoting. [Filename.quote] on Unix produces exactly
    this, but spelling it out keeps the script independent of the
    platform the bot happens to be built on. *)
let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '\'';
  String.iter (function
    | '\'' -> Buffer.add_string b {|'\''|}
    | c -> Buffer.add_char b c) s;
  Buffer.add_char b '\'';
  Buffer.contents b

let started_tag = "discord_agents.remote_started"
let exit_tag = "discord_agents.remote_exit"
let error_tag = "discord_agents.remote_error"

(* Marker lines are plain "<tag> <nonce> <value>" rather than JSON so
   that printf in the script needs no escaping. *)
let marker tag nonce value = Printf.sprintf "%s %s %s" tag nonce value

(* Linux's MAX_ARG_STRLEN: exec fails with E2BIG on any single argv or
   environment string longer than this, whatever ARG_MAX is. *)
let max_arg_bytes = 128 * 1024 - 1

let required_tools = ["flock"; "setsid"; "cat"; "sleep"; "touch"; "rm"]

let script ~nonce ~mode ~cwd ~env ~argv =
  let too_long s = String.length s > max_arg_bytes in
  (* The kernel limit covers an environment entry's whole NAME=value. *)
  let env_too_long (k, v) = String.length k + 1 + String.length v > max_arg_bytes in
  if argv = [] then Error "remote command must not be empty"
  else if List.exists has_nul (cwd :: argv) then
    Error "remote command and directory must not contain NUL bytes"
  else if not (cwd = "." || (cwd <> "" && cwd.[0] = '/')) then
    (* "-" would mean $OLDPWD to cd, and a relative path depends on the
       login directory. *)
    Error (Printf.sprintf "remote directory %S must be absolute" cwd)
  else if List.exists too_long argv
       || List.exists env_too_long env then
    Error (Printf.sprintf
      "an argument or environment value exceeds Linux's %d-byte \
       per-argument limit, so exec would fail"
      max_arg_bytes)
  else
    match List.find_opt (fun (k, v) -> not (is_env_name k) || has_nul v) env with
    | Some (k, _) -> Error (Printf.sprintf "invalid environment entry %S" k)
    | None ->
      let lock_ok =
        match mode with
        | Run { lock_key } -> is_lock_key lock_key
        | Capture -> true
      in
      if not lock_ok then Error "lock key must match [A-Za-z0-9._-]{1,128}"
      else begin
        let b = Buffer.create 1024 in
        let line fmt = Printf.ksprintf (fun s ->
          Buffer.add_string b s; Buffer.add_char b '\n') fmt in
        (* Every marker starts with a newline so it begins a line even
           after output (a login banner, or the command's last line)
           that did not end in one; readers skip the empty line. *)
        let marker_line tag value =
          Printf.sprintf "printf '\\n%%s\\n' %s" (quote (marker tag nonce value))
        in
        let err code = marker_line error_tag code in
        line "set -u";
        (* Before the exports, so an env entry named HOME cannot move
           the lock and let two runs of one session miss each other. *)
        line "lockdir=\"$HOME/.cache/discord-agents/run\"";
        (match mode with
         | Run { lock_key } ->
           line "lockfile=\"$lockdir\"/%s.lock" (quote lock_key);
           (* Per invocation: the wrapper pid and this call's nonce, so
              no two runs ever share one, even if a pid is reused. '+'
              cannot occur in a lock key, so no key's pattern below can
              match another key's file (keys may contain dots). *)
           line "donefile=\"$lockfile+$$-%s.done\"" nonce
         | Capture -> ());
        List.iter (fun (k, v) -> line "export %s=%s" k (quote v)) env;
        line "set -- %s" (String.concat " " (List.map quote argv));
        line "cd %s 2>/dev/null || { %s; exit 1; }" (quote cwd)
          (err "missing_cwd");
        (match mode with
         | Capture ->
           line "%s" (marker_line started_tag "0");
           line "\"$@\" 2>&1";
           line "rc=$?";
           line "printf '\\n%%s %%s\\n' %s \"$rc\""
             (quote (exit_tag ^ " " ^ nonce));
           line "exit 0"
         | Run _ ->
           (* A missing tool would otherwise surface as "busy" (flock),
              as the command exiting 127 (setsid), or as a watchdog that
              kills a healthy run at once (cat). *)
           List.iter (fun tool ->
             line "command -v %s >/dev/null 2>&1 || { %s; exit 1; }"
               tool (err ("missing_tool:" ^ tool)))
             required_tools;
           (* fd 3 keeps the SSH channel's stdin for the watchdog; the
              command itself gets /dev/null and neither fd 3 nor the
              lock fd 9. *)
           line "exec 3<&0";
           (* [exec 9>file] is a special builtin: a failed redirection
              would kill the shell before any [||] ran. Create it first
              with an ordinary command. *)
           line "mkdir -p \"$lockdir\" 2>/dev/null && touch \"$lockfile\" 2>/dev/null \
                 || { %s; exit 1; }" (err "lock_failed");
           line "exec 9>\"$lockfile\"";
           (* flock -n exits 1 on contention; anything else is a fault. *)
           line "flock -n 9";
           line "lockrc=$?";
           line "if [ \"$lockrc\" -eq 1 ]; then %s; exit 1; fi" (err "busy");
           line "if [ \"$lockrc\" -ne 0 ]; then %s; exit 1; fi" (err "lock_failed");
           (* Holding the lock means no earlier watchdog for this key is
              alive (each keeps fd 9 until it ends), so its done-files are
              stale: sweep them, which also stops one leaking per stop.
              The watchdog's disarm signal must start absent, or a stop
              would be mistaken for a normal finish; refuse to run rather
              than run unguarded. *)
           line "rm -f \"$lockfile\"+*.done 2>/dev/null";
           line "if [ -e \"$donefile\" ]; then %s; exit 1; fi" (err "lock_failed");
           (* The command prints its own started marker, with its own pid
              (setsid does not fork here: a background job is not a group
              leader, so the pid is the new session's pgid), then execs.
              So the wrapper writes nothing to the channel between spawn
              and arming the watchdog, and a channel that is already gone
              kills the marker's printf before the command runs. *)
           (* [|| exit 1]: if the marker cannot be delivered (SIGPIPE may
              be inherited as ignored, so printf just fails), the command
              must not run unannounced. *)
           line "setsid --wait sh -c 'printf \"\\n%%s %%s\\n\" \"$1\" \"$$\" || exit 1; shift; exec \"$@\"' \
                 discord-agents-run %s \"$@\" </dev/null 3<&- 9>&- &"
             (quote (started_tag ^ " " ^ nonce));
           line "child=$!";
           (* The watchdog. On stdin EOF, unless the wrapper has marked the
              run done, TERM the group, wait up to 3 s for it to empty
              (polling, so the lock is released as soon as it has), and
              KILL whatever is left. It keeps fd 9, so the lock is held
              until the group is gone.

              The wrapper disarms it with the done-file rather than by
              killing it: the shell may reap the watchdog early, and a
              later [kill $pid] could then hit an unrelated process that
              reused the pid. Disarmed or not, the watchdog ends at stdin
              EOF, which comes as soon as sshd closes the channel. After a
              stop it waits (up to 3 s) for the wrapper ($$ in this
              subshell) to exit, since the wrapper writes the done-file on
              its way out, and then removes it, so stops leave nothing
              behind; the sweep above catches any that outlive that.

              dash's kill only signals a group in the numeric form
              ([kill -15 -PGID], [kill -0 -PGID]). The KILL can in
              principle hit a reused pgid if the group emptied and its id
              was reallocated between the last poll and the kill. *)
           line "( cat <&3 >/dev/null 2>&1 9>&-; \
                 if [ -e \"$donefile\" ]; then rm -f \"$donefile\"; exit 0; fi; \
                 kill -15 \"-$child\" 2>/dev/null; i=0; \
                 while [ \"$i\" -lt 30 ] && kill -0 \"-$child\" 2>/dev/null; do \
                   sleep 0.1; i=$((i + 1)); done; \
                 if kill -0 \"-$child\" 2>/dev/null; then kill -9 \"-$child\" 2>/dev/null; fi; \
                 i=0; while [ \"$i\" -lt 30 ] && kill -0 \"$$\" 2>/dev/null; do \
                   sleep 0.1; i=$((i + 1)); done; \
                 rm -f \"$donefile\" \
                 ) </dev/null >/dev/null 2>&1 &";
           line "exec 3<&-";
           line "wait \"$child\"";
           line "rc=$?";
           (* If the disarm cannot be written (a full disk), the run's
              result stands, but say so: the watchdog will treat channel
              close as a stop and end detached processes from this turn. *)
           line "touch \"$donefile\" 2>/dev/null || echo \"discord-agents: could not \
                 disarm the watchdog; detached processes from this turn will be \
                 stopped when the channel closes\" >&2";
           line "printf '\\n%%s %%s\\n' %s \"$rc\"" (quote (exit_tag ^ " " ^ nonce));
           line "exit 0");
        Ok (Buffer.contents b)
      end

(** The single stdin line that carries [script]. *)
let stdin_line script = Base64.encode_string script ^ "\n"

type marker =
  | Started of int
  | Exit of int
  | Remote_error of string
  | Not_marker

let parse_marker ~nonce line =
  match String.split_on_char ' ' line with
  | [tag; n; value] when String.equal n nonce ->
    if String.equal tag started_tag then
      (match int_of_string_opt value with
       | Some p when p > 0 -> Started p
       | _ -> Not_marker)
    else if String.equal tag exit_tag then
      (match int_of_string_opt value with Some rc -> Exit rc | None -> Not_marker)
    else if String.equal tag error_tag then Remote_error value
    else Not_marker
  | _ -> Not_marker

(** A fresh nonce per call. Not a secret: it only has to be unknown to
    the command when its output is produced. A self-seeded [Random]
    state per call rather than [Resource.generate_uuid], whose crypto
    RNG must be initialised first; remote execution should not depend
    on that having happened. *)
let fresh_nonce () =
  let st = Random.State.make_self_init () in
  String.init 32 (fun _ -> "0123456789abcdef".[Random.State.int st 16])
