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

    Every word interpolated into a script is single-quoted, so the
    only shell syntax in a script is what this module writes. *)

let bootstrap =
  {|sh -c 'IFS= read -r l; eval "$(printf %s "$l" | base64 -d)"'|}

type mode =
  | Capture
      (** Run to completion; stdout and stderr merged, as
          [Project.run_capture] does locally. *)
  | Run of { lock_key : string }
      (** Long-running agent turn: own process group, stdin-EOF
          watchdog, and a per-[lock_key] flock. *)

let is_env_name s =
  s <> ""
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

let lock_dir = {|"$HOME/.cache/discord-agents/run"|}

let script ~nonce ~mode ~cwd ~env ~argv =
  if argv = [] then Error "remote command must not be empty"
  else if List.exists has_nul (cwd :: argv) then
    Error "remote command and directory must not contain NUL bytes"
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
        let err code =
          Printf.sprintf "printf '%%s\\n' %s"
            (quote (marker error_tag nonce code))
        in
        line "set -u";
        List.iter (fun (k, v) -> line "export %s=%s" k (quote v)) env;
        line "set -- %s" (String.concat " " (List.map quote argv));
        line "cd %s 2>/dev/null || { %s; exit 1; }" (quote cwd)
          (err "missing_cwd");
        (match mode with
         | Capture ->
           line "printf '%%s\\n' %s" (quote (marker started_tag nonce "0"));
           line "\"$@\" 2>&1";
           line "rc=$?";
           (* Leading newline: the command's output need not end in one,
              and the caller strips exactly this byte back off. *)
           line "printf '\\n%%s %%s\\n' %s \"$rc\""
             (quote (exit_tag ^ " " ^ nonce));
           line "exit 0"
         | Run { lock_key } ->
           (* fd 3 keeps the SSH channel's stdin for the watchdog; the
              command itself gets /dev/null and neither fd 3 nor the
              lock fd 9, so it cannot hold the lock or the channel
              open after the wrapper is gone. *)
           line "exec 3<&0";
           line "mkdir -p %s 2>/dev/null || { %s; exit 1; }" lock_dir
             (err "lock_failed");
           line "exec 9>%s/%s.lock || { %s; exit 1; }" lock_dir
             (quote lock_key) (err "lock_failed");
           line "flock -n 9 || { %s; exit 1; }" (err "busy");
           line "setsid --wait \"$@\" </dev/null 3<&- 9>&- &";
           line "child=$!";
           line "printf '%%s %%s\\n' %s \"$child\""
             (quote (started_tag ^ " " ^ nonce));
           (* dash's builtin kill only signals a process group with the
              numeric form: [kill -s TERM -PGID] and [kill -TERM -- -PGID]
              are both rejected. *)
           line "( cat <&3 >/dev/null 2>&1; kill -15 \"-$child\" 2>/dev/null; \
                 sleep 3; kill -9 \"-$child\" 2>/dev/null ) \
                 </dev/null >/dev/null 2>&1 9>&- &";
           line "dog=$!";
           line "exec 3<&-";
           line "wait \"$child\"";
           line "rc=$?";
           line "kill \"$dog\" 2>/dev/null";
           (* Leading newline, as in Capture: the command's last line may
              lack one, and the marker must start its own line. The
              reader drops the empty line this creates otherwise. *)
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
      (match int_of_string_opt value with Some p -> Started p | None -> Not_marker)
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
