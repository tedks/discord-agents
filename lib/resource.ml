(** Safe resource management — prevents file descriptor leaks.
    Every file/lock operation goes through these helpers. *)

let with_file_in path f =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)

let with_file_out path f =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)

(** Environment variable lookup that treats empty values as absent. *)
let getenv_nonempty name =
  match Sys.getenv_opt name with
  | Some "" | None -> None
  | Some value -> Some value

(** Legacy pre-XDG config directory. *)
let legacy_home_config_dir () =
  match getenv_nonempty "HOME" with
  | Some home -> Some (Filename.concat home ".config/discord-agents")
  | None -> None

let config_dir_has_state dir =
  Sys.file_exists dir
  && List.exists
       (fun name -> Sys.file_exists (Filename.concat dir name))
       ["config.json"; "settings.json"; "sessions.json"; "control.sock"]

(** Application config directory, preferring XDG for new installs while
    preserving pre-XDG HOME-based installs. If XDG_CONFIG_HOME appears
    later but its app directory has no bot state yet, use the legacy
    directory when it already exists so settings, sessions, and the
    control socket remain discoverable. *)
let app_config_dir () =
  match getenv_nonempty "XDG_CONFIG_HOME" with
  | Some xdg_config_home ->
    let xdg_dir = Filename.concat xdg_config_home "discord-agents" in
    (match legacy_home_config_dir () with
     | Some legacy_dir
       when (not (config_dir_has_state xdg_dir)) && Sys.file_exists legacy_dir ->
       legacy_dir
     | _ -> xdg_dir)
  | None ->
    match legacy_home_config_dir () with
    | Some legacy_dir -> legacy_dir
    | None ->
      let fallback =
        Filename.concat (Filename.get_temp_dir_name ())
          (Printf.sprintf "discord-agents-%d" (Unix.getuid ()))
      in
      Logs.warn (fun m ->
        m "resource: HOME/XDG_CONFIG_HOME unset; using %s" fallback);
      fallback

(** Ensure the parent directory of [path] exists. *)
let ensure_parent_dir path =
  let rec mkdir_p dir =
    if Sys.file_exists dir then begin
      if not (Sys.is_directory dir) then
        failwith (Printf.sprintf "parent is not a directory: %s" dir)
    end else begin
      let parent = Filename.dirname dir in
      if parent <> dir then mkdir_p parent;
      try Unix.mkdir dir 0o700 with
      | Unix.Unix_error ((Unix.EEXIST | Unix.EISDIR), _, _) ->
        if not (Sys.is_directory dir) then
          failwith (Printf.sprintf "parent is not a directory: %s" dir)
      | exn -> raise exn
    end
  in
  mkdir_p (Filename.dirname path)

(** Read entire file contents safely. *)
let read_file path =
  with_file_in path (fun ic ->
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    Bytes.to_string s)

let file_mtime_opt path =
  try Some (Unix.stat path).Unix.st_mtime
  with _ -> None

let next_write_epoch paths =
  let baseline = Unix.gettimeofday () in
  let latest =
    List.fold_left (fun acc path ->
      match file_mtime_opt path with
      | Some mtime -> max acc mtime
      | None -> acc) baseline paths
  in
  latest +. 1.0

let stamp_file_mtime path mtime =
  let atime =
    match Unix.stat path with
    | stat -> stat.Unix.st_atime
    | exception _ -> mtime
  in
  Unix.utimes path atime mtime

exception Durable_write_visible_but_unconfirmed of string * exn

(** Retry fsync across EINTR so signals do not turn a completed write
    into a spurious failure. *)
let rec fsync fd =
  try Unix.fsync fd with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
    fsync fd

(** Best-effort fsync of a parent directory after a rename. *)
let fsync_dir path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    fsync fd)

(** Reap stale temp files from prior crashed writers.
    Callers must already hold the per-file flock before using this. *)
let cleanup_atomic_write_temps path =
  let dir = Filename.dirname path in
  let prefix = Filename.basename path ^ ".tmp." in
  match Sys.readdir dir with
  | exception Sys_error _ -> ()
  | entries ->
    Array.iter (fun name ->
      if String.starts_with ~prefix name then
        let temp_path = Filename.concat dir name in
        match Unix.lstat temp_path with
        | { Unix.st_kind = S_REG; _ } ->
          (try Unix.unlink temp_path with
           | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
           | exn ->
             Logs.warn (fun m ->
               m "resource: failed to remove stale temp file %s: %s"
                 temp_path (Printexc.to_string exn)))
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ())
      entries

let rec write_once fd content offset length =
  try Unix.single_write_substring fd content offset length with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
    write_once fd content offset length

let rec write_all fd content offset length =
  if length > 0 then
    let wrote = write_once fd content offset length in
    if wrote <= 0 then
      failwith "resource: short write"
    else
      write_all fd content (offset + wrote) (length - wrote)

(** Write string to file atomically and durably:
    - write to a temp file in the same directory
    - fsync the temp file
    - rename into place
    - fsync the parent directory so the rename is durable *)
let write_file_atomic ?(fsync_parent=fsync_dir) path content =
  ensure_parent_dir path;
  let dir = Filename.dirname path in
  let tmp =
    Filename.temp_file
      ~temp_dir:dir
      (Filename.basename path ^ ".tmp.")
      ""
  in
  let renamed = ref false in
  try
    let fd = Unix.openfile tmp
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      write_all fd content 0 (String.length content);
      write_all fd "\n" 0 1;
      fsync fd);
    Unix.rename tmp path;
    renamed := true;
    (try
       fsync_parent dir
     with exn ->
       raise (Durable_write_visible_but_unconfirmed
         (path, exn)))
  with exn ->
    if not !renamed then
      (try Sys.remove tmp with _ -> ());
    raise exn

let in_process_flock_table_mu = Mutex.create ()
let in_process_flock_table : (string, Mutex.t) Hashtbl.t = Hashtbl.create 16

let in_process_flock_mutex lock_path =
  Mutex.lock in_process_flock_table_mu;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock in_process_flock_table_mu)
    (fun () ->
       match Hashtbl.find_opt in_process_flock_table lock_path with
       | Some mu -> mu
       | None ->
         let mu = Mutex.create () in
         Hashtbl.add in_process_flock_table lock_path mu;
         mu)

(** Execute f while holding an exclusive flock on lock_path.
    Used for cross-process synchronization (bot + MCP server).

    POSIX record locks are per-process, so the OS lock alone does not
    serialize two fibers in this bot process. The in-process mutex closes
    that gap, while [lockf] still coordinates with the MCP helper process.
    Callers must keep [f] synchronous: doing Eio I/O while holding this
    stdlib mutex can deadlock the scheduler if another fiber contends on
    the same lock path. The table is intentionally never pruned; current
    callers use a bounded set of config/session lock paths. *)
let with_flock lock_path f =
  let in_process_mu = in_process_flock_mutex lock_path in
  Mutex.lock in_process_mu;
  Fun.protect ~finally:(fun () ->
    Mutex.unlock in_process_mu
  ) (fun () ->
    ensure_parent_dir lock_path;
    let fd = Unix.openfile lock_path [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
    Fun.protect ~finally:(fun () ->
      (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
      Unix.close fd
    ) (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      f ()))

(** Generate a random hex string of the given byte length using mirage-crypto-rng. *)
let random_hex n =
  let raw = Mirage_crypto_rng.generate n in
  let hex = Buffer.create (n * 2) in
  String.iter (fun c ->
    Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c))
  ) raw;
  Buffer.contents hex

(** Generate a UUID v4. *)
let generate_uuid () =
  let s = random_hex 16 in
  Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub s 0 8) (String.sub s 8 4) (String.sub s 12 4)
    (String.sub s 16 4) (String.sub s 20 12)

(** First 8 hex chars of a session id, the convention every Discord
    listing and resume reply uses. Safe on shorter inputs. *)
let short_id sid = String.sub sid 0 (min 8 (String.length sid))

(** Replace each \n / \r / \t in [s] with a single space. The
    replacement is 1:1 (a run of three newlines becomes three
    spaces, not one) — visually equivalent in Discord and simpler
    to reason about than a collapsing version.

    Used as defense-in-depth at every render boundary that emits
    user-controlled strings into Discord markdown, so a literal
    newline in (e.g.) a project name or session summary doesn't
    let the rest of the entry land at column 0 — Discord parses
    that as a sibling top-level bullet. *)
let single_line s =
  String.map (function
    | '\n' | '\r' | '\t' -> ' '
    | c -> c) s

(** Replace any invalid UTF-8 byte sequence in [s] with U+FFFD
    (the Unicode replacement character, 3 bytes: 0xEF 0xBF 0xBD).
    Valid UTF-8 input is returned unchanged.

    Discord's create_message endpoint rejects any request body whose
    JSON contains raw invalid-UTF-8 bytes with HTTP 400 / error
    code 50109 ("The request body contains invalid JSON"). Yojson
    happily encodes raw bytes verbatim, so anything in the agent's
    output buffer that isn't valid UTF-8 — typically lone surrogate
    halves (0xED 0xA0..0xBF / 0xED 0xB0..0xBF) decoded from
    \\uXXXX escapes in the agent's stream-json, or raw bytes from
    files Claude reads — gets silently dropped on the floor:
    [agent_runner.send] just logs the warning and moves on, so the
    user sees a missing chunk in mid-turn ("messages getting cut
    off"). Sanitizing at the send boundary makes that class of bug
    impossible regardless of where the bad bytes came from.

    Strict per RFC 3629: rejects overlong encodings and surrogates,
    so the output is what JSON requires. Each replacement is 3 bytes
    (longer than the bytes it replaced, in the worst case 3×); the
    Discord_rest splitter handles any post-sanitization length growth
    that crosses the 2000-char message limit. *)
let sanitize_utf8 s =
  let n = String.length s in
  let buf = Buffer.create n in
  let replacement = "\xEF\xBF\xBD" in
  let is_cont b = b land 0xC0 = 0x80 in
  let i = ref 0 in
  while !i < n do
    let c = Char.code s.[!i] in
    let valid_len =
      if c < 0x80 then Some 1
      else if c < 0xC2 then None  (* lone continuation, or overlong lead *)
      else if c < 0xE0 then begin
        if !i + 1 < n && is_cont (Char.code s.[!i + 1]) then Some 2
        else None
      end else if c < 0xF0 then begin
        if !i + 2 < n
        && is_cont (Char.code s.[!i + 1])
        && is_cont (Char.code s.[!i + 2]) then begin
          let cp = ((c land 0x0F) lsl 12)
                lor ((Char.code s.[!i + 1] land 0x3F) lsl 6)
                lor (Char.code s.[!i + 2] land 0x3F) in
          if cp < 0x800 then None              (* overlong *)
          else if cp >= 0xD800 && cp <= 0xDFFF then None  (* surrogate *)
          else Some 3
        end else None
      end else if c < 0xF5 then begin
        if !i + 3 < n
        && is_cont (Char.code s.[!i + 1])
        && is_cont (Char.code s.[!i + 2])
        && is_cont (Char.code s.[!i + 3]) then begin
          let cp = ((c land 0x07) lsl 18)
                lor ((Char.code s.[!i + 1] land 0x3F) lsl 12)
                lor ((Char.code s.[!i + 2] land 0x3F) lsl 6)
                lor (Char.code s.[!i + 3] land 0x3F) in
          if cp < 0x10000 then None            (* overlong *)
          else if cp > 0x10FFFF then None      (* beyond Unicode *)
          else Some 4
        end else None
      end else None  (* 0xF5..0xFF: not a valid lead byte *)
    in
    match valid_len with
    | Some k -> Buffer.add_substring buf s !i k; i := !i + k
    | None -> Buffer.add_string buf replacement; incr i
  done;
  Buffer.contents buf

(** Truncate [s] to at most [max_bytes] bytes, dropping any
    incomplete multi-byte sequence at the cut so the output never
    contains a half-encoded character introduced *by truncation*.
    Whitespace is preserved (no [single_line] collapse) — use
    [normalize_summary] when you also want bullet-leak defense.

    [max_bytes] is clamped to 0 if negative, so this is safe to call
    with attacker-supplied or arithmetic-derived bounds.

    Two-step boundary walk:
    1. Walk back from [max_bytes] over continuation bytes (0x80..0xBF)
       so we don't cut mid-codepoint when the cut byte itself is part
       of a still-in-progress sequence.
    2. Identify the last would-be-included codepoint by walking back
       from there over continuations to its lead byte. If the lead's
       declared length doesn't fit within the truncated prefix
       (e.g. cap=2 over [\\xE2\\x80a]: lead \\xE2 declares 3 bytes,
       only 2 fit), drop that codepoint too.

    Validity guarantee — for VALID UTF-8 input, the output is valid
    UTF-8 (no half-encoded characters introduced at the cut). For
    input that's already invalid (lone continuations, lone leaders
    away from the cut, overlongs, surrogates), those bytes survive
    into the output unchanged — this function refuses to introduce
    new invalidity but cannot repair pre-existing invalidity. Pair
    with [sanitize_utf8] (which the Discord send path applies
    automatically, and which [normalize_summary] now applies for
    session-listing callers) for strict validity at the boundary.

    Termination: each loop decrements a non-negative counter or
    bails on a non-continuation byte, so always halts. *)
let truncate_utf8 ~max_bytes s =
  let max_bytes = max 0 max_bytes in
  let n = String.length s in
  if n <= max_bytes then s
  else
    let is_cont c = c >= 0x80 && c < 0xC0 in
    (* Step 1: walk back from max_bytes over continuation bytes. *)
    let p = ref max_bytes in
    while !p > 0 && !p < n && is_cont (Char.code s.[!p]) do
      decr p
    done;
    (* Step 2: find the start of the last codepoint in [0..!p) and
       check whether its declared length fits. If not, drop it. *)
    if !p > 0 then begin
      let lead_pos = ref (!p - 1) in
      while !lead_pos > 0 && is_cont (Char.code s.[!lead_pos]) do
        decr lead_pos
      done;
      let lead = Char.code s.[!lead_pos] in
      let expected =
        if lead < 0x80 then 1
        else if lead < 0xC0 then 1   (* lone continuation; treat as 1 *)
        else if lead < 0xE0 then 2
        else if lead < 0xF0 then 3
        else 4
      in
      if !p - !lead_pos < expected then
        p := !lead_pos
    end;
    String.sub s 0 !p

(** Normalize a session-summary string for any downstream renderer:
    [single_line] it (multi-paragraph user prompts would otherwise
    leak as sibling top-level bullets when Discord renders them
    inside a markdown list), then truncate to [max_bytes] on a
    UTF-8 codepoint boundary so we never emit a half-encoded
    character.

    Used at the source by every session discoverer
    (claude_sessions, codex_sessions, gemini_sessions) so the
    [info.summary] field is safe regardless of which renderer
    consumes it (Bot.format_session_listing, MCP server,
    control_api JSON, etc).

    Do NOT use this for free-form text where structure matters
    (code blocks, bullets, paragraph breaks) — the [single_line]
    pass destroys it. See [truncate_utf8] for that case. *)
let normalize_summary ~max_bytes s =
  (* sanitize_utf8 first: session files are JSON (so should be valid
     UTF-8 by yojson decode), but yojson tolerates raw bytes in JSON
     strings, so a session file with malformed bytes can leak invalid
     UTF-8 into a summary, then into the JSON the control_api
     serializes for MCP, where the Python proxy decodes
     [data.decode()] strictly and would raise UnicodeDecodeError.
     Sanitizing here makes the session-listing boundary safe. *)
  truncate_utf8 ~max_bytes (sanitize_utf8 (single_line s))
