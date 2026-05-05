(** Safe resource management — prevents file descriptor leaks.
    Every file/lock operation goes through these helpers. *)

let with_file_in path f =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)

let with_file_out path f =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)

(** Read entire file contents safely. *)
let read_file path =
  with_file_in path (fun ic ->
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    Bytes.to_string s)

(** Write string to file atomically (temp + rename). *)
let write_file_atomic path content =
  let tmp = path ^ ".tmp" in
  (try
    with_file_out tmp (fun oc ->
      output_string oc content;
      output_char oc '\n');
    Unix.rename tmp path
  with exn ->
    (try Sys.remove tmp with _ -> ());
    raise exn)

(** Execute f while holding an exclusive flock on lock_path.
    Used for cross-process synchronization (bot + MCP server). *)
let with_flock lock_path f =
  let fd = Unix.openfile lock_path [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
  Fun.protect ~finally:(fun () ->
    (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
    Unix.close fd
  ) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    f ())

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

(** Truncate [s] to at most [max_bytes] bytes, walking back to the
    nearest UTF-8 codepoint boundary so we never emit a half-encoded
    character. Whitespace is preserved (no [single_line] collapse) —
    use [normalize_summary] when you also want bullet-leak defense.

    The walk-back is bounded by [max_bytes] and by remaining length,
    so termination is guaranteed regardless of input validity. *)
let truncate_utf8 ~max_bytes s =
  let n = String.length s in
  if n <= max_bytes then s
  else
    let p = ref max_bytes in
    while !p > 0 && !p < n
      && (let c = Char.code s.[!p] in c >= 0x80 && c < 0xC0) do
      decr p
    done;
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
  truncate_utf8 ~max_bytes (single_line s)
