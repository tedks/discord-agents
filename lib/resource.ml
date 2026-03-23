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
