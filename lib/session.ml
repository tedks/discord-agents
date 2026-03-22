(** Agent session — the bridge between a Discord thread and an agent subprocess.

    Session lifecycle is now managed by Bot directly using
    Agent_process.run_streaming. This module provides the state types
    and ID generation. *)

type state =
  | Starting
  | Active
  | Stopping
  | Stopped
[@@deriving show]

let generate_id () =
  let raw = Mirage_crypto_rng.generate 8 in
  let hex = Buffer.create 16 in
  String.iter (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c))) raw;
  Buffer.contents hex
