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
  let buf = Bytes.create 8 in
  let ic = open_in "/dev/urandom" in
  really_input ic buf 0 8;
  close_in ic;
  let hex = Buffer.create 16 in
  Bytes.iter (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c))) buf;
  Buffer.contents hex
