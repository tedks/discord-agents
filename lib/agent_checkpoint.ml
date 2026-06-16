type child_process_identity = {
  pid : int;
  start_ticks : int64;
}

type untracked
type tracked

type _ t =
  | Untracked : {
      message_id : Discord_types.message_id;
    } -> untracked t
  | Tracked : {
      message_id : Discord_types.message_id;
      child_process : child_process_identity;
    } -> tracked t

type any = Any : _ t -> any

let child_process_identity ~pid ~start_ticks = { pid; start_ticks }

let compare_child_process_identity a b =
  match Int.compare a.pid b.pid with
  | 0 -> Int64.compare a.start_ticks b.start_ticks
  | n -> n

let equal_child_process_identity a b =
  Int.equal a.pid b.pid && Int64.equal a.start_ticks b.start_ticks

let create ~message_id = Untracked { message_id }

let track_child : untracked t -> child_process_identity -> tracked t =
  fun checkpoint child_process ->
    match checkpoint with
    | Untracked { message_id } -> Tracked { message_id; child_process }

let erase checkpoint = Any checkpoint

let of_persisted ~message_id ~child_process =
  match child_process with
  | Some child_process ->
    Any (Tracked { message_id; child_process })
  | None ->
    Any (Untracked { message_id })

let message_id : type state. state t -> Discord_types.message_id = function
  | Untracked { message_id } -> message_id
  | Tracked { message_id; _ } -> message_id

let message_id_any (Any checkpoint) = message_id checkpoint

let child_process : tracked t -> child_process_identity = function
  | Tracked { child_process; _ } -> child_process

let child_process_any (Any checkpoint) =
  match checkpoint with
  | Untracked _ -> None
  | Tracked { child_process; _ } -> Some child_process

let equal_any a b =
  String.equal (message_id_any a) (message_id_any b)
  && Option.equal equal_child_process_identity
       (child_process_any a)
       (child_process_any b)

let equal_any_option a b = Option.equal equal_any a b
