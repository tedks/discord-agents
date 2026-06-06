(** Typed restart checkpoints for in-flight agent runs.

    The persisted format allows an active run to exist before a child
    process identity is captured, but cleanup operations require that
    identity. The phantom state keeps those cases distinct at call sites:
    callers create an untracked checkpoint first, and only [track_child]
    can promote it to a tracked checkpoint.

    Values recovered from disk are erased to [any] because persistence
    loses the static phantom-state distinction. Callers that need
    tracked-only operations must stay on the fresh [tracked t] path. *)

type child_process_identity = private {
  pid : int;
  start_ticks : int64;
}

type untracked
type tracked

type _ t

type any

val child_process_identity : pid:int -> start_ticks:int64 -> child_process_identity
val compare_child_process_identity :
  child_process_identity -> child_process_identity -> int

val create : message_id:Discord_types.message_id -> untracked t
val track_child : untracked t -> child_process_identity -> tracked t
val erase : _ t -> any
val of_persisted :
  message_id:Discord_types.message_id ->
  child_process:child_process_identity option ->
  any

val message_id : _ t -> Discord_types.message_id
val message_id_any : any -> Discord_types.message_id
val child_process : tracked t -> child_process_identity
val child_process_any : any -> child_process_identity option
val equal_any : any -> any -> bool
val equal_any_option : any option -> any option -> bool
