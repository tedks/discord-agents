(** Type-safe ID wrappers. Prevents mixing up thread IDs, channel IDs, etc.
    Each ID type is a distinct abstract type backed by string. *)

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make () : ID = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let compare = String.compare
  let equal = String.equal
  let pp fmt s = Format.pp_print_string fmt s
end

module Thread_id = Make ()
module Channel_id = Make ()
module Session_id = Make ()
module Guild_id = Make ()

(** Channel names are always lowercased by Discord.
    This type normalizes on construction. *)
module Channel_name : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
end = struct
  type t = string
  let of_string s = String.lowercase_ascii s
  let to_string s = s
  let compare = String.compare
  let equal = String.equal
end
