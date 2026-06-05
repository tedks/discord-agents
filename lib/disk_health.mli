type mode =
  | Healthy
  | Warning
  | Read_only

type snapshot = {
  mode : mode;
  available_bytes : int64 option;
  warning_threshold_bytes : int64;
  read_only_threshold_bytes : int64;
  last_error : string option;
  checked_path : string option;
  checked_at : float option;
}

type write_failure = {
  code : string;
  summary : string;
}

val warning_threshold_bytes : int64
val read_only_threshold_bytes : int64
val snapshot : unit -> snapshot
val string_of_mode : mode -> string
val pressure : snapshot -> bool
val is_read_only : unit -> bool
val human_bytes : int64 -> string
val preflight_write : string -> (unit, string) result
val preflight_state_mutation : unit -> (unit, string) result
val note_write_failure : string -> exn -> unit
val note_write_success : string -> unit
val status_summary : unit -> string
val new_session_block_message :
  ?preflight:(unit -> (unit, string) result) -> unit -> string option

module For_testing : sig
  val reset : unit -> unit
  val mib : int -> int64
  val mode_of_available_bytes : int64 -> mode
  val update_from_available_bytes :
    ?force:bool -> path:string -> int64 -> (unit, string) result
  val classify_write_failure : exn -> write_failure option
  val note_write_failure : string -> exn -> unit
  val note_write_success : string -> unit
  val new_session_block_message :
    ?preflight:(unit -> (unit, string) result) -> unit -> string option
  val preflight_path_with :
    available_bytes_of_path:(string -> int64) ->
    string ->
    (unit, string) result
end
