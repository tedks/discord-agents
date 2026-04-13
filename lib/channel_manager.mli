(** Discord channel management for project channels.

    [t] is abstract — callers cannot directly read or write the internal
    channel map or category_id. This enforces the project_state snapshot
    invariant: once a [t] is part of a snapshot, external code cannot
    mutate it behind the snapshot's back.

    Mutation happens only through the functions in this module. *)

type t

(** Create an empty channel manager with no mappings. *)
val create : unit -> t

(** The category ID, if set up. *)
val category_id : t -> string option

(** Set the category ID. Used during initial setup. *)
val set_category_id : t -> string option -> unit

(** Find a project's channel ID by project name. *)
val find : t -> project_name:string -> string option

(** Number of mapped channels. *)
val count : t -> int

(** All channel bindings as (project_name, channel_id) pairs. *)
val bindings : t -> (string * string) list

(** Set up the "Agent Projects" category and map existing Discord channels
    to projects (case-insensitive match on names). Mutates [t] in place.
    Calls Discord REST — must be called from an Eio fiber. *)
val setup :
  rest:Discord_rest.t ->
  guild_id:Discord_types.guild_id ->
  projects:Project.t list ->
  t -> unit

(** Find or create a channel for a project. Returns the channel ID.
    Checks Discord's actual channels first to avoid creating duplicates.
    Mutates [t] to cache new mappings. Must be called from an Eio fiber. *)
val find_or_create :
  rest:Discord_rest.t ->
  guild_id:Discord_types.guild_id ->
  project:Project.t ->
  t -> string option

(** Move a project's channel to the top of the category. *)
val bump :
  rest:Discord_rest.t ->
  guild_id:Discord_types.guild_id ->
  project_name:string ->
  t -> unit

(** Reorder project channels by activity. Takes a list of (project_name, score)
    pairs sorted most-active first. Uses a single batch API call. *)
val reorder_by_activity :
  rest:Discord_rest.t ->
  guild_id:Discord_types.guild_id ->
  t -> (string * int) list -> unit

(** Delete channels that don't match any current project name. *)
val cleanup :
  rest:Discord_rest.t ->
  guild_id:Discord_types.guild_id ->
  projects:Project.t list ->
  t -> (int, string) result

(** Check if a channel ID belongs to a project channel, return the project name. *)
val project_for_channel :
  t -> channel_id:Discord_types.channel_id -> string option
