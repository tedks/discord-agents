(** Discord API types — minimal subset for text bot operations.

    We only model what we need: messages, channels, threads, reactions.
    No voice, no presence, no guild member management.

    ID types: Discord uses "snowflake" IDs (64-bit integers as strings).
    We define distinct types for each ID kind so the compiler prevents
    mixing up channel_id with message_id, etc. They're all strings at
    runtime but distinct types at compile time. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** {1 ID types}
    Each is a distinct type alias with its own deriving.
    They share the same runtime representation (string) but are
    not interchangeable at compile time thanks to the .mli. *)

type channel_id = string [@@deriving show, eq, yojson]
type message_id = string [@@deriving show, eq, yojson]
type guild_id = string [@@deriving show, eq, yojson]
type user_id = string [@@deriving show, eq, yojson]
type attachment_id = string [@@deriving show, eq, yojson]

(** {1 Channel types} *)

type channel_type =
  | Guild_text        (* 0 *)
  | DM                (* 1 *)
  | Guild_voice       (* 2 *)
  | Guild_category    (* 4 *)
  | Guild_announcement (* 5 *)
  | Guild_stage       (* 13 *)
  | Guild_forum       (* 15 *)
  | Guild_public_thread  (* 11 *)
  | Guild_private_thread (* 12 *)
  | Unknown_channel_type of int
[@@deriving show, eq]

let int_of_channel_type = function
  | Guild_text -> 0
  | DM -> 1
  | Guild_voice -> 2
  | Guild_category -> 4
  | Guild_announcement -> 5
  | Guild_public_thread -> 11
  | Guild_private_thread -> 12
  | Guild_stage -> 13
  | Guild_forum -> 15
  | Unknown_channel_type n -> n

let channel_type_of_int = function
  | 0 -> Guild_text
  | 1 -> DM
  | 2 -> Guild_voice
  | 4 -> Guild_category
  | 5 -> Guild_announcement
  | 11 -> Guild_public_thread
  | 12 -> Guild_private_thread
  | 13 -> Guild_stage
  | 15 -> Guild_forum
  | n -> Unknown_channel_type n

let channel_type_of_yojson = function
  | `Int n -> channel_type_of_int n
  | _ -> failwith "channel_type: expected int"

let yojson_of_channel_type ct = `Int (int_of_channel_type ct)

(** {1 API object types} *)

type user = {
  id : user_id;
  username : string;
  bot : bool option; [@default None]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type channel = {
  id : channel_id;
  type_ : channel_type; [@key "type"]
  guild_id : guild_id option; [@default None]
  name : string option; [@default None]
  topic : string option; [@default None]
  parent_id : channel_id option; [@default None]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type attachment = {
  id : attachment_id;
  filename : string;
  size : int;
  url : string;
  content_type : string option; [@default None]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type message = {
  id : message_id;
  channel_id : channel_id;
  author : user;
  content : string;
  timestamp : string;
  guild_id : guild_id option; [@default None]
  attachments : attachment list; [@default []]
  referenced_message : message option; [@default None]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type guild = {
  id : guild_id;
  name : string;
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

(** {1 Gateway types} *)

type gateway_dispatch =
  | Ready of { user : user; guilds : Yojson.Safe.t list }
  | Message_create of message
  | Thread_create of channel
  | Guild_create of guild
  | Unknown of string * Yojson.Safe.t

type gateway_opcode =
  | Dispatch        (* 0 *)
  | Heartbeat       (* 1 *)
  | Identify        (* 2 *)
  | Resume          (* 6 *)
  | Reconnect       (* 7 *)
  | Invalid_session (* 9 *)
  | Hello           (* 10 *)
  | Heartbeat_ack   (* 11 *)
  | Unknown_op of int

let gateway_opcode_of_int = function
  | 0 -> Dispatch
  | 1 -> Heartbeat
  | 2 -> Identify
  | 6 -> Resume
  | 7 -> Reconnect
  | 9 -> Invalid_session
  | 10 -> Hello
  | 11 -> Heartbeat_ack
  | n -> Unknown_op n

module Intent = struct
  let guilds = 1 lsl 0
  let guild_messages = 1 lsl 9
  let guild_message_reactions = 1 lsl 10
  let direct_messages = 1 lsl 12
  let message_content = 1 lsl 15
end
