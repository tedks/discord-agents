(** Discord API types — minimal subset for text bot operations.

    We only model what we need: messages, channels, threads, reactions.
    No voice, no presence, no guild member management. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Channel types per Discord API *)
type channel_type =
  | Guild_text        (* 0 *)
  | DM                (* 1 *)
  | Guild_category    (* 4 *)
  | Guild_public_thread  (* 11 *)
  | Guild_private_thread (* 12 *)
[@@deriving show, eq]

let int_of_channel_type = function
  | Guild_text -> 0
  | DM -> 1
  | Guild_category -> 4
  | Guild_public_thread -> 11
  | Guild_private_thread -> 12

let channel_type_of_int = function
  | 0 -> Guild_text
  | 1 -> DM
  | 4 -> Guild_category
  | 11 -> Guild_public_thread
  | 12 -> Guild_private_thread
  | n -> failwith (Printf.sprintf "unknown channel type: %d" n)

let channel_type_of_yojson = function
  | `Int n -> channel_type_of_int n
  | _ -> failwith "channel_type: expected int"

let yojson_of_channel_type ct = `Int (int_of_channel_type ct)

type snowflake = string [@@deriving show, eq, yojson]

type user = {
  id : snowflake;
  username : string;
  bot : bool option; [@yojson.option]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type channel = {
  id : snowflake;
  type_ : channel_type; [@key "type"]
  guild_id : snowflake option; [@yojson.option]
  name : string option; [@yojson.option]
  topic : string option; [@yojson.option]
  parent_id : snowflake option; [@yojson.option]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type attachment = {
  id : snowflake;
  filename : string;
  size : int;
  url : string;
  content_type : string option; [@yojson.option]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type message = {
  id : snowflake;
  channel_id : snowflake;
  author : user;
  content : string;
  timestamp : string;
  guild_id : snowflake option; [@yojson.option]
  attachments : attachment list; [@default []]
  referenced_message : message option; [@yojson.option]
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

type guild = {
  id : snowflake;
  name : string;
} [@@deriving show, yojson] [@@yojson.allow_extra_fields]

(** Gateway event types we care about *)
type gateway_dispatch =
  | Ready of { user : user; guilds : Yojson.Safe.t list }
  | Message_create of message
  | Thread_create of channel
  | Guild_create of guild
  | Unknown of string * Yojson.Safe.t

(** Gateway opcodes *)
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

(** Intent flags for gateway identify *)
module Intent = struct
  let guilds = 1 lsl 0
  let guild_messages = 1 lsl 9
  let guild_message_reactions = 1 lsl 10
  let direct_messages = 1 lsl 12
  let message_content = 1 lsl 15
end
