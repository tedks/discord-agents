(** Discord REST API client — minimal surface for bot operations.

    Assumptions:
    - Bot token auth only
    - No file uploads yet (will add for agent output sharing)
    - Rate limiting is TODO — Discord returns 429 with Retry-After header *)

open Discord_types

let api_base = "https://discord.com/api/v10"

type t = {
  token : string;
}

let create ~token = { token }

let headers t = [
  ("Authorization", "Bot " ^ t.token);
  ("Content-Type", "application/json");
  ("User-Agent", "DiscordBot (discord-agents/0.1.0, OCaml)");
]

(** Low-level HTTP request. Returns parsed JSON or error. *)
let request t ~meth ~path ?body () =
  ignore (t, meth, path, body);
  (* TODO: implement with cohttp-eio *)
  Error "REST client not yet implemented"

(** Send a message to a channel. *)
let create_message t ~channel_id ~content ?reply_to () =
  let body = `Assoc ([
    ("content", `String content);
  ] @ match reply_to with
    | Some msg_id -> [("message_reference", `Assoc [("message_id", `String msg_id)])]
    | None -> [])
  in
  ignore (request t ~meth:`POST ~path:(Printf.sprintf "/channels/%s/messages" channel_id) ~body ());
  (* Placeholder until REST is wired up *)
  Ok { id = "0"; channel_id; author = { id = "0"; username = "bot"; bot = Some true };
       content; timestamp = ""; guild_id = None; attachments = [];
       referenced_message = None }

(** Create a new text channel in a guild. *)
let create_channel t ~guild_id ~name ?parent_id ?topic () =
  let body = `Assoc ([
    ("name", `String name);
    ("type", `Int 0); (* GUILD_TEXT *)
  ] @ (match parent_id with Some id -> [("parent_id", `String id)] | None -> [])
    @ (match topic with Some t -> [("topic", `String t)] | None -> []))
  in
  ignore (request t ~meth:`POST ~path:(Printf.sprintf "/guilds/%s/channels" guild_id) ~body ());
  Error "not yet implemented"

(** Create a thread from a message. *)
let create_thread t ~channel_id ~message_id ~name () =
  let body = `Assoc [
    ("name", `String name);
    ("auto_archive_duration", `Int 1440); (* 24 hours *)
  ] in
  ignore (request t ~meth:`POST
    ~path:(Printf.sprintf "/channels/%s/messages/%s/threads" channel_id message_id)
    ~body ());
  Error "not yet implemented"

(** Create a thread without a starter message. *)
let create_thread_no_message t ~channel_id ~name () =
  let body = `Assoc [
    ("name", `String name);
    ("type", `Int 11); (* PUBLIC_THREAD *)
    ("auto_archive_duration", `Int 1440);
  ] in
  ignore (request t ~meth:`POST
    ~path:(Printf.sprintf "/channels/%s/threads" channel_id)
    ~body ());
  Error "not yet implemented"

(** Fetch messages from a channel. *)
let get_messages t ~channel_id ?(limit=20) () =
  ignore (request t ~meth:`GET
    ~path:(Printf.sprintf "/channels/%s/messages?limit=%d" channel_id limit) ());
  Error "not yet implemented"

(** Add a reaction to a message. *)
let create_reaction t ~channel_id ~message_id ~emoji () =
  let encoded_emoji = Uri.pct_encode emoji in
  ignore (request t ~meth:`PUT
    ~path:(Printf.sprintf "/channels/%s/messages/%s/reactions/%s/@me"
      channel_id message_id encoded_emoji) ());
  Ok ()
