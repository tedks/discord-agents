(** Discord Gateway (WebSocket) client.

    Handles the connection lifecycle:
    1. Connect to wss://gateway.discord.gg/?v=10&encoding=json
    2. Receive Hello, start heartbeating
    3. Send Identify with token + intents
    4. Dispatch events to handler callback

    Reconnection and resume are TODO. *)

open Discord_types

type event =
  | Connected of user
  | Message_received of message
  | Thread_created of channel
  | Disconnected of string

type handler = event -> unit

type t = {
  token : string;
  intents : int;
  mutable sequence : int option;
  mutable session_id : string option;
  mutable handler : handler;
}

let create ~token ~intents ~handler =
  { token; intents; sequence = None; session_id = None; handler }

let default_intents =
  Intent.guilds
  lor Intent.guild_messages
  lor Intent.guild_message_reactions
  lor Intent.direct_messages
  lor Intent.message_content

(** Parse a gateway payload and dispatch events. *)
let handle_payload t json =
  let open Yojson.Safe.Util in
  let op = json |> member "op" |> to_int |> gateway_opcode_of_int in
  let d = json |> member "d" in
  let s = json |> member "s" |> to_int_option in
  (* Update sequence for heartbeats *)
  (match s with Some _ -> t.sequence <- s | None -> ());
  match op with
  | Dispatch ->
    let event_name = json |> member "t" |> to_string in
    (match event_name with
     | "READY" ->
       let user = d |> member "user" |> user_of_yojson in
       t.session_id <- Some (d |> member "session_id" |> to_string);
       t.handler (Connected user)
     | "MESSAGE_CREATE" ->
       let msg = message_of_yojson d in
       t.handler (Message_received msg)
     | "THREAD_CREATE" ->
       let ch = channel_of_yojson d in
       t.handler (Thread_created ch)
     | _ ->
       Logs.debug (fun m -> m "gateway: unhandled event %s" event_name))
  | Hello ->
    let _heartbeat_interval = d |> member "heartbeat_interval" |> to_int in
    (* TODO: start heartbeat fiber, send Identify *)
    Logs.info (fun m -> m "gateway: received Hello")
  | Heartbeat_ack ->
    Logs.debug (fun m -> m "gateway: heartbeat ack")
  | Reconnect ->
    Logs.warn (fun m -> m "gateway: server requested reconnect");
    t.handler (Disconnected "reconnect requested")
  | Invalid_session ->
    Logs.warn (fun m -> m "gateway: invalid session");
    t.handler (Disconnected "invalid session")
  | _ ->
    Logs.debug (fun m -> m "gateway: unhandled opcode")

(** Build the Identify payload. *)
let identify_payload t =
  `Assoc [
    ("op", `Int 2);
    ("d", `Assoc [
      ("token", `String t.token);
      ("intents", `Int t.intents);
      ("properties", `Assoc [
        ("os", `String "linux");
        ("browser", `String "discord-agents");
        ("device", `String "discord-agents");
      ]);
    ]);
  ]

(** Build a heartbeat payload. *)
let heartbeat_payload t =
  `Assoc [
    ("op", `Int 1);
    ("d", match t.sequence with Some s -> `Int s | None -> `Null);
  ]

(** Connect to the gateway and run the event loop.
    This is a placeholder — actual WebSocket integration is TODO. *)
let connect _t =
  (* TODO: implement with websocket + tls-eio under Eio *)
  Logs.info (fun m -> m "gateway: connect not yet implemented");
  ignore (identify_payload _t);
  ignore (heartbeat_payload _t);
  ignore (handle_payload _t (`Null));
  ()
