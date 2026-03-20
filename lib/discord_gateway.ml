(** Discord Gateway (WebSocket) client.

    Handles the connection lifecycle:
    1. Connect to wss://gateway.discord.gg/?v=10&encoding=json
    2. Receive Hello, start heartbeating
    3. Send Identify with token + intents
    4. Dispatch events to handler callback
    5. Maintain heartbeat, handle reconnect/resume *)

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
  mutable ws : Websocket.t option;
  mutable sequence : int option;
  mutable session_id : string option;
  mutable resume_gateway_url : string option;
  mutable heartbeat_interval_ms : int;
  mutable last_heartbeat_acked : bool;
  mutable handler : handler;
}

let create ~token ~intents ~handler =
  { token; intents;
    ws = None;
    sequence = None;
    session_id = None;
    resume_gateway_url = None;
    heartbeat_interval_ms = 0;
    last_heartbeat_acked = true;
    handler }

let default_intents =
  Intent.guilds
  lor Intent.guild_messages
  lor Intent.guild_message_reactions
  lor Intent.direct_messages
  lor Intent.message_content

(** Send a JSON payload over the WebSocket. *)
let send_json t json =
  match t.ws with
  | Some ws -> Websocket.send_text ws (Yojson.Safe.to_string json)
  | None -> Logs.warn (fun m -> m "gateway: send_json but no ws connection")

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

(** Build a Resume payload. *)
let _resume_payload t =
  `Assoc [
    ("op", `Int 6);
    ("d", `Assoc [
      ("token", `String t.token);
      ("session_id", `String (Option.value ~default:"" t.session_id));
      ("seq", match t.sequence with Some s -> `Int s | None -> `Null);
    ]);
  ]

(** Parse a gateway payload and dispatch events. *)
let handle_payload t ~sw ~clock json =
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
       let resume_url = d |> member "resume_gateway_url" |> to_string_option in
       t.resume_gateway_url <- resume_url;
       Logs.info (fun m -> m "gateway: READY as %s (session %s)"
         user.username (Option.value ~default:"?" t.session_id));
       t.handler (Connected user)
     | "MESSAGE_CREATE" ->
       (try
          let msg = message_of_yojson d in
          t.handler (Message_received msg)
        with exn ->
          Logs.warn (fun m -> m "gateway: failed to parse MESSAGE_CREATE: %s"
            (Printexc.to_string exn)))
     | "THREAD_CREATE" ->
       (try
          let ch = channel_of_yojson d in
          t.handler (Thread_created ch)
        with exn ->
          Logs.warn (fun m -> m "gateway: failed to parse THREAD_CREATE: %s"
            (Printexc.to_string exn)))
     | _ ->
       Logs.debug (fun m -> m "gateway: unhandled event %s" event_name))
  | Hello ->
    let heartbeat_interval = d |> member "heartbeat_interval" |> to_int in
    t.heartbeat_interval_ms <- heartbeat_interval;
    t.last_heartbeat_acked <- true;
    Logs.info (fun m -> m "gateway: Hello, heartbeat interval %dms" heartbeat_interval);
    (* Start heartbeat daemon *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
      (* Initial jitter: random fraction of the interval *)
      let jitter = Random.float (float_of_int heartbeat_interval /. 1000.0) in
      Eio.Time.sleep clock jitter;
      let rec heartbeat_loop () =
        if not t.last_heartbeat_acked then begin
          Logs.warn (fun m -> m "gateway: heartbeat not acked, reconnecting");
          (match t.ws with Some ws -> Websocket.send_close ws | None -> ());
          `Stop_daemon
        end else begin
          t.last_heartbeat_acked <- false;
          send_json t (heartbeat_payload t);
          Logs.debug (fun m -> m "gateway: heartbeat sent (seq=%s)"
            (match t.sequence with Some s -> string_of_int s | None -> "null"));
          Eio.Time.sleep clock (float_of_int t.heartbeat_interval_ms /. 1000.0);
          heartbeat_loop ()
        end
      in
      heartbeat_loop ()
    );
    (* Send Identify *)
    Logs.info (fun m -> m "gateway: sending Identify");
    send_json t (identify_payload t)
  | Heartbeat ->
    (* Server requesting immediate heartbeat *)
    Logs.debug (fun m -> m "gateway: server requested heartbeat");
    send_json t (heartbeat_payload t)
  | Heartbeat_ack ->
    t.last_heartbeat_acked <- true;
    Logs.debug (fun m -> m "gateway: heartbeat ack")
  | Reconnect ->
    Logs.warn (fun m -> m "gateway: server requested reconnect");
    (match t.ws with Some ws -> Websocket.send_close ws | None -> ());
    t.handler (Disconnected "reconnect requested")
  | Invalid_session ->
    let resumable = try Yojson.Safe.Util.to_bool d with _ -> false in
    Logs.warn (fun m -> m "gateway: invalid session (resumable=%b)" resumable);
    if not resumable then begin
      t.session_id <- None;
      t.sequence <- None
    end;
    (match t.ws with Some ws -> Websocket.send_close ws | None -> ());
    t.handler (Disconnected "invalid session")
  | _ ->
    Logs.debug (fun m -> m "gateway: unhandled opcode")

(** Connect to the gateway and run the event loop.
    Reconnects automatically on disconnect. *)
let connect ~sw ~env t =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let gateway_host = "gateway.discord.gg" in
  let gateway_path = "/?v=10&encoding=json" in
  let rec connect_loop () =
    Logs.info (fun m -> m "gateway: connecting to %s" gateway_host);
    let ws = Websocket.connect ~sw ~net
      ~host:gateway_host ~port:443 ~path:gateway_path in
    t.ws <- Some ws;
    Logs.info (fun m -> m "gateway: WebSocket connected");
    let rec recv_loop () =
      match Websocket.recv_frame ws with
      | { Websocket.opcode = Text; payload } ->
        (try
           let json = Yojson.Safe.from_string payload in
           handle_payload t ~sw ~clock json
         with exn ->
           Logs.warn (fun m -> m "gateway: failed to parse payload: %s"
             (Printexc.to_string exn)));
        recv_loop ()
      | { opcode = Close; _ } ->
        Logs.info (fun m -> m "gateway: received close frame")
      | { opcode = _; _ } ->
        (* Ping/Pong handled inside recv_frame *)
        recv_loop ()
      | exception exn ->
        Logs.warn (fun m -> m "gateway: recv error: %s" (Printexc.to_string exn))
    in
    recv_loop ();
    t.ws <- None;
    (* Reconnect after a brief delay *)
    Logs.info (fun m -> m "gateway: reconnecting in 5s...");
    Eio.Time.sleep clock 5.0;
    connect_loop ()
  in
  connect_loop ()
