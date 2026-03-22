(** Discord Gateway (WebSocket) client.

    Handles the connection lifecycle:
    1. Connect to wss://gateway.discord.gg/?v=10&encoding=json
    2. Receive Hello, start heartbeating
    3. Send Identify with token + intents
    4. Dispatch events to handler callback
    5. Maintain heartbeat, handle reconnect/resume

    On disconnect with a valid session_id + sequence, we attempt to
    Resume the session instead of re-Identifying, which avoids missing
    events during the reconnect window. *)

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
  mutable heartbeat_cancel : bool;  (* signal old heartbeat daemon to stop *)
  mutable resuming : bool;
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
    heartbeat_cancel = false;
    resuming = false;
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
  | Some ws ->
    (try Websocket.send_text ws (Yojson.Safe.to_string json)
     with exn ->
       Logs.warn (fun m -> m "gateway: send_json error: %s" (Printexc.to_string exn)))
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

(** Build a Resume payload for reconnecting without losing events. *)
let resume_payload t =
  `Assoc [
    ("op", `Int 6);
    ("d", `Assoc [
      ("token", `String t.token);
      ("session_id", `String (Option.value ~default:"" t.session_id));
      ("seq", match t.sequence with Some s -> `Int s | None -> `Null);
    ]);
  ]

(** Can we attempt a resume? Requires both session_id and sequence. *)
let can_resume t =
  Option.is_some t.session_id && Option.is_some t.sequence

(** Parse a gateway payload and dispatch events. *)
let handle_payload t ~sw ~(clock : _ Eio.Time.clock) json =
  let open Yojson.Safe.Util in
  let op = match json |> member "op" |> to_int_option with
    | Some n -> gateway_opcode_of_int n
    | None -> Unknown_op (-1)
  in
  let d = json |> member "d" in
  let s = json |> member "s" |> to_int_option in
  (* Update sequence for heartbeats *)
  (match s with Some _ -> t.sequence <- s | None -> ());
  match op with
  | Dispatch ->
    let event_name = json |> member "t" |> to_string_option
      |> Option.value ~default:"UNKNOWN" in
    (match event_name with
     | "READY" ->
       (try
          let user = d |> member "user" |> user_of_yojson in
          t.session_id <- Some (d |> member "session_id" |> to_string);
          let resume_url = d |> member "resume_gateway_url" |> to_string_option in
          t.resume_gateway_url <- resume_url;
          t.resuming <- false;
          Logs.info (fun m -> m "gateway: READY as %s (session %s)"
            user.username (Option.value ~default:"?" t.session_id));
          t.handler (Connected user)
        with exn ->
          Logs.warn (fun m -> m "gateway: failed to parse READY: %s"
            (Printexc.to_string exn)))
     | "RESUMED" ->
       t.resuming <- false;
       Logs.info (fun m -> m "gateway: RESUMED successfully (session %s)"
         (Option.value ~default:"?" t.session_id))
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
    let heartbeat_interval = match d |> member "heartbeat_interval" |> to_int_option with
      | Some i -> i
      | None -> 41250  (* fallback default *)
    in
    t.heartbeat_interval_ms <- heartbeat_interval;
    t.last_heartbeat_acked <- true;
    (* Cancel any previous heartbeat daemon *)
    t.heartbeat_cancel <- true;
    Logs.info (fun m -> m "gateway: Hello, heartbeat interval %dms" heartbeat_interval);
    (* Start new heartbeat daemon *)
    t.heartbeat_cancel <- false;
    Eio.Fiber.fork_daemon ~sw (fun () ->
      (* Initial jitter: random fraction of the interval *)
      let jitter = Random.float (float_of_int heartbeat_interval /. 1000.0) in
      Eio.Time.sleep clock jitter;
      let rec heartbeat_loop () =
        (* Check if we've been cancelled by a newer Hello *)
        if t.heartbeat_cancel then
          `Stop_daemon
        else if not t.last_heartbeat_acked then begin
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
    (* Send Identify or Resume depending on whether we have a session *)
    if t.resuming && can_resume t then begin
      Logs.info (fun m -> m "gateway: sending Resume (session %s, seq %s)"
        (Option.value ~default:"?" t.session_id)
        (match t.sequence with Some s -> string_of_int s | None -> "null"));
      send_json t (resume_payload t)
    end else begin
      Logs.info (fun m -> m "gateway: sending Identify");
      t.resuming <- false;
      send_json t (identify_payload t)
    end
  | Heartbeat ->
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
      t.sequence <- None;
      t.resuming <- false
    end;
    (match t.ws with Some ws -> Websocket.send_close ws | None -> ());
    t.handler (Disconnected "invalid session")
  | _ ->
    Logs.debug (fun m -> m "gateway: unhandled opcode")

(** Connect to the gateway and run the event loop.
    Reconnects automatically on disconnect, using Resume when possible.
    Uses exponential backoff on repeated failures. *)
let connect ~sw ~(env : Eio_unix.Stdenv.base) t =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let gateway_host = "gateway.discord.gg" in
  let gateway_path = "/?v=10&encoding=json" in
  let backoff = ref 5.0 in
  let rec connect_loop () =
    let host = match t.resume_gateway_url with
      | Some url ->
        (match Uri.host (Uri.of_string url) with
         | Some h -> h
         | None -> gateway_host)
      | None -> gateway_host
    in
    Logs.info (fun m -> m "gateway: connecting to %s" host);
    (match
      (try Ok (Websocket.connect ~sw ~net ~host ~port:443 ~path:gateway_path)
       with exn -> Error exn)
    with
    | Error exn ->
      Logs.warn (fun m -> m "gateway: connect failed: %s" (Printexc.to_string exn));
      Logs.info (fun m -> m "gateway: retrying in %.0fs..." !backoff);
      Eio.Time.sleep clock !backoff;
      backoff := min (!backoff *. 2.0) 60.0;
      connect_loop ()
    | Ok ws ->
      t.ws <- Some ws;
      backoff := 5.0;  (* Reset backoff on successful connect *)
      Logs.info (fun m -> m "gateway: WebSocket connected");
      (* Cancel any stale heartbeat daemon from previous connection *)
      t.heartbeat_cancel <- true;
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
          recv_loop ()
        | exception exn ->
          Logs.warn (fun m -> m "gateway: recv error: %s" (Printexc.to_string exn))
      in
      recv_loop ();
      t.ws <- None;
      t.heartbeat_cancel <- true;
      if can_resume t then
        t.resuming <- true;
      Logs.info (fun m -> m "gateway: reconnecting in %.0fs..." !backoff);
      Eio.Time.sleep clock !backoff;
      backoff := min (!backoff *. 1.5) 60.0;
      connect_loop ())
  in
  connect_loop ()
