(** Discord REST API client — makes real HTTPS requests to Discord's API.

    Assumptions:
    - Bot token auth only
    - No file uploads yet (will add for agent output sharing)
    - Simple rate limiting: sleep on 429 Retry-After, then retry once *)

open Discord_types

let api_base = "https://discord.com/api/v10"

type t = {
  token : string;
  client : Cohttp_eio.Client.t;
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
}

let create ~sw ~(env : Eio_unix.Stdenv.base) ~token =
  Mirage_crypto_rng_unix.use_default ();
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg msg) -> failwith ("ca-certs: " ^ msg)
  in
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Ok c -> c
    | Error (`Msg msg) -> failwith ("tls config: " ^ msg)
  in
  let https uri flow =
    let host =
      Uri.host uri
      |> Option.map (fun h -> Domain_name.(of_string_exn h |> host_exn))
    in
    Tls_eio.client_of_flow tls_config ?host flow
  in
  let net = Eio.Stdenv.net env in
  let client = Cohttp_eio.Client.make ~https:(Some https) net in
  let clock = Eio.Stdenv.clock env in
  { token; client; sw; clock }

let make_headers t =
  Http.Header.of_list [
    ("Authorization", "Bot " ^ t.token);
    ("Content-Type", "application/json");
    ("User-Agent", "DiscordBot (discord-agents/0.1.0, OCaml)");
  ]

(** Read entire body from a cohttp-eio response source.
    Capped at 10MB to prevent OOM from unexpected large responses. *)
let max_body_size = 10 * 1024 * 1024

let read_body (body : Cohttp_eio.Body.t) =
  let buf = Buffer.create 4096 in
  let chunk = Cstruct.create 4096 in
  let truncated = ref false in
  let rec loop () =
    match Eio.Flow.single_read body chunk with
    | n ->
      if not !truncated then begin
        Buffer.add_string buf (Cstruct.to_string ~off:0 ~len:n chunk);
        if Buffer.length buf > max_body_size then
          truncated := true
      end;
      (* Always drain to EOF so the connection stays clean for reuse *)
      loop ()
    | exception End_of_file -> Buffer.contents buf
  in
  loop ()

(** Low-level HTTP request. Returns parsed JSON or error string.
    On 429 (rate limited), sleeps Retry-After seconds and retries once. *)
let request t ~meth ~path ?body () =
  let uri = Uri.of_string (api_base ^ path) in
  let headers = make_headers t in
  let body_str = Option.map (fun j -> Yojson.Safe.to_string j) body in
  let do_call () =
    let cohttp_body = Option.map Cohttp_eio.Body.of_string body_str in
    Cohttp_eio.Client.call t.client ~sw:t.sw ~headers ?body:cohttp_body meth uri
  in
  let handle_response (resp, resp_body) =
    let status = Http.Response.status resp in
    let code = Http.Status.to_int status in
    let body_str = read_body resp_body in
    if code >= 200 && code < 300 then begin
      if String.length body_str = 0 then
        Ok `Null
      else
        Ok (Yojson.Safe.from_string body_str)
    end else
      Error (Printf.sprintf "discord REST %s %s: %d %s"
        (Http.Method.to_string meth) path code body_str)
  in
  try
    let (resp, resp_body) = do_call () in
    let status = Http.Response.status resp in
    let code = Http.Status.to_int status in
    if code = 429 then begin
      let body_str = read_body resp_body in
      let retry_after =
        try
          let json = Yojson.Safe.from_string body_str in
          Yojson.Safe.Util.(json |> member "retry_after" |> to_float)
        with _ -> 5.0
      in
      Logs.warn (fun m -> m "REST: rate limited on %s, retrying after %.1fs" path retry_after);
      Eio.Time.sleep t.clock retry_after;
      handle_response (do_call ())
    end else
      handle_response (resp, resp_body)
  with exn ->
    Error (Printf.sprintf "discord REST %s %s: exception %s"
      (Http.Method.to_string meth) path (Printexc.to_string exn))

(** Send a message to a channel. *)
let create_message t ~(channel_id : Discord_types.channel_id) ~content
    ?(reply_to : Discord_types.message_id option) () =
  let body = `Assoc ([
    ("content", `String content);
  ] @ match reply_to with
    | Some msg_id -> [("message_reference", `Assoc [("message_id", `String msg_id)])]
    | None -> [])
  in
  match request t ~meth:`POST ~path:(Printf.sprintf "/channels/%s/messages" channel_id) ~body () with
  | Ok json ->
    (try Ok (message_of_yojson json)
     with exn -> Error (Printf.sprintf "create_message: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Edit an existing message. *)
let edit_message t ~(channel_id : Discord_types.channel_id)
    ~(message_id : Discord_types.message_id) ~content () =
  let body = `Assoc [("content", `String content)] in
  match request t ~meth:`PATCH
    ~path:(Printf.sprintf "/channels/%s/messages/%s" channel_id message_id)
    ~body () with
  | Ok json ->
    (try Ok (message_of_yojson json)
     with exn -> Error (Printf.sprintf "edit_message: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Send a typing indicator. *)
let send_typing t ~(channel_id : Discord_types.channel_id) () =
  match request t ~meth:`POST
    ~path:(Printf.sprintf "/channels/%s/typing" channel_id) () with
  | Ok _ -> Ok ()
  | Error e -> Error e

(** Create a new text channel in a guild. *)
let create_channel t ~(guild_id : Discord_types.guild_id) ~name ?(channel_type=0)
    ?(parent_id : Discord_types.channel_id option) ?topic () =
  let body = `Assoc ([
    ("name", `String name);
    ("type", `Int channel_type);
  ] @ (match parent_id with Some id -> [("parent_id", `String id)] | None -> [])
    @ (match topic with Some t -> [("topic", `String t)] | None -> []))
  in
  match request t ~meth:`POST ~path:(Printf.sprintf "/guilds/%s/channels" guild_id) ~body () with
  | Ok json ->
    (try Ok (channel_of_yojson json)
     with exn -> Error (Printf.sprintf "create_channel: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Get all channels in a guild. *)
let get_guild_channels t ~(guild_id : Discord_types.guild_id) () =
  match request t ~meth:`GET ~path:(Printf.sprintf "/guilds/%s/channels" guild_id) () with
  | Ok json ->
    (try Ok (Yojson.Safe.Util.to_list json |> List.map channel_of_yojson)
     with exn -> Error (Printf.sprintf "get_guild_channels: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Delete a channel. *)
let delete_channel t ~(channel_id : Discord_types.channel_id) () =
  match request t ~meth:`DELETE ~path:(Printf.sprintf "/channels/%s" channel_id) () with
  | Ok _ -> Ok ()
  | Error e -> Error e

(** Move a channel to a given position within its category. *)
let modify_channel_position t ~(guild_id : Discord_types.guild_id)
    ~(channel_id : Discord_types.channel_id) ~position () =
  let body = `List [`Assoc [
    ("id", `String channel_id);
    ("position", `Int position);
  ]] in
  match request t ~meth:`PATCH ~path:(Printf.sprintf "/guilds/%s/channels" guild_id) ~body () with
  | Ok _ -> Ok ()
  | Error e -> Error e

(** Create a thread from a message. *)
let create_thread t ~(channel_id : Discord_types.channel_id)
    ~(message_id : Discord_types.message_id) ~name () =
  let body = `Assoc [
    ("name", `String name);
    ("auto_archive_duration", `Int 1440);
  ] in
  match request t ~meth:`POST
    ~path:(Printf.sprintf "/channels/%s/messages/%s/threads" channel_id message_id)
    ~body () with
  | Ok json ->
    (try Ok (channel_of_yojson json)
     with exn -> Error (Printf.sprintf "create_thread: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Create a thread without a starter message. *)
let create_thread_no_message t ~(channel_id : Discord_types.channel_id) ~name () =
  let body = `Assoc [
    ("name", `String name);
    ("type", `Int 11);
    ("auto_archive_duration", `Int 1440);
  ] in
  match request t ~meth:`POST
    ~path:(Printf.sprintf "/channels/%s/threads" channel_id)
    ~body () with
  | Ok json ->
    (try Ok (channel_of_yojson json)
     with exn -> Error (Printf.sprintf "create_thread_no_message: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Fetch messages from a channel. *)
let get_messages t ~(channel_id : Discord_types.channel_id) ?(limit=20) () =
  match request t ~meth:`GET
    ~path:(Printf.sprintf "/channels/%s/messages?limit=%d" channel_id limit) () with
  | Ok json ->
    (try
       let msgs = Yojson.Safe.Util.to_list json |> List.map message_of_yojson in
       Ok msgs
     with exn -> Error (Printf.sprintf "get_messages: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Add a reaction to a message. *)
let create_reaction t ~(channel_id : Discord_types.channel_id)
    ~(message_id : Discord_types.message_id) ~emoji () =
  let encoded_emoji = Uri.pct_encode emoji in
  match request t ~meth:`PUT
    ~path:(Printf.sprintf "/channels/%s/messages/%s/reactions/%s/@me"
      channel_id message_id encoded_emoji) () with
  | Ok _ -> Ok ()
  | Error e -> Error e

(** Modify a channel/thread's properties (currently: name only). *)
let modify_channel t ~(channel_id : Discord_types.channel_id) ~name () =
  let body = `Assoc [("name", `String name)] in
  match request t ~meth:`PATCH
    ~path:(Printf.sprintf "/channels/%s" channel_id) ~body () with
  | Ok json ->
    (try Ok (channel_of_yojson json)
     with exn -> Error (Printf.sprintf "modify_channel: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Get a single channel by ID (works for threads too — returns parent_id). *)
let get_channel t ~(channel_id : Discord_types.channel_id) () =
  match request t ~meth:`GET ~path:(Printf.sprintf "/channels/%s" channel_id) () with
  | Ok json ->
    (try Ok (channel_of_yojson json)
     with exn -> Error (Printf.sprintf "get_channel: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Get the gateway URL for WebSocket connection. *)
let get_gateway t =
  match request t ~meth:`GET ~path:"/gateway/bot" () with
  | Ok json ->
    (try
       let url = Yojson.Safe.Util.(json |> member "url" |> to_string) in
       Ok url
     with exn -> Error (Printf.sprintf "get_gateway: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e
