(** Discord REST API client — makes real HTTPS requests to Discord's API.

    Assumptions:
    - Bot token auth only
    - No file uploads yet (will add for agent output sharing)
    - Transient transport failures are retried only for operations we can
      safely repeat (idempotent calls, or create_message with Discord
      nonce dedupe). *)

open Discord_types

let api_base = "https://discord.com/api/v10"

type t = {
  token : string;
  client : Cohttp_eio.Client.t;
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  mutable last_transport_error : string option;
  mutable consecutive_transport_failures : int;
  mutable transport_backoff_until : float;
}

type retry_mode =
  | No_retry
  | Retry_transient

type transport_error_kind =
  | Hostname_resolution
  | Timeout
  | Connection
  | Tls
  | Other_transport

let max_rate_limit_retries = 1
let max_transient_attempts = 4
let base_transport_backoff_s = 0.5
let max_transport_backoff_s = 8.0

let raise_if_cancelled exn =
  match exn with
  | Eio.Cancel.Cancelled _ -> raise exn
  | _ -> ()

let string_contains s needle =
  let s_len = String.length s in
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > s_len then false
    else if String.sub s i needle_len = needle then true
    else loop (i + 1)
  in
  needle_len = 0 || loop 0

let classify_transport_error_message msg =
  let msg = String.lowercase_ascii msg in
  if string_contains msg "failed to resolve hostname"
     || string_contains msg "name resolution"
     || string_contains msg "getaddrinfo"
     || string_contains msg "no address associated with hostname"
  then Hostname_resolution
  else if string_contains msg "timed out"
          || string_contains msg "timeout"
          || string_contains msg "etimedout"
  then Timeout
  else if string_contains msg "tls"
          || string_contains msg "ssl"
          || string_contains msg "certificate"
  then Tls
  else if string_contains msg "connection reset"
          || string_contains msg "broken pipe"
          || string_contains msg "connection refused"
          || string_contains msg "connection aborted"
          || string_contains msg "network is unreachable"
          || string_contains msg "host is unreachable"
          || string_contains msg "end_of_file"
          || string_contains msg "econn"
  then Connection
  else Other_transport

let string_of_transport_error_kind = function
  | Hostname_resolution -> "hostname_resolution"
  | Timeout -> "timeout"
  | Connection -> "connection"
  | Tls -> "tls"
  | Other_transport -> "other_transport"

let transport_backoff_seconds failure_count =
  let exponent = max 0 (failure_count - 1) in
  min max_transport_backoff_s
    (base_transport_backoff_s *. (2. ** float_of_int exponent))

let transport_retry_delay_s t =
  max 0.0 (t.transport_backoff_until -. Unix.gettimeofday ())

let transport_degraded t =
  t.consecutive_transport_failures > 0

let last_transport_error t =
  t.last_transport_error

let consecutive_transport_failures t =
  t.consecutive_transport_failures

let note_transport_recovery t =
  if t.consecutive_transport_failures > 0 then
    Logs.info (fun m -> m "REST transport recovered after %d failure(s)"
      t.consecutive_transport_failures);
  t.last_transport_error <- None;
  t.consecutive_transport_failures <- 0;
  t.transport_backoff_until <- 0.0

let note_transport_failure t ~host exn =
  let raw = Printexc.to_string exn in
  let kind =
    classify_transport_error_message raw
    |> string_of_transport_error_kind
  in
  let failures = t.consecutive_transport_failures + 1 in
  let delay = transport_backoff_seconds failures in
  let summary =
    Printf.sprintf "host=%s kind=%s error=%s" host kind raw
  in
  let now = Unix.gettimeofday () in
  t.last_transport_error <- Some summary;
  t.consecutive_transport_failures <- failures;
  t.transport_backoff_until <- max t.transport_backoff_until (now +. delay);
  (summary, delay)

let wait_for_transport_backoff t =
  let delay = transport_retry_delay_s t in
  if delay > 0.0 then
    Eio.Time.sleep t.clock delay

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
  { token; client; sw; clock;
    last_transport_error = None;
    consecutive_transport_failures = 0;
    transport_backoff_until = 0.0; }

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

(** Truncate a byte string near [max_len] for log output. Preserves
    UTF-8 validity without verifying it: if the input is valid UTF-8,
    the prefix is also valid (we walk back past continuation bytes at
    the cut point to land on a codepoint boundary). If the input is
    already invalid (stray continuation bytes, lone leading bytes),
    that invalidity may remain in the prefix — we don't scan or sanitize
    bytes that were already there. Destination is a log; the caller
    wants bounded length, not full validation. *)
let truncate_for_log ?(max_len = 500) s =
  let len = String.length s in
  if len <= max_len then s
  else begin
    let is_continuation b = Char.code b land 0xC0 = 0x80 in
    (* Walk back without a step bound. Worst-case O(max_len), which is
       fine — we already paid O(len) to receive the string. The previous
       4-step bound was incorrect for invalid UTF-8 (an all-continuation-
       bytes tail would leave cut still pointing inside a sequence). *)
    let rec find_boundary i =
      if i = 0 then 0
      else if is_continuation s.[i] then find_boundary (i - 1)
      else i
    in
    let cut = find_boundary max_len in
    String.sub s 0 cut ^ "... (truncated)"
  end

(** Return a short head-of-body excerpt for user-facing Error strings.
    Returns [s] unchanged if it's within [max_len] bytes; otherwise
    returns a prefix near [max_len] bytes plus the "... (truncated)"
    suffix added by [truncate_for_log]. Final length can exceed [max_len]
    by the suffix length. Inherits [truncate_for_log]'s UTF-8 behavior
    (valid input stays valid; already-invalid input passes through). *)
let body_snippet ?(max_len = 150) s =
  if String.length s <= max_len then s
  else truncate_for_log ~max_len s

let message_nonce () =
  Resource.random_hex 12

let create_message_body ~content ?reply_to ~nonce () =
  `Assoc ([
    ("content", `String content);
    ("nonce", `String nonce);
    ("enforce_nonce", `Bool true);
  ] @ match reply_to with
    | Some msg_id ->
      [("message_reference", `Assoc [("message_id", `String msg_id)])]
    | None -> [])

(** Low-level HTTP request. Returns parsed JSON or error string.
    On 429 (rate limited), sleeps Retry-After seconds and retries once.
    Retryable operations also back off and retry on transient transport
    failures or 5xx responses. Non-2xx responses are logged centrally so
    callers that [ignore] the Result still surface failures. 404s log at
    debug level (expected for typing/reactions on deleted channels); other
    errors log at warn. *)
let request ?(retry_mode = No_retry) t ~meth ~path ?body () =
  let uri = Uri.of_string (api_base ^ path) in
  let host = Uri.host uri |> Option.value ~default:"discord.com" in
  let headers = make_headers t in
  let body_str = Option.map (fun j -> Yojson.Safe.to_string j) body in
  let meth_str = Http.Method.to_string meth in
  let do_call () =
    let cohttp_body = Option.map Cohttp_eio.Body.of_string body_str in
    Cohttp_eio.Client.call t.client ~sw:t.sw ~headers ?body:cohttp_body meth uri
  in
  let log_non_2xx code body =
    let body = truncate_for_log body in
    if code = 404 then
      Logs.debug (fun m -> m "REST %s %s: %d %s" meth_str path code body)
    else
      Logs.warn (fun m -> m "REST %s %s: %d %s" meth_str path code body);
    (* On 400, also log a snippet of the request body. Discord's
       50109 ("invalid JSON") tells us nothing about which bytes
       offended; without the request body it's near-impossible to
       diagnose. The Resource.sanitize_utf8 patch should keep
       50109 from the [content] path, but other endpoints / fields
       can still hit it.

       Privacy note: for create_message / edit_message the body holds
       the agent's text or tool output verbatim — same content the
       channel already sees, so this just mirrors it to the operator
       log. Bot tokens are not in the body (auth is header-only).
       Acceptable for the self-hosted single-user bot this codebase
       targets; a multi-tenant deployment would want per-method
       redaction. *)
    if code = 400 then
      Option.iter (fun b ->
        Logs.warn (fun m -> m "REST %s %s: 400 request body: %s"
          meth_str path (truncate_for_log b))
      ) body_str
  in
  let handle_response code body_str =
    if code >= 200 && code < 300 then begin
      if String.length body_str = 0 then Ok `Null
      else
        try Ok (Yojson.Safe.from_string body_str)
        with exn ->
          Logs.warn (fun m ->
            m "REST %s %s: response parse error: %s"
              meth_str path (Printexc.to_string exn));
          Error (Printf.sprintf "discord REST %s %s: response parse error %s"
            meth_str path (body_snippet (Printexc.to_string exn)))
    end else begin
      (* Log the full body (up to truncate_for_log's cap) centrally for
         operator debugging, and include a short body snippet in the
         Error string so user-facing surfaces (bot replies, control-API
         responses) retain actionable detail like "Missing Permissions"
         without bloating duplicated logs. *)
      log_non_2xx code body_str;
      Error (Printf.sprintf "discord REST %s %s: HTTP %d %s"
        meth_str path code (body_snippet body_str))
    end
  in
  let rec loop attempt rate_limit_retries =
    wait_for_transport_backoff t;
    try
      let (resp, resp_body) = do_call () in
      note_transport_recovery t;
      let status = Http.Response.status resp in
      let code = Http.Status.to_int status in
      let body_str = read_body resp_body in
      if code = 429 && rate_limit_retries < max_rate_limit_retries then begin
        let retry_after =
          try
            let json = Yojson.Safe.from_string body_str in
            Yojson.Safe.Util.(json |> member "retry_after" |> to_float)
          with _ -> 5.0
        in
        Logs.warn (fun m ->
          m "REST %s %s: rate limited, retrying after %.1fs"
            meth_str path retry_after);
        Eio.Time.sleep t.clock retry_after;
        loop attempt (rate_limit_retries + 1)
      end else if code >= 500 && code < 600
                && retry_mode = Retry_transient
                && attempt < max_transient_attempts then begin
        log_non_2xx code body_str;
        let delay = transport_backoff_seconds attempt in
        Logs.warn (fun m ->
          m "REST %s %s: HTTP %d on attempt %d/%d, retrying in %.1fs"
            meth_str path code attempt max_transient_attempts delay);
        Eio.Time.sleep t.clock delay;
        loop (attempt + 1) rate_limit_retries
      end else
        handle_response code body_str
    with exn ->
      raise_if_cancelled exn;
      let (summary, delay) = note_transport_failure t ~host exn in
      let can_retry =
        retry_mode = Retry_transient && attempt < max_transient_attempts
      in
      if can_retry then begin
        Logs.warn (fun m ->
          m "REST %s %s: transport failure on attempt %d/%d, retrying in %.1fs (%s)"
            meth_str path attempt max_transient_attempts delay
            (truncate_for_log summary));
        Eio.Time.sleep t.clock delay;
        loop (attempt + 1) rate_limit_retries
      end else begin
        Logs.warn (fun m -> m "REST %s %s: exception %s"
          meth_str path (truncate_for_log summary));
        let suffix =
          if attempt > 1 then
            Printf.sprintf " after %d attempts" attempt
          else
            ""
        in
        Error (Printf.sprintf "discord REST %s %s: exception%s %s"
          meth_str path suffix (body_snippet summary))
      end
  in
  loop 1 0

(** Plan the chunks for a [create_message] call. Pure function, separated
    from I/O so it can be unit-tested. Content fitting in a single Discord
    message (\u2264 [discord_max_len]) returns a singleton carrying [reply_to].
    Only the first chunk carries [reply_to] when split; follow-ups post as
    standalone messages. *)
let plan_message_chunks ?reply_to content =
  if String.length content <= Agent_process.discord_max_len then
    [(content, reply_to)]
  else
    match Agent_process.split_message content with
    | [] -> [(content, reply_to)]  (* split_message never returns [] on non-empty input *)
    | first :: rest ->
      (first, reply_to) :: List.map (fun c -> (c, None)) rest

(** Send a message to a channel. Content over Discord's 2000-char limit
    is transparently split into multiple messages via [Agent_process.split_message],
    which preserves code-fence continuity. Only the first chunk carries
    [reply_to]; follow-up chunks post as regular messages. The returned
    message is the first chunk (ids of follow-ups are not exposed).

    If a follow-up chunk fails, we stop and return Error so the caller
    knows delivery was incomplete, rather than silently returning Ok
    with missing content in the middle.

    Partial-delivery semantics: Error does NOT imply nothing was
    delivered — the first chunk (and possibly several follow-ups) may
    already be visible in the channel. There is no automatic rollback;
    callers that need "all-or-nothing" must clean up themselves. *)
let create_message t ~(channel_id : Discord_types.channel_id) ~content
    ?(reply_to : Discord_types.message_id option) () =
  (* Strip invalid UTF-8 before splitting / sending: Discord's API
     returns 400 / code 50109 for any raw invalid-UTF-8 byte sequence
     in the request body, and [agent_runner.send] doesn't retry on 400
     — the chunk just vanishes from the user's view. See
     [Resource.sanitize_utf8] for the full rationale. *)
  let content = Resource.sanitize_utf8 content in
  let post_one (chunk, chunk_reply_to) =
    let body =
      create_message_body ~content:chunk ?reply_to:chunk_reply_to
        ~nonce:(message_nonce ()) ()
    in
    match request ~retry_mode:Retry_transient t ~meth:`POST
      ~path:(Printf.sprintf "/channels/%s/messages" channel_id) ~body () with
    | Ok json ->
      (try Ok (message_of_yojson json)
       with exn -> Error (Printf.sprintf "create_message: parse error: %s"
         (Printexc.to_string exn)))
    | Error e -> Error e
  in
  match plan_message_chunks ?reply_to content with
  | [] -> Error "create_message: empty plan (should not happen)"
  | [single] -> post_one single
  | first :: rest ->
    Logs.info (fun m -> m "create_message: content %d chars exceeds Discord limit; split into %d chunks"
      (String.length content) (List.length rest + 1));
    (match post_one first with
     | Error e -> Error e
     | Ok first_msg ->
       let rec send_rest = function
         | [] -> Ok first_msg
         | chunk :: more ->
           match post_one chunk with
           | Ok _ -> send_rest more
           | Error e ->
             Error (Printf.sprintf
               "create_message: follow-up chunk failed, delivery incomplete: %s" e)
       in
       send_rest rest)

(** Edit an existing message. *)
let edit_message t ~(channel_id : Discord_types.channel_id)
    ~(message_id : Discord_types.message_id) ~content () =
  let content = Resource.sanitize_utf8 content in
  let body = `Assoc [("content", `String content)] in
  match request ~retry_mode:Retry_transient t ~meth:`PATCH
    ~path:(Printf.sprintf "/channels/%s/messages/%s" channel_id message_id)
    ~body () with
  | Ok json ->
    (try Ok (message_of_yojson json)
     with exn -> Error (Printf.sprintf "edit_message: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Send a typing indicator. *)
let send_typing t ~(channel_id : Discord_types.channel_id) () =
  match request ~retry_mode:Retry_transient t ~meth:`POST
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
  match request ~retry_mode:Retry_transient t ~meth:`GET
    ~path:(Printf.sprintf "/guilds/%s/channels" guild_id) () with
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
  match request ~retry_mode:Retry_transient t ~meth:`PATCH
    ~path:(Printf.sprintf "/guilds/%s/channels" guild_id) ~body () with
  | Ok _ -> Ok ()
  | Error e -> Error e

(** Set positions for multiple channels in a single API call.
    Takes a list of (channel_id, position) pairs. This avoids
    race conditions from sequential single-channel position updates. *)
let batch_modify_channel_positions t ~(guild_id : Discord_types.guild_id)
    ~positions () =
  let body = `List (List.map (fun (channel_id, position) ->
    `Assoc [
      ("id", `String channel_id);
      ("position", `Int position);
    ]
  ) positions) in
  match request ~retry_mode:Retry_transient t ~meth:`PATCH
    ~path:(Printf.sprintf "/guilds/%s/channels" guild_id) ~body () with
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
  match request ~retry_mode:Retry_transient t ~meth:`GET
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
  match request ~retry_mode:Retry_transient t ~meth:`PUT
    ~path:(Printf.sprintf "/channels/%s/messages/%s/reactions/%s/@me"
      channel_id message_id encoded_emoji) () with
  | Ok _ -> Ok ()
  | Error e -> Error e

(** Remove own reaction from a message. *)
let delete_own_reaction t ~(channel_id : Discord_types.channel_id)
    ~(message_id : Discord_types.message_id) ~emoji () =
  let encoded_emoji = Uri.pct_encode emoji in
  match request t ~meth:`DELETE
    ~path:(Printf.sprintf "/channels/%s/messages/%s/reactions/%s/@me"
      channel_id message_id encoded_emoji) () with
  | Ok _ -> Ok ()
  | Error e -> Error e

(** Modify a channel/thread's properties (currently: name only). *)
let modify_channel t ~(channel_id : Discord_types.channel_id) ~name () =
  let body = `Assoc [("name", `String name)] in
  match request ~retry_mode:Retry_transient t ~meth:`PATCH
    ~path:(Printf.sprintf "/channels/%s" channel_id) ~body () with
  | Ok json ->
    (try Ok (channel_of_yojson json)
     with exn -> Error (Printf.sprintf "modify_channel: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Get a single channel by ID (works for threads too — returns parent_id). *)
let get_channel t ~(channel_id : Discord_types.channel_id) () =
  match request ~retry_mode:Retry_transient t ~meth:`GET
    ~path:(Printf.sprintf "/channels/%s" channel_id) () with
  | Ok json ->
    (try Ok (channel_of_yojson json)
     with exn -> Error (Printf.sprintf "get_channel: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e

(** Download a file from an arbitrary URL (e.g. Discord CDN).
    Returns the raw bytes on success. *)
let download_url t ~url () =
  let uri = Uri.of_string url in
  let headers = Http.Header.of_list [
    ("User-Agent", "DiscordBot (discord-agents/0.1.0, OCaml)");
  ] in
  let rec loop attempt =
    try
      let (resp, body) =
        Cohttp_eio.Client.call t.client ~sw:t.sw ~headers `GET uri in
      let status = Http.Response.status resp in
      let code = Http.Status.to_int status in
      let body_str = read_body body in
      if code >= 200 && code < 300 then Ok body_str
      else if code >= 500 && code < 600 && attempt < max_transient_attempts then begin
        let delay = transport_backoff_seconds attempt in
        Logs.warn (fun m ->
          m "download_url %s: HTTP %d on attempt %d/%d, retrying in %.1fs"
            url code attempt max_transient_attempts delay);
        Eio.Time.sleep t.clock delay;
        loop (attempt + 1)
      end else
        Error (Printf.sprintf "download_url %s: HTTP %d" url code)
    with exn ->
      raise_if_cancelled exn;
      let summary =
        Printf.sprintf "kind=%s error=%s"
          (classify_transport_error_message (Printexc.to_string exn)
           |> string_of_transport_error_kind)
          (Printexc.to_string exn)
      in
      let delay = transport_backoff_seconds attempt in
      if attempt < max_transient_attempts then begin
        Logs.warn (fun m ->
          m "download_url %s: transport failure on attempt %d/%d, retrying in %.1fs (%s)"
            url attempt max_transient_attempts delay
            (truncate_for_log summary));
        Eio.Time.sleep t.clock delay;
        loop (attempt + 1)
      end else
        Error (Printf.sprintf "download_url %s: %s" url summary)
  in
  loop 1

(** Get the gateway URL for WebSocket connection. *)
let get_gateway t =
  match request ~retry_mode:Retry_transient t ~meth:`GET
    ~path:"/gateway/bot" () with
  | Ok json ->
    (try
       let url = Yojson.Safe.Util.(json |> member "url" |> to_string) in
       Ok url
     with exn -> Error (Printf.sprintf "get_gateway: parse error: %s" (Printexc.to_string exn)))
  | Error e -> Error e
