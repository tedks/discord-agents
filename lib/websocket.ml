(** Minimal WebSocket client over Eio TLS sockets.

    Implements just enough of RFC 6455 for Discord's text-only JSON gateway:
    - HTTP upgrade handshake
    - Text frame send/receive with client-side masking
    - Ping/pong handling
    - Close frame handling

    Does NOT implement: binary frames, extensions, compression, fragmentation
    of outgoing messages (Discord messages are small enough to fit in one frame). *)

type frame_opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
  | Unknown of int

type frame = {
  opcode : frame_opcode;
  payload : string;
}

type t = {
  flow : Eio.Flow.two_way_ty Eio.Resource.t;
  reader : Eio.Buf_read.t;
  mutable closed : bool;
}

let opcode_of_int = function
  | 0 -> Continuation
  | 1 -> Text
  | 2 -> Binary
  | 8 -> Close
  | 9 -> Ping
  | 10 -> Pong
  | n -> Unknown n

let int_of_opcode = function
  | Continuation -> 0
  | Text -> 1
  | Binary -> 2
  | Close -> 8
  | Ping -> 9
  | Pong -> 10
  | Unknown n -> n

(** Generate 4 random bytes for WebSocket client masking.
    Uses mirage-crypto-rng (already initialized by discord_rest). *)
let generate_mask_key () =
  Bytes.of_string (Mirage_crypto_rng.generate 4)

(** Generate a random 16-byte base64-encoded key for the Sec-WebSocket-Key header. *)
let generate_ws_key () =
  Base64.encode_exn (Mirage_crypto_rng.generate 16)

(** Apply XOR masking to payload bytes (in-place). *)
let apply_mask ~mask_key payload_bytes =
  for i = 0 to Bytes.length payload_bytes - 1 do
    let mask_byte = Bytes.get mask_key (i mod 4) in
    let orig = Bytes.get payload_bytes i in
    Bytes.set payload_bytes i (Char.chr (Char.code orig lxor Char.code mask_byte))
  done

(** Send a WebSocket frame. Client frames are always masked per RFC 6455. *)
let send_frame t ~opcode payload =
  if t.closed then failwith "websocket: connection closed";
  let payload_len = String.length payload in
  let buf = Buffer.create (payload_len + 14) in
  (* First byte: FIN=1 | opcode *)
  Buffer.add_char buf (Char.chr (0x80 lor (int_of_opcode opcode)));
  (* Second byte: MASK=1 | payload length *)
  if payload_len < 126 then
    Buffer.add_char buf (Char.chr (0x80 lor payload_len))
  else if payload_len < 65536 then begin
    Buffer.add_char buf (Char.chr (0x80 lor 126));
    Buffer.add_char buf (Char.chr (payload_len lsr 8));
    Buffer.add_char buf (Char.chr (payload_len land 0xFF))
  end else begin
    Buffer.add_char buf (Char.chr (0x80 lor 127));
    for i = 7 downto 0 do
      Buffer.add_char buf (Char.chr ((payload_len lsr (i * 8)) land 0xFF))
    done
  end;
  (* Masking key *)
  let mask_key = generate_mask_key () in
  Buffer.add_bytes buf mask_key;
  (* Masked payload *)
  let masked = Bytes.of_string payload in
  apply_mask ~mask_key masked;
  Buffer.add_bytes buf masked;
  let data = Buffer.contents buf in
  Eio.Flow.write (t.flow :> Eio.Flow.sink_ty Eio.Resource.t)
    [Cstruct.of_string data]

(** Read exactly n bytes from the buffered reader. *)
let read_exactly reader n =
  Eio.Buf_read.take n reader

(** Read a WebSocket frame from the connection.
    Handles fragmented incoming messages by accumulating continuation frames.
    Server frames are NOT masked per the spec. *)
(** Maximum payload size we'll accept (100MB). Prevents OOM from malicious frames. *)
let max_payload_size = 100 * 1024 * 1024

let recv_frame t =
  if t.closed then failwith "websocket: connection closed";
  let acc_buf = Buffer.create 4096 in
  let rec read_fragments ~first_opcode =
    let b0 = Char.code (Eio.Buf_read.any_char t.reader) in
    let fin = b0 land 0x80 <> 0 in
    let opcode_int = b0 land 0x0F in
    let b1 = Char.code (Eio.Buf_read.any_char t.reader) in
    let masked = b1 land 0x80 <> 0 in
    let payload_len_initial = b1 land 0x7F in
    let payload_len =
      if payload_len_initial < 126 then payload_len_initial
      else if payload_len_initial = 126 then begin
        let hi = Char.code (Eio.Buf_read.any_char t.reader) in
        let lo = Char.code (Eio.Buf_read.any_char t.reader) in
        (hi lsl 8) lor lo
      end else begin
        let len = ref 0 in
        for _ = 0 to 7 do
          len := (!len lsl 8) lor Char.code (Eio.Buf_read.any_char t.reader)
        done;
        if !len > max_payload_size then
          failwith (Printf.sprintf "websocket: payload too large: %d bytes" !len);
        !len
      end
    in
    if payload_len > max_payload_size then
      failwith (Printf.sprintf "websocket: payload too large: %d bytes" payload_len);
    let mask_key =
      if masked then Some (Bytes.of_string (read_exactly t.reader 4))
      else None
    in
    let payload_bytes = Bytes.of_string (read_exactly t.reader payload_len) in
    (match mask_key with
     | Some key -> apply_mask ~mask_key:key payload_bytes
     | None -> ());
    let payload = Bytes.to_string payload_bytes in
    let effective_opcode = if opcode_int = 0 then first_opcode else opcode_of_int opcode_int in
    match effective_opcode with
    | Ping ->
      send_frame t ~opcode:Pong payload;
      read_fragments ~first_opcode
    | Close ->
      t.closed <- true;
      (try send_frame t ~opcode:Close "" with _ -> ());
      { opcode = Close; payload }
    | _ ->
      Buffer.add_string acc_buf payload;
      if Buffer.length acc_buf > max_payload_size then
        failwith "websocket: accumulated fragments too large";
      if fin then
        { opcode = effective_opcode; payload = Buffer.contents acc_buf }
      else
        read_fragments ~first_opcode:effective_opcode
  in
  read_fragments ~first_opcode:(Unknown 0)

let send_text t text = send_frame t ~opcode:Text text

let send_close t =
  if not t.closed then begin
    t.closed <- true;
    (try send_frame t ~opcode:Close "" with _ -> ())
  end

(** Perform the WebSocket upgrade handshake over an existing TLS connection. *)
let handshake t ~host ~path =
  let key = generate_ws_key () in
  let request = Printf.sprintf
"GET %s HTTP/1.1\r\n\
Host: %s\r\n\
Upgrade: websocket\r\n\
Connection: Upgrade\r\n\
Sec-WebSocket-Key: %s\r\n\
Sec-WebSocket-Version: 13\r\n\
\r\n"
    path host key
  in
  Eio.Flow.write (t.flow :> Eio.Flow.sink_ty Eio.Resource.t)
    [Cstruct.of_string request];
  (* Read the HTTP response line *)
  let status_line = Eio.Buf_read.line t.reader in
  (* Expect "HTTP/1.1 101 Switching Protocols" *)
  if not (String.length status_line >= 12
          && String.sub status_line 9 3 = "101") then
    failwith (Printf.sprintf "websocket: upgrade failed: %s" status_line);
  (* Read headers until empty line *)
  let rec skip_headers () =
    let line = Eio.Buf_read.line t.reader in
    if String.length line > 0 && line <> "\r" then skip_headers ()
  in
  skip_headers ()

(** Connect to a WebSocket server over TLS.
    Returns a connected websocket handle ready for send/recv. *)
let connect ~sw ~net ~host ~port ~path =
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
  let addr = Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host in
  let tcp_flow =
    match addr with
    | ip :: _ -> Eio.Net.connect ~sw net ip
    | [] -> failwith (Printf.sprintf "websocket: failed to resolve %s" host)
  in
  let host_dn =
    match Domain_name.of_string host with
    | Ok dn -> (match Domain_name.host dn with Ok h -> Some h | Error _ -> None)
    | Error _ -> None
  in
  (* If TLS or handshake fails, close the TCP flow to avoid leaking it *)
  match
    let tls_flow = Tls_eio.client_of_flow tls_config ?host:host_dn tcp_flow in
    let flow = (tls_flow :> Eio.Flow.two_way_ty Eio.Resource.t) in
    let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
    let t = { flow; reader; closed = false } in
    handshake t ~host ~path;
    t
  with
  | t -> t
  | exception exn ->
    Eio.Resource.close tcp_flow;
    raise exn
