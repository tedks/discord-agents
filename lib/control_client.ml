(** Client for the bot's line-delimited JSON control socket. *)

type request = {
  method_name : string;
  params : Yojson.Safe.t option;
  timeout_s : int;
}

type t = {
  request : request -> (Yojson.Safe.t, string) result;
}

let make ~request =
  { request }

let request t ?params ?(timeout_s=60) method_name =
  t.request { method_name; params; timeout_s }

let request_method t ?params method_id =
  match Control_api.method_spec_of_id method_id with
  | None ->
    Error (
      Printf.sprintf "missing control method: %s"
        (Control_api.string_of_method_id method_id)
    )
  | Some spec ->
    request t ?params
      ~timeout_s:(Control_api.method_spec_timeout_s spec)
      (Control_api.method_spec_name spec)

let json_of_request request =
  let fields = [("method", `String request.method_name)] in
  let fields =
    match request.params with
    | None -> fields
    | Some params -> ("params", params) :: fields
  in
  `Assoc (List.rev fields)

let request_line request =
  Yojson.Safe.to_string (json_of_request request) ^ "\n"

let max_response_size = 1_000_000

let write_all fd data =
  let length = String.length data in
  let rec loop offset =
    if offset < length then begin
      let written = Unix.write_substring fd data offset (length - offset) in
      if written = 0 then raise End_of_file;
      loop (offset + written)
    end
  in
  loop 0

let find_newline bytes length =
  let rec loop index =
    if index >= length then None
    else if Bytes.get bytes index = '\n' then Some index
    else loop (index + 1)
  in
  loop 0

let read_line fd =
  let chunk = Bytes.create 4096 in
  let buffer = Buffer.create 4096 in
  let rec loop total =
    if total > max_response_size then
      Error "Control API error: response too large"
    else
      match Unix.read fd chunk 0 (Bytes.length chunk) with
      | 0 ->
        if Buffer.length buffer = 0 then
          Error "Control API error: empty response"
        else
          Ok (Buffer.contents buffer)
      | n ->
        (match find_newline chunk n with
         | Some newline ->
           if total + newline > max_response_size then
             Error "Control API error: response too large"
           else begin
           Buffer.add_subbytes buffer chunk 0 newline;
             Ok (Buffer.contents buffer)
           end
         | None ->
           if total + n > max_response_size then
             Error "Control API error: response too large"
           else begin
           Buffer.add_subbytes buffer chunk 0 n;
             loop (total + n)
           end)
  in
  loop 0

let error_of_unix = function
  | Unix.ENOENT -> "Bot is not running (control socket not found)."
  | Unix.ECONNREFUSED -> "Bot is not running (connection refused)."
  | Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.ETIMEDOUT ->
    "Bot did not respond in time."
  | code ->
    Printf.sprintf "Control API error: %s" (Unix.error_message code)

let request_unix ~socket_path request =
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  try
    Fun.protect ~finally:(fun () ->
      try Unix.close fd with Unix.Unix_error _ -> ())
      (fun () ->
        let timeout = float_of_int request.timeout_s in
        Unix.setsockopt_float fd Unix.SO_RCVTIMEO timeout;
        Unix.setsockopt_float fd Unix.SO_SNDTIMEO timeout;
        Unix.connect fd (Unix.ADDR_UNIX socket_path);
        write_all fd (request_line request);
        match read_line fd with
        | Error _ as error -> error
        | Ok line ->
          match Yojson.Safe.from_string line with
          | json -> Ok json
          | exception Yojson.Json_error message ->
            Error (Printf.sprintf
              "Control API error: invalid JSON response: %s" message))
  with
  | Unix.Unix_error (code, _, _) -> Error (error_of_unix code)
  | End_of_file -> Error "Control API error: connection closed"

let unix ?(socket_path=Control_api.socket_path ()) () =
  make ~request:(request_unix ~socket_path)
