(** Hosts that can run agent sessions.

    One bot process, the writer, owns the Discord gateway and the
    session store. Agents can run on the writer itself or on other
    executor hosts reached over SSH (see docs/multi-host.md).

    This module only describes the hosts. Nothing here runs commands
    on them. The registry comes from the optional [hosts] block in
    config.json; when that block is absent the registry is a single
    local host named after this machine, which is exactly the
    single-host behaviour that predates multi-host support. *)

type transport =
  | Local
  | Ssh of string  (** SSH destination, e.g. an ~/.ssh/config alias *)
[@@deriving show, eq]

type t = {
  name : string;
  transport : transport;
  writer_ssh : string option;
  (** Destination the executor uses to reach the writer for the MCP
      callback. [None] means the writer's name. *)
  base_directories : string list option;
  (** Where to look for projects on this host. [None] means the
      top-level [base_directories]. *)
  enabled : bool;
}
[@@deriving show, eq]

type registry = {
  writer : string;
  hosts : t list;  (** Always contains the writer, as a [Local] host. *)
}
[@@deriving show, eq]

let max_name_length = 63

let is_name_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
  | _ -> false

(** Host names appear in sessions.json, Discord messages and (later)
    in [project@host] command tokens, so they are restricted to a
    character set that is inert in all of those. *)
let validate_name name =
  if name = "" then Error "host name must not be empty"
  else if String.length name > max_name_length then
    Error (Printf.sprintf "host name %S is longer than %d characters"
      name max_name_length)
  else if not (String.for_all is_name_char name) then
    Error (Printf.sprintf
      "host name %S may contain only letters, digits, '.', '_' and '-'" name)
  else if name.[0] = '-' || name.[0] = '.' then
    Error (Printf.sprintf "host name %S must not start with '-' or '.'" name)
  else Ok name

(** SSH destinations are passed to [ssh] as an argument. One that
    starts with '-' would be parsed as an option (for example
    [-oProxyCommand=...]), so it is rejected outright, as is anything
    with whitespace or control characters. *)
let validate_destination ~field dest =
  if dest = "" then Error (Printf.sprintf "%s must not be empty" field)
  else if dest.[0] = '-' then
    Error (Printf.sprintf "%s %S must not start with '-'" field dest)
  else if String.exists (fun c -> c <= ' ' || c = '\127') dest then
    Error (Printf.sprintf
      "%s %S must not contain whitespace or control characters" field dest)
  else Ok dest

(** This machine's name, shortened to the part before the first dot so
    that [tower0] and [tower0.local] compare equal. *)
let local_hostname () =
  let full = Unix.gethostname () in
  match String.index_opt full '.' with
  | Some i when i > 0 -> String.sub full 0 i
  | _ -> full

let local_host name = {
  name;
  transport = Local;
  writer_ssh = None;
  base_directories = None;
  enabled = true;
}

let single_host_registry name = { writer = name; hosts = [local_host name] }

let default_registry () = single_host_registry (local_hostname ())

let find registry name =
  List.find_opt (fun h -> String.equal h.name name) registry.hosts

let writer registry =
  match find registry registry.writer with
  | Some h -> h
  | None ->
    (* [registry_of_yojson] always inserts the writer; a registry built
       by hand without it is a programming error. *)
    invalid_arg "Host.writer: registry has no entry for its writer"

let is_local h =
  match h.transport with Local -> true | Ssh _ -> false

(** Whether [name] refers to this machine, accepting the short or the
    fully-qualified form on either side. *)
let names_this_machine ?(hostname = Unix.gethostname ()) name =
  let short s =
    match String.index_opt s '.' with
    | Some i when i > 0 -> String.sub s 0 i
    | _ -> s
  in
  String.equal name hostname
  || String.equal (short name) (short hostname)

(** The startup guard against two writers. Only an explicit [hosts]
    block names a writer; without one, whatever machine runs the bot
    is the writer, as before. *)
let check_writer ?hostname ~force registry_opt =
  match registry_opt with
  | None -> Ok ()
  | Some registry when names_this_machine ?hostname registry.writer -> Ok ()
  | Some registry ->
    let here = Option.value hostname ~default:(Unix.gethostname ()) in
    if force then begin
      Logs.warn (fun m ->
        m "hosts.writer is %S but this machine is %S; starting anyway \
           because --force-writer was given"
          registry.writer here);
      Ok ()
    end else
      Error (Printf.sprintf
        "hosts.writer is %S but this machine is %S. Only the writer may \
         run the bot, or two bots would answer every Discord message. \
         Start the bot on %s, change hosts.writer, or pass --force-writer."
        registry.writer here registry.writer)

(* JSON. Unknown fields are errors, matching the rest of config.json:
   a typo such as "enabeld" silently doing nothing is worse than a
   refusal to start. *)

let fail fmt = Printf.ksprintf failwith fmt

let check_known ~what ~known fields =
  List.iter (fun (k, _) ->
    if not (List.mem k known) then fail "%s: unknown field %s" what k)
    fields

let ok_or_fail = function Ok v -> v | Error msg -> failwith msg

let string_opt ~what = function
  | None | Some `Null -> None
  | Some (`String s) -> Some s
  | Some _ -> fail "%s: expected string" what

let host_of_yojson name = function
  | `Assoc fields ->
    let what = Printf.sprintf "hosts.executors.%s" name in
    check_known ~what fields
      ~known:["local"; "ssh"; "writer_ssh"; "base_directories"; "enabled"];
    let field k = List.assoc_opt k fields in
    let local =
      match field "local" with
      | None | Some `Null -> false
      | Some (`Bool b) -> b
      | Some _ -> fail "%s.local: expected boolean" what
    in
    let ssh = string_opt ~what:(what ^ ".ssh") (field "ssh") in
    let transport =
      match local, ssh with
      | true, Some _ -> fail "%s: a local host must not set ssh" what
      | true, None -> Local
      | false, Some dest ->
        Ssh (ok_or_fail (validate_destination ~field:(what ^ ".ssh") dest))
      | false, None ->
        fail "%s: set \"ssh\" to an SSH destination, or \"local\": true" what
    in
    let writer_ssh =
      string_opt ~what:(what ^ ".writer_ssh") (field "writer_ssh")
      |> Option.map (fun d ->
        ok_or_fail (validate_destination ~field:(what ^ ".writer_ssh") d))
    in
    let base_directories =
      match field "base_directories" with
      | None | Some `Null -> None
      | Some (`List xs) ->
        Some (List.map (function
          | `String s when String.trim s <> "" -> s
          | `String _ -> fail "%s.base_directories: entries must not be empty" what
          | _ -> fail "%s.base_directories: expected array of strings" what) xs)
      | Some _ -> fail "%s.base_directories: expected array of strings" what
    in
    let enabled =
      match field "enabled" with
      | None | Some `Null -> true
      | Some (`Bool b) -> b
      | Some _ -> fail "%s.enabled: expected boolean" what
    in
    { name; transport; writer_ssh; base_directories; enabled }
  | _ -> fail "hosts.executors.%s: expected object" name

let registry_of_yojson = function
  | `Assoc fields ->
    check_known ~what:"hosts" ~known:["writer"; "executors"] fields;
    let writer =
      match List.assoc_opt "writer" fields with
      | Some (`String s) ->
        (match validate_name s with
         | Ok s -> s
         | Error msg -> fail "hosts.writer: %s" msg)
      | Some _ -> fail "hosts.writer: expected string"
      | None -> fail "hosts.writer is required when hosts is set"
    in
    let executors =
      match List.assoc_opt "executors" fields with
      | None | Some `Null -> []
      | Some (`Assoc entries) ->
        let seen = Hashtbl.create 8 in
        List.map (fun (name, json) ->
          (match validate_name name with
           | Ok _ -> ()
           | Error msg -> fail "hosts.executors: %s" msg);
          if Hashtbl.mem seen name then
            fail "hosts.executors: %s is listed twice" name;
          Hashtbl.add seen name ();
          host_of_yojson name json) entries
      | Some _ -> fail "hosts.executors: expected object keyed by host name"
    in
    let hosts =
      match List.find_opt (fun h -> String.equal h.name writer) executors with
      | None -> local_host writer :: executors
      | Some h ->
        if not (is_local h) then
          fail "hosts.executors.%s: the writer runs agents locally; \
                set \"local\": true and remove \"ssh\"" writer;
        if not h.enabled then
          fail "hosts.executors.%s: the writer cannot be disabled" writer;
        executors
    in
    (match List.filter is_local hosts with
     | [_] -> ()
     | locals ->
       fail "hosts: only the writer (%s) may be local, but %s are" writer
         (String.concat ", " (List.map (fun h -> h.name) locals)));
    { writer; hosts }
  | _ -> failwith "hosts: expected object"

let yojson_of_host h =
  let fields =
    (match h.transport with
     | Local -> [("local", `Bool true)]
     | Ssh dest -> [("ssh", `String dest)])
    @ (match h.writer_ssh with
       | Some d -> [("writer_ssh", `String d)]
       | None -> [])
    @ (match h.base_directories with
       | Some dirs -> [("base_directories", `List (List.map (fun d -> `String d) dirs))]
       | None -> [])
    @ (if h.enabled then [] else [("enabled", `Bool false)])
  in
  `Assoc fields

let yojson_of_registry r =
  `Assoc [
    ("writer", `String r.writer);
    ("executors", `Assoc (List.map (fun h -> (h.name, yojson_of_host h)) r.hosts));
  ]
