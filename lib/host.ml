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

(* Linux's HOST_NAME_MAX. *)
let max_name_length = 64

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

let is_destination_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
  | '.' | '_' | '-' | '@' | ':' | '%' | '[' | ']' -> true
  | _ -> false

(** SSH destinations become an argument to ssh here, and [writer_ssh]
    later becomes a word in a command line that a remote login shell
    parses. They are allowlisted to what an ssh-config alias,
    [user@host], an IPv4 address or a bracketed IPv6 address with a
    scope id needs, and a leading '-' (which ssh would read as an
    option such as [-oProxyCommand=...]) is refused. The allowlist is
    not quoting: '[' and ']' are glob characters, so any code that puts
    a destination into a shell word must still quote it
    ([Remote_script.quote]). *)
let validate_destination ~field dest =
  if dest = "" then Error (Printf.sprintf "%s must not be empty" field)
  else if dest.[0] = '-' then
    Error (Printf.sprintf "%s %S must not start with '-'" field dest)
  else if not (String.for_all is_destination_char dest) then
    Error (Printf.sprintf
      "%s %S may contain only letters, digits and . _ - @ : %% [ ]" field dest)
  else Ok dest

(** Directories on an executor are always single-quoted before they
    reach a shell; this only refuses values that are wrong whatever the
    quoting: empty, option-like, or containing control characters. *)
let validate_directory ~field dir =
  if String.trim dir = "" then Error (Printf.sprintf "%s: entries must not be empty" field)
  else if dir.[0] = '-' then
    Error (Printf.sprintf "%s: %S must not start with '-'" field dir)
  else if String.exists (fun c -> c < ' ' || c = '\127') dir then
    Error (Printf.sprintf "%s: %S must not contain control characters" field dir)
  else Ok dir

let first_label s =
  match String.index_opt s '.' with
  | Some i when i > 0 -> String.sub s 0 i
  | _ -> s

(** Whether two host names refer to the same machine. Case-insensitive.
    An unqualified name matches a qualified one by its first label
    ([tower0] = [tower0.local]), but two qualified names must match
    exactly: [bot.prod.example] and [bot.dev.example] are different
    machines, and treating them as one would let both pass the
    writer guard. *)
let same_host a b =
  let a = String.lowercase_ascii a and b = String.lowercase_ascii b in
  if String.contains a '.' && String.contains b '.' then String.equal a b
  else String.equal (first_label a) (first_label b)

(** This machine's name as recorded on sessions when no registry entry
    names it: the lowercased hostname, validated so that a name the bot
    writes is always one it can read back. The whole name, not its first
    label: [bot.dev.example] and [bot.prod.example] are different
    machines and must not both be recorded as [bot]. *)
let this_machine_name ?(hostname = Unix.gethostname ()) () =
  match validate_name (String.lowercase_ascii hostname) with
  | Ok name -> Ok name
  | Error msg ->
    Error (Printf.sprintf
      "this machine's hostname %S cannot be used as a host name: %s"
      hostname msg)

let local_host name = {
  name;
  transport = Local;
  writer_ssh = None;
  base_directories = None;
  enabled = true;
}

let single_host_registry name = { writer = name; hosts = [local_host name] }

(** The host [name] refers to: an exact (case-insensitive) match first,
    otherwise the only host [same_host] matches. [None] when nothing
    matches or the name is ambiguous ([tower0] when both [tower0.lan]
    and [tower0.local] are configured): routing by list order would
    pick a machine arbitrarily. *)
type resolution =
  | Found of t
  | Not_found
  | Ambiguous of t list

let resolve hosts name =
  let lower = String.lowercase_ascii name in
  match List.find_opt (fun h ->
          String.equal (String.lowercase_ascii h.name) lower) hosts with
  | Some h -> Found h
  | None ->
    match List.filter (fun h -> same_host h.name name) hosts with
    | [] -> Not_found
    | [h] -> Found h
    | hs -> Ambiguous hs

let lookup hosts name =
  match resolve hosts name with
  | Found h -> Some h
  | Not_found | Ambiguous _ -> None

let find registry name = lookup registry.hosts name

let ambiguous_error ~hostname hs =
  Error (Printf.sprintf
    "this machine's hostname %S matches several configured hosts (%s). \
     Spell hosts.executors keys so that only one matches, or give this \
     machine its fully-qualified hostname."
    hostname (String.concat ", " (List.map (fun h -> h.name) hs)))

let writer registry =
  match find registry registry.writer with
  | Some h -> h
  | None ->
    (* [registry_of_yojson] always inserts the writer; a registry built
       by hand without it is a programming error. *)
    invalid_arg "Host.writer: registry has no entry for its writer"

let is_local h =
  match h.transport with Local -> true | Ssh _ -> false

(** Name comparison only, with no registry: use [resolve] wherever a
    registry exists, so ambiguity is caught. *)
let names_this_machine ?(hostname = Unix.gethostname ()) name =
  same_host name hostname

(** The host name to record on sessions this process creates. It must
    name the machine that actually runs the agents, which is this one:
    the registry's spelling of this machine when the registry has it
    (so lookups by that key work), otherwise this machine's own name.
    Under a forced start that is not the configured writer, and
    stamping the writer would send those sessions back to a machine
    that never ran them. A forced machine may be listed as an SSH
    executor; sessions recorded with this machine's name still run
    locally. Routing must decide "is this session's host this machine"
    with [resolve] against the hostname, as this function and
    [check_writer] do, before looking at the transport. Not with
    [names_this_machine]: plain [same_host] can say yes to a hostname
    that [resolve] reports as ambiguous. *)
let session_host_name ?(hostname = Unix.gethostname ()) registry_opt =
  match registry_opt with
  | None -> this_machine_name ~hostname ()
  | Some r ->
    match resolve r.hosts hostname with
    | Found h -> Ok h.name
    | Not_found -> this_machine_name ~hostname ()
    (* Recording a name that could resolve to another machine is the
       failure this function exists to prevent. *)
    | Ambiguous hs -> ambiguous_error ~hostname hs

let force_writer_env = "DISCORD_AGENTS_FORCE_WRITER"


(** Where a writer override came from, if anywhere. The environment
    variable exists because !restart respawns the bot with no flags;
    main exports it whenever the flag is given. *)
let force_writer_source ~args ~env_value =
  if List.mem "--force-writer" args then Some "--force-writer"
  else
    match env_value with
    | Some v when List.mem (String.lowercase_ascii (String.trim v))
                    ["1"; "true"; "yes"; "on"] ->
      Some (force_writer_env ^ "=" ^ v)
    | _ -> None

(** The startup guard against two writers. Only an explicit [hosts]
    block names a writer; without one, whatever machine runs the bot
    is the writer, as before. [force] names the override's source. *)
let check_writer ?(hostname = Unix.gethostname ()) ?force registry_opt =
  match registry_opt with
  | None -> Ok ()
  | Some registry ->
    let here = hostname in
    (* Resolve this machine the same way sessions are stamped, so the
       guard and the stamp can never disagree about which entry is us. *)
    match resolve registry.hosts hostname with
    | Ambiguous hs -> ambiguous_error ~hostname hs
    | Found h when String.equal h.name (writer registry).name -> Ok ()
    | Found _ | Not_found ->
    match force with
    | Some source ->
      Logs.warn (fun m ->
        m "hosts.writer is %S but this machine is %S; starting anyway \
           because of %s"
          registry.writer here source);
      Ok ()
    | None ->
      Error (Printf.sprintf
        "hosts.writer is %S but this machine is %S. Only the writer may \
         run the bot, or two bots would answer every Discord message. \
         Start the bot on %s, change hosts.writer, or override with \
         --force-writer (or %s=1)."
        registry.writer here registry.writer force_writer_env)

(** Everything about hosts that must hold before this process may act
    as the bot: the writer guard, and a usable name for this machine.
    Returns the name to record on sessions. Used at startup and again
    before !restart shuts anything down, so a respawn that would be
    refused is caught while the old bot is still serving. *)
let startup_check ?hostname ?force registry_opt =
  match check_writer ?hostname ?force registry_opt with
  | Error _ as e -> e
  | Ok () -> session_host_name ?hostname registry_opt

(* JSON. Unknown fields are errors, matching the rest of config.json:
   a typo such as "enabeld" silently doing nothing is worse than a
   refusal to start. *)

let fail fmt = Printf.ksprintf failwith fmt

(* Yojson keeps every duplicate key and [List.assoc_opt] would quietly
   pick the first, so a strict parser has to reject them itself. *)
let check_known ~what ~known fields =
  let seen = Hashtbl.create 8 in
  List.iter (fun (k, _) ->
    if not (List.mem k known) then fail "%s: unknown field %s" what k;
    if Hashtbl.mem seen k then fail "%s: field %s is given twice" what k;
    Hashtbl.add seen k ())
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
          | `String s ->
            ok_or_fail
              (validate_directory ~field:(what ^ ".base_directories") s)
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
        (* No two keys may name the same machine by [same_host]'s rule,
           which is how lookups compare. *)
        let seen = ref [] in
        List.map (fun (name, json) ->
          (match validate_name name with
           | Ok _ -> ()
           | Error msg -> fail "hosts.executors: %s" msg);
          (match List.find_opt (fun other -> same_host other name) !seen with
           | Some other when String.equal other name ->
             fail "hosts.executors: %s is listed twice" name
           | Some other ->
             fail "hosts.executors: %s and %s would name the same host" other name
           | None -> ());
          seen := name :: !seen;
          host_of_yojson name json) entries
      | Some _ -> fail "hosts.executors: expected object keyed by host name"
    in
    let hosts =
      match lookup executors writer with
      | None ->
        (match List.filter (fun h -> same_host h.name writer) executors with
         | [] -> local_host writer :: executors
         | matches ->
           fail "hosts.writer %s matches more than one executor (%s); \
                 spell it exactly as one of them" writer
             (String.concat ", " (List.map (fun h -> h.name) matches)))
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
