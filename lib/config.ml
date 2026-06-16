(** Application configuration, loaded from a YAML/JSON file or environment. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type agent_kind =
  | Claude
  | Codex
  | Gemini
[@@deriving show, eq]

let agent_kind_of_string = function
  | "claude" -> Ok Claude
  | "codex" -> Ok Codex
  | "gemini" -> Ok Gemini
  | s -> Error (Printf.sprintf "unknown agent kind: %s" s)

let string_of_agent_kind = function
  | Claude -> "claude"
  | Codex -> "codex"
  | Gemini -> "gemini"

let preferred_agent_order preferred =
  preferred
  :: List.filter (fun kind -> not (equal_agent_kind kind preferred))
       [Claude; Codex; Gemini]

let find_with_preferred_agent preferred f =
  let rec first_found = function
    | [] -> None
    | kind :: rest ->
      match f kind with
      | Some _ as found -> found
      | None -> first_found rest
  in
  first_found (preferred_agent_order preferred)

(** Whether this agent accepts a caller-supplied session id at startup
    (Claude's [--session-id]) or allocates its own server-side and
    emits it on first run (Codex's [thread.started], Gemini's [init]).

    [Session_store.session_id_confirmed] defaults from this:
    caller-pinned ids are confirmed at creation; server-allocated ids
    start unconfirmed until the parser sees the first event. *)
let caller_pinned_session_id = function
  | Claude -> true
  | Codex | Gemini -> false

let agent_kind_of_yojson = function
  | `String s ->
    (match agent_kind_of_string s with
     | Ok k -> k
     | Error msg -> failwith msg)
  | _ -> failwith "agent_kind: expected string"

let yojson_of_agent_kind k = `String (string_of_agent_kind k)

type project = {
  name : string;
  path : string; (** Absolute path to the project's bare repo or working directory *)
  channel_id : string option; (** Discord channel ID, populated once created *)
} [@@deriving show, yojson]

type t = {
  discord_token : string;
  base_directories : string list; (** Directories to scan for projects (e.g. ~/Projects) *)
  guild_id : string; (** Discord server/guild to operate in *)
  control_channel_id : string option; (** Top-level channel for server-wide commands *)
  projects : project list;
} [@@deriving show]

let default = {
  discord_token = "";
  base_directories = [];
  guild_id = "";
  control_channel_id = None;
  projects = [];
}

let field fields name = List.assoc_opt name fields

let string_field ~name = function
  | `String s -> s
  | _ -> failwith (Printf.sprintf "%s: expected string" name)

let optional_string_field ~name = function
  | `Null -> None
  | `String s -> Some s
  | _ -> failwith (Printf.sprintf "%s: expected string or null" name)

let string_list_field ~name = function
  | `List xs -> List.map (string_field ~name) xs
  | _ -> failwith (Printf.sprintf "%s: expected array of strings" name)

let project_list_field ~name = function
  | `List xs -> List.map project_of_yojson xs
  | _ -> failwith (Printf.sprintf "%s: expected array of project objects" name)

let t_of_yojson = function
  | `Assoc fields ->
    let discord_token =
      match field fields "discord_token" with
      | Some json -> string_field ~name:"discord_token" json
      | None -> default.discord_token
    in
    let base_directories =
      match field fields "base_directories", field fields "base_dirs" with
      | Some json, _ -> string_list_field ~name:"base_directories" json
      | None, Some json -> string_list_field ~name:"base_dirs" json
      | None, None -> default.base_directories
    in
    let guild_id =
      match field fields "guild_id" with
      | Some json -> string_field ~name:"guild_id" json
      | None -> default.guild_id
    in
    let control_channel_id =
      match field fields "control_channel_id" with
      | Some json -> optional_string_field ~name:"control_channel_id" json
      | None -> default.control_channel_id
    in
    let projects =
      match field fields "projects" with
      | Some json -> project_list_field ~name:"projects" json
      | None -> default.projects
    in
    { discord_token; base_directories; guild_id; control_channel_id; projects }
  | _ -> failwith "config: expected object"

let yojson_of_t t =
  `Assoc [
    ("discord_token", `String t.discord_token);
    ("base_directories",
     `List (List.map (fun path -> `String path) t.base_directories));
    ("guild_id", `String t.guild_id);
    ("control_channel_id",
     (match t.control_channel_id with
      | Some id -> `String id
      | None -> `Null));
    ("projects", `List (List.map yojson_of_project t.projects));
  ]

let blank s = String.trim s = ""

let validation_errors ?(require_guild_id=true) config =
  let errors = ref [] in
  let add msg = errors := msg :: !errors in
  if blank config.discord_token then
    add "discord_token is required, unless DISCORD_BOT_TOKEN is set";
  if require_guild_id && blank config.guild_id then
    add "guild_id is required";
  List.iteri (fun i path ->
    if blank path then
      add (Printf.sprintf "base_directories[%d] must not be empty" i)
  ) config.base_directories;
  List.iteri (fun i project ->
    if blank project.name then
      add (Printf.sprintf "projects[%d].name must not be empty" i);
    if blank project.path then
      add (Printf.sprintf "projects[%d].path must not be empty" i)
  ) config.projects;
  List.rev !errors

let validate ?require_guild_id config =
  match validation_errors ?require_guild_id config with
  | [] -> Ok ()
  | errors -> Error errors

let config_path () =
  Filename.concat (Resource.app_config_dir ()) "config.json"

let load_file path =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    Bytes.to_string s)

let load () =
  let path = config_path () in
  let config =
    if Sys.file_exists path then
      let contents = load_file path in
      let json = Yojson.Safe.from_string contents in
      t_of_yojson json
    else
      default
  in
  (* Allow env var override for the token *)
  match Sys.getenv_opt "DISCORD_BOT_TOKEN" with
  | Some token when not (blank token) && blank config.discord_token ->
    { config with discord_token = token }
  | _ -> config

let save config =
  let path = config_path () in
  match Disk_health.preflight_write path with
  | Error err -> failwith err
  | Ok () ->
    let json = yojson_of_t config in
    try
      Resource.with_flock (path ^ ".lock") (fun () ->
        Resource.cleanup_atomic_write_temps path;
        Resource.write_file_atomic path (Yojson.Safe.pretty_to_string json));
      Disk_health.note_write_success path
    with exn ->
      Disk_health.note_write_failure path exn;
      raise exn
