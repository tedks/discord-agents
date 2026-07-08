(** Application configuration, loaded from a YAML/JSON file or environment. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type agent_kind =
  | Claude
  | Codex
  | Gemini
[@@deriving show, eq]

type reasoning_effort =
  | Low
  | Medium
  | High
  | Xhigh
  | Max
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

let reasoning_effort_of_string = function
  | "low" -> Ok Low
  | "medium" -> Ok Medium
  | "high" -> Ok High
  | "xhigh" | "extra-high" | "extra_high" -> Ok Xhigh
  | "max" -> Ok Max
  | s -> Error (Printf.sprintf "unknown reasoning effort: %s" s)

let string_of_reasoning_effort = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Xhigh -> "xhigh"
  | Max -> "max"

let reasoning_effort_of_yojson = function
  | `String s ->
    (match reasoning_effort_of_string s with
     | Ok effort -> effort
     | Error msg -> failwith msg)
  | _ -> failwith "reasoning_effort: expected string"

let yojson_of_reasoning_effort effort =
  `String (string_of_reasoning_effort effort)

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
} [@@deriving show, yojson]

let default = {
  discord_token = "";
  base_directories = [];
  guild_id = "";
  control_channel_id = None;
  projects = [];
}

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
  | Some token when token <> "" && config.discord_token = "" ->
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
