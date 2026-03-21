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
  let home = Sys.getenv "HOME" in
  Filename.concat home ".config/discord-agents/config.json"

let load_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

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
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    Sys.mkdir dir 0o700;
  let json = yojson_of_t config in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  output_char oc '\n';
  close_out oc
