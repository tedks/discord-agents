(** Mutable runtime settings that are safe to change from Discord.

    These live separately from [config.json] so operational commands
    do not rewrite credentials or project discovery config. *)

type t = {
  mutable default_agent : Config.agent_kind;
}

let settings_path () =
  let home = Sys.getenv "HOME" in
  Filename.concat home ".config/discord-agents/settings.json"

let lock_path () = settings_path () ^ ".lock"

let default () = {
  default_agent = Config.Claude;
}

let ensure_parent_dir path =
  let rec mkdir_p dir =
    if not (Sys.file_exists dir) then begin
      let parent = Filename.dirname dir in
      if parent <> dir then mkdir_p parent;
      Unix.mkdir dir 0o700
    end
  in
  mkdir_p (Filename.dirname path)

let to_yojson t =
  `Assoc [("default_agent", Config.yojson_of_agent_kind t.default_agent)]

let of_yojson = function
  | `Assoc fields ->
    (match List.assoc_opt "default_agent" fields with
     | Some (`String _ as json) ->
       { default_agent = Config.agent_kind_of_yojson json }
     | Some _ -> failwith "default_agent: expected string"
     | None -> default ())
  | _ -> failwith "runtime settings: expected object"

let load () =
  let path = settings_path () in
  if not (Sys.file_exists path) then default ()
  else
    try
      let contents = Resource.read_file path in
      of_yojson (Yojson.Safe.from_string contents)
    with exn ->
      Logs.warn (fun m ->
        m "runtime_settings: load error: %s" (Printexc.to_string exn));
      default ()

let save t =
  let path = settings_path () in
  try
    ensure_parent_dir path;
    Resource.with_flock (lock_path ()) (fun () ->
      Resource.write_file_atomic path (Yojson.Safe.pretty_to_string (to_yojson t)));
    Ok ()
  with exn ->
    Error (Printexc.to_string exn)

let set_default_agent t agent =
  if Config.equal_agent_kind t.default_agent agent then
    Ok ()
  else
    let next = { default_agent = agent } in
    match save next with
    | Ok () as ok ->
      t.default_agent <- agent;
      ok
    | Error _ as err ->
      err
