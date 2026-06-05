(** Mutable runtime settings that are safe to change from Discord.

    These live separately from [config.json] so operational commands
    do not rewrite credentials or project discovery config. *)

type t = {
  mutable default_agent : Config.agent_kind;
}

let settings_path () =
  Filename.concat (Resource.app_config_dir ()) "settings.json"

let lock_path () = settings_path () ^ ".lock"
let backup_path () = settings_path () ^ ".bak"

let default () = {
  default_agent = Config.Claude;
}

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

let load_file path =
  let contents = Resource.read_file path in
  of_yojson (Yojson.Safe.from_string contents)

let load () =
  let path = settings_path () in
  let backup = backup_path () in
  match Sys.file_exists path, Sys.file_exists backup with
  | false, false -> default ()
  | false, true ->
    (match load_file backup with
     | settings ->
       Logs.warn (fun m ->
         m "runtime_settings: primary missing; recovered from backup %s" backup);
       settings
     | exception backup_exn ->
       Logs.warn (fun m ->
         m "runtime_settings: backup load error from %s: %s"
           backup (Printexc.to_string backup_exn));
       default ())
  | true, _ ->
    match load_file path with
    | settings -> settings
    | exception exn ->
      Logs.warn (fun m ->
        m "runtime_settings: load error from %s: %s"
          path (Printexc.to_string exn));
      (match load_file backup with
       | settings ->
         Logs.warn (fun m ->
           m "runtime_settings: recovered from backup %s" backup);
         settings
       | exception backup_exn ->
         Logs.warn (fun m ->
           m "runtime_settings: backup load error from %s: %s"
             backup (Printexc.to_string backup_exn));
        default ())

let log_visible_but_unconfirmed path exn =
  Logs.warn (fun m ->
    m "runtime_settings: write to %s is visible but durability could not be confirmed: %s"
      path (Printexc.to_string exn))

let save_with
    ?(preflight_write=Disk_health.preflight_write)
    ?(note_write_success=Disk_health.note_write_success)
    ?(note_write_failure=Disk_health.note_write_failure)
    ~write_file t =
  let path = settings_path () in
  let backup = backup_path () in
  let rendered = Yojson.Safe.pretty_to_string (to_yojson t) in
  let primary_warning = ref None in
  match preflight_write path with
  | Error err -> Error err
  | Ok () ->
    let saw_disk_issue = ref false in
    (try
       Resource.with_flock (lock_path ()) (fun () ->
         Resource.cleanup_atomic_write_temps path;
         Resource.cleanup_atomic_write_temps backup;
         (try write_file path rendered with
          | Resource.Durable_write_visible_but_unconfirmed (path, exn) ->
            saw_disk_issue := true;
            note_write_failure path exn;
            primary_warning := Some (path, exn));
         (try write_file backup rendered with
          | Resource.Durable_write_visible_but_unconfirmed (path, exn) ->
            saw_disk_issue := true;
            note_write_failure path exn;
            log_visible_but_unconfirmed path exn
          | exn ->
            note_write_failure backup exn;
            Logs.warn (fun m ->
              m "runtime_settings: failed to update backup %s: %s"
                backup (Printexc.to_string exn))));
       Option.iter (fun (path, exn) ->
         log_visible_but_unconfirmed path exn) !primary_warning;
       if not !saw_disk_issue then
         note_write_success path;
       Ok ()
     with exn ->
       note_write_failure path exn;
       Error (Printexc.to_string exn))

let save t =
  save_with ~write_file:(fun path rendered ->
    Resource.write_file_atomic path rendered) t

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
