(** Durable last-successful project observations. This store is independent of
    the local launch/channel registry and never supplies cleanup authority. *)
module Scan = Host_project_scan

type success = { at : float; projects : Scan.project list }
type entry = {
  host : Host.t;
  roots : string list;
  attempted_at : float;
  error : string option;
  success : success option;
}
type t = entry list
let empty = []
let max_hosts = 64
let max_file_bytes = 8 * 1024 * 1024
let max_entry_bytes = 120 * 1024
let default_max_age = 300.
let filename () = Filename.concat (Resource.app_config_dir ()) "host-projects.json"
let same_context entry host roots = Host.equal entry.host host && entry.roots = roots
let host_key host = String.lowercase_ascii host.Host.name
let find entries host = List.find_opt (fun e -> host_key e.host = host_key host) entries
let prune entries ~configured =
  List.filter (fun e -> List.exists (fun h -> host_key h = host_key e.host) configured) entries
let error_code = function
  | Scan.Invalid_roots -> "invalid roots" | Malformed -> "malformed response"
  | Transport why -> Host_diagnostics.reason why

let project_json (p : Scan.project) = `Assoc [
  "name", `String p.name; "path", `String p.path; "is_bare", `Bool p.is_bare;
  "repository", (match p.repository with None -> `Null | Some s -> `String s)]
let success_json (s : success) = `Assoc [
  "at", `Float s.at; "projects", `List (List.map project_json s.projects)]
let entry_json e = `Assoc [
  "host_name", `String e.host.name; "host", Host.yojson_of_host e.host;
  "roots", `List (List.map (fun s -> `String s) e.roots);
  "attempted_at", `Float e.attempted_at;
  "error", (match e.error with None -> `Null | Some s -> `String s);
  "success", (match e.success with None -> `Null | Some s -> success_json s)]
(* Reserve space for the next timestamp and any bounded failure label, so a
   failed attempt can retain a snapshot that filled its original budget. *)
let fits_entry e =
  String.length (Yojson.Safe.to_string (entry_json {e with attempted_at=0.;error=None}))
    <= max_entry_bytes - 256
let encode entries = Yojson.Safe.to_string (`Assoc [
  "version", `Int 1; "entries", `List (List.map entry_json entries)])
let time = function
  | `Float t when Float.is_finite t && t >= 0. -> t
  | `Int t when t >= 0 -> float_of_int t
  | _ -> raise Exit
let optional_string = function `Null -> None | x -> Some (Scan.string x)
let decode text =
  try
    if String.length text > max_file_bytes then raise Exit;
    let top = Scan.fields ["version"; "entries"] (Yojson.Safe.from_string text) in
    if List.assoc "version" top <> `Int 1 then raise Exit;
    let entries = match List.assoc "entries" top with
      | `List xs when List.length xs <= max_hosts -> xs | _ -> raise Exit in
    let names = Hashtbl.create 16 in
    let entries = List.map (fun json ->
      let xs = Scan.fields ["host_name"; "host"; "roots"; "attempted_at"; "error"; "success"] json in
      let name = Scan.string (List.assoc "host_name" xs) in
      if Result.is_error (Host.validate_name name) || Hashtbl.mem names (String.lowercase_ascii name) then raise Exit;
      Hashtbl.add names (String.lowercase_ascii name) ();
      let host = Host.host_of_yojson name (List.assoc "host" xs) in
      let roots = match List.assoc "roots" xs with `List xs -> List.map Scan.string xs | _ -> raise Exit in
      if not (Scan.valid_roots roots) then raise Exit;
      let attempted_at = time (List.assoc "attempted_at" xs) in
      let error = optional_string (List.assoc "error" xs) in
      (match error with Some s when String.length s > 64 -> raise Exit | _ -> ());
      let success = match List.assoc "success" xs with
        | `Null -> None
        | json ->
          let ss = Scan.fields ["at"; "projects"] json in
          let at = time (List.assoc "at" ss) in
          if at > attempted_at then raise Exit;
          let ps = match List.assoc "projects" ss with
            | `List xs when List.length xs <= Scan.max_projects -> xs | _ -> raise Exit in
          let paths = Hashtbl.create 16 in
          let projects = List.map (fun p ->
            let p = Scan.fields ["name"; "path"; "is_bare"; "repository"] p in
            let name = Scan.string (List.assoc "name" p) and path = Scan.string (List.assoc "path" p) in
            if not (Scan.valid_path path) || Hashtbl.mem paths path then raise Exit;
            Hashtbl.add paths path ();
            let is_bare = match List.assoc "is_bare" p with `Bool b -> b | _ -> raise Exit in
            let repository = optional_string (List.assoc "repository" p) in
            Scan.{ name; path; is_bare; repository }) ps in
          Some { at; projects } in
      (match error, success with
       | None, Some s when s.at = attempted_at -> ()
       | Some _, _ -> () | _ -> raise Exit);
      let entry = { host; roots; attempted_at; error; success } in
      if not (fits_entry entry) then raise Exit;
      entry) entries in
    Ok entries
  with Exit | Failure _ | Yojson.Json_error _ | Invalid_argument _ -> Error "Invalid project inventory cache."

(* Immutable update: a failure retains earlier projects only for the identical
   host/config context. A successful empty scan deliberately replaces them.
   Caller serializes prune/record/save/publication and prunes configured hosts
   first. An oversized scan becomes a bounded failed observation, never empty. *)
let record entries ~now ~host ~roots result =
  if not (Float.is_finite now) || now < 0. || not (Scan.valid_roots roots) then
    Error "Invalid inventory observation context."
  else
    let previous = match find entries host with
      | Some e when same_context e host roots -> e.success | _ -> None in
    (* Clock rollback cannot give older data a newer provenance. *)
    let previous = match previous with Some s when s.at <= now -> previous | _ -> None in
    let error, success = match result with
      | Ok projects -> None, Some { at = now; projects }
      | Error failure -> Some (error_code failure), previous in
    let entry = { host; roots; attempted_at = now; error; success } in
    let entry = if fits_entry entry then entry else
      {entry with error=Some "snapshot byte limit exceeded"; success=previous} in
    let entries = entry :: List.filter (fun e -> host_key e.host <> host_key host) entries in
    if not (fits_entry entry) then Error "Project inventory host metadata exceeds the entry limit."
    else if List.length entries > max_hosts then Error "Project inventory host limit exceeded."
    else Ok entries

let load path =
  try
    let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0 in
    let ic = match Unix.in_channel_of_descr fd with
      | ic -> ic | exception exn -> Unix.close fd; raise exn in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      let stat = Unix.fstat fd in
      if stat.st_kind <> Unix.S_REG || stat.st_size > max_file_bytes then
        Error "Project inventory cache is not a bounded regular file."
      else
        let text = really_input_string ic stat.st_size in
        match input_char ic with
        | exception End_of_file -> decode text
        | _ -> Error "Project inventory cache changed while reading.")
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok empty
  | End_of_file | Sys_error _ | Unix.Unix_error _ -> Error "Project inventory cache could not be read."

(* Caller publishes the returned immutable state only after save succeeds.
   Even visible-but-unconfirmed writes remain errors; this cache is recoverable
   observational data, never a prerequisite for session or process ownership. *)
let save ?(write = fun path text -> Resource.write_file_atomic path text) path entries =
  let text = encode entries in
  if String.length text > max_file_bytes then Error "Project inventory cache exceeds the size limit."
  else match decode text with
    | Error _ as e -> e
    | Ok _ ->
      try write path text; Ok () with
      | Resource.Durable_write_visible_but_unconfirmed _ -> Error "Project inventory write is visible but durability is unconfirmed."
      | Failure _ | Sys_error _ | Unix.Unix_error _ -> Error "Project inventory cache could not be saved."

type view = {
  state : string;
  attempted_at : float option;
  last_success_at : float option;
  projects : Scan.project list;
  error : string option;
}
let view ?(max_age=default_max_age) entries ~now ~host ~roots =
  let unknown state = {state; attempted_at=None; last_success_at=None; projects=[]; error=None} in
  if not host.Host.enabled then unknown "disabled"
  else match find entries host with
    | None -> unknown "unknown"
    | Some e when not (same_context e host roots) -> unknown "configuration_changed"
    | Some e ->
      let state =
        if not (Float.is_finite now && Float.is_finite max_age) || max_age < 0. || now < e.attempted_at then "stale"
        else if now -. e.attempted_at > max_age then "stale"
        else if Option.is_some e.error then "unavailable" else "observed" in
      { state; attempted_at=Some e.attempted_at;
        last_success_at=Option.map (fun (s:success) -> s.at) e.success;
        projects=(match e.success with None -> [] | Some s -> s.projects); error=e.error }
