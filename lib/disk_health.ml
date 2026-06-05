(** Shared disk-pressure state for durable bot metadata writes.

    The bot writes critical state under the app config directory, but
    generated agent config can land inside project worktrees. Disk
    pressure therefore has to be tracked per filesystem path: a healthy
    app-config probe must not erase a read-only project-worktree failure. *)

type mode =
  | Healthy
  | Warning
  | Read_only

type snapshot = {
  mode : mode;
  available_bytes : int64 option;
  warning_threshold_bytes : int64;
  read_only_threshold_bytes : int64;
  last_error : string option;
  checked_path : string option;
  checked_at : float option;
}

type write_failure = {
  code : string;
  summary : string;
}

type observation = {
  path : string;
  mode : mode;
  available_bytes : int64 option;
  last_error : string option;
  checked_at : float;
  sticky_until_write_success : bool;
}

type probe_error = {
  probe_path : string;
  probe_error : string;
  probe_checked_at : float;
}

type state = {
  observations : observation list;
  last_probe_error : probe_error option;
}

external statvfs_available_bytes : string -> int64 =
  "discord_agents_available_bytes"

let mib n =
  Int64.mul (Int64.of_int n) (Int64.mul 1024L 1024L)

let warning_threshold_bytes = mib 128
let read_only_threshold_bytes = mib 64

let state_mu = Mutex.create ()
let probe_available_bytes_override = ref None

let state = ref {
  observations = [];
  last_probe_error = None;
}

let with_state f =
  Mutex.lock state_mu;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock state_mu)
    (fun () -> f state)

let string_of_mode = function
  | Healthy -> "healthy"
  | Warning -> "warning"
  | Read_only -> "read_only"

let severity = function
  | Healthy -> 0
  | Warning -> 1
  | Read_only -> 2

let pressure (snapshot : snapshot) =
  match snapshot.mode with
  | Healthy -> false
  | Warning | Read_only -> true

let human_bytes bytes =
  let f = Int64.to_float bytes in
  let kib = 1024.0 in
  let mib = kib *. 1024.0 in
  let gib = mib *. 1024.0 in
  if f >= gib then Printf.sprintf "%.1f GiB" (f /. gib)
  else if f >= mib then Printf.sprintf "%.1f MiB" (f /. mib)
  else if f >= kib then Printf.sprintf "%.1f KiB" (f /. kib)
  else Printf.sprintf "%Ld B" bytes

let mode_of_available_bytes available_bytes =
  if Int64.compare available_bytes read_only_threshold_bytes <= 0 then
    Read_only
  else if Int64.compare available_bytes warning_threshold_bytes <= 0 then
    Warning
  else
    Healthy

let low_space_summary ~path ~available_bytes ~mode =
  let available = human_bytes available_bytes in
  match mode with
  | Healthy ->
    Printf.sprintf "disk healthy near %s: %s free" path available
  | Warning ->
    Printf.sprintf
      "disk pressure near %s: %s free (warning threshold %s)"
      path available (human_bytes warning_threshold_bytes)
  | Read_only ->
    Printf.sprintf
      "disk pressure near %s: %s free (read-only threshold %s)"
      path available (human_bytes read_only_threshold_bytes)

let read_only_message ~path ~available_bytes =
  Printf.sprintf
    "Bot is in read-only mode due to disk pressure near `%s`: only %s free. Free space and retry."
    path (human_bytes available_bytes)

let rec existing_ancestor path =
  if Sys.file_exists path then
    path
  else
    let parent = Filename.dirname path in
    if parent = path then
      path
    else
      existing_ancestor parent

let probe_target path =
  let candidate =
    match Sys.file_exists path with
    | true when Sys.is_directory path -> path
    | _ -> Filename.dirname path
  in
  existing_ancestor candidate

let replace_observation observations observation =
  observation
  :: List.filter (fun prior -> prior.path <> observation.path) observations

let drop_observation observations path =
  List.filter (fun prior -> prior.path <> path) observations

let rec path_is_ancestor ~ancestor path =
  ancestor = path
  || (let parent = Filename.dirname path in
      parent <> path && path_is_ancestor ~ancestor parent)

let clear_sticky_observations_for_success path =
  with_state (fun state ->
    state := {
      !state with
      observations =
        List.filter (fun obs ->
          not (obs.sticky_until_write_success
               && path_is_ancestor ~ancestor:obs.path path))
          !state.observations;
    })

let representative observations =
  let better a b =
    let severity_cmp = compare (severity a.mode) (severity b.mode) in
    if severity_cmp <> 0 then severity_cmp > 0
    else a.checked_at >= b.checked_at
  in
  List.fold_left
    (fun acc obs ->
       match acc with
       | None -> Some obs
       | Some current when better obs current -> Some obs
       | Some _ -> acc)
    None observations

let snapshot_of_state state =
  match representative state.observations with
  | Some obs ->
    let last_error =
      match obs.mode, obs.last_error, state.last_probe_error with
      | Healthy, None, Some err -> Some err.probe_error
      | _ -> obs.last_error
    in
    let checked_path, checked_at =
      match obs.mode, state.last_probe_error with
      | Healthy, Some err
        when err.probe_checked_at > obs.checked_at ->
        Some err.probe_path, Some err.probe_checked_at
      | _ -> Some obs.path, Some obs.checked_at
    in
    {
      mode = obs.mode;
      available_bytes = obs.available_bytes;
      warning_threshold_bytes;
      read_only_threshold_bytes;
      last_error;
      checked_path;
      checked_at;
    }
  | None ->
    {
      mode = Healthy;
      available_bytes = None;
      warning_threshold_bytes;
      read_only_threshold_bytes;
      last_error =
        Option.map (fun err -> err.probe_error) state.last_probe_error;
      checked_path =
        Option.map (fun err -> err.probe_path) state.last_probe_error;
      checked_at =
        Option.map (fun err -> err.probe_checked_at) state.last_probe_error;
    }

let snapshot () =
  with_state (fun state -> snapshot_of_state !state)

let is_read_only () =
  match (snapshot ()).mode with
  | Read_only -> true
  | Healthy | Warning -> false

let reset_for_tests () =
  with_state (fun state ->
    probe_available_bytes_override := None;
    state := {
      observations = [];
      last_probe_error = None;
    })

let set_probe_available_bytes_for_tests f =
  with_state (fun _state ->
    probe_available_bytes_override := Some f)

let probe_available_bytes path =
  match !probe_available_bytes_override with
  | Some probe -> probe path
  | None -> statvfs_available_bytes path

let update_from_available_bytes ?(force = false) ~path available_bytes =
  let mode = mode_of_available_bytes available_bytes in
  let last_error =
    match mode with
    | Healthy -> None
    | Warning | Read_only ->
      Some (low_space_summary ~path ~available_bytes ~mode)
  in
  let checked_at = Unix.gettimeofday () in
  with_state (fun state ->
    let next =
      { path; mode; available_bytes = Some available_bytes;
        last_error; checked_at; sticky_until_write_success = false }
    in
    let keep_sticky =
      (not force)
      && mode <> Read_only
      && List.exists (fun prior ->
        prior.path = path && prior.sticky_until_write_success)
        !state.observations
    in
    state := {
      observations =
        if keep_sticky then
          !state.observations
        else
          replace_observation !state.observations next;
      last_probe_error = None;
    });
  match mode with
  | Healthy | Warning -> Ok ()
  | Read_only -> Error (read_only_message ~path ~available_bytes)

let note_probe_failure ~path exn =
  let err =
    Printf.sprintf "disk probe failed near %s: %s"
      path (Printexc.to_string exn)
  in
  with_state (fun state ->
    state := {
      !state with
      last_probe_error =
        Some {
          probe_path = path;
          probe_error = err;
          probe_checked_at = Unix.gettimeofday ();
        };
    })

let preflight_path_with ~available_bytes_of_path path =
  let requested_path = path in
  let path = probe_target path in
  if path <> requested_path then
    with_state (fun state ->
      state := {
        !state with
        observations = drop_observation !state.observations requested_path;
      });
  try
    let available_bytes = available_bytes_of_path path in
    update_from_available_bytes ~path available_bytes
  with exn ->
    note_probe_failure ~path exn;
    Logs.warn (fun m ->
      m "disk_health: free-space probe failed for %s: %s"
        path (Printexc.to_string exn));
    Ok ()

let preflight_path path =
  preflight_path_with ~available_bytes_of_path:probe_available_bytes path

let preflight_write path =
  preflight_path path

let tracked_pressure_paths () =
  with_state (fun state ->
    !state.observations
    |> List.filter_map (fun obs ->
      match obs.mode with
      | Healthy -> None
      | Warning | Read_only -> Some obs.path))

let refresh_tracked_pressure_paths () =
  tracked_pressure_paths ()
  |> List.iter (fun path -> ignore (preflight_path path))

let status_summary () =
  let state = snapshot () in
  match state.mode, state.available_bytes, state.last_error with
  | Healthy, Some available, _ ->
    Printf.sprintf "Disk: healthy (%s free)." (human_bytes available)
  | Healthy, None, Some err ->
    Printf.sprintf "Disk: probe unavailable (%s)." err
  | Healthy, None, _ ->
    "Disk: healthy."
  | Warning, Some available, _ ->
    Printf.sprintf
      "Disk: pressure warning (%s free; warning threshold %s)."
      (human_bytes available) (human_bytes state.warning_threshold_bytes)
  | Warning, None, Some err ->
    Printf.sprintf "Disk: pressure warning (%s)." err
  | Warning, None, None ->
    "Disk: pressure warning."
  | Read_only, Some available, Some err ->
    Printf.sprintf "Disk: read-only (%s free). %s"
      (human_bytes available) err
  | Read_only, _, Some err ->
    Printf.sprintf "Disk: read-only. %s" err
  | Read_only, Some available, None ->
    Printf.sprintf "Disk: read-only (%s free)." (human_bytes available)
  | Read_only, None, None ->
    "Disk: read-only."

let preflight_state_mutation () =
  match preflight_path (Resource.app_config_dir ()) with
  | Error _ as err -> err
  | Ok () ->
    refresh_tracked_pressure_paths ();
    if is_read_only () then
      Error (status_summary () ^ " Free space and retry.")
    else
      Ok ()

let string_contains s needle =
  let s_len = String.length s in
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > s_len then false
    else if String.sub s i needle_len = needle then true
    else loop (i + 1)
  in
  needle_len = 0 || loop 0

let classify_sys_error msg =
  let lower = String.lowercase_ascii msg in
  if string_contains lower "no space left on device" then
    Some { code = "ENOSPC"; summary = msg }
  else if string_contains lower "disk quota exceeded"
          || string_contains lower "quota exceeded" then
    Some { code = "EDQUOT"; summary = msg }
  else if string_contains lower "read-only file system" then
    Some { code = "EROFS"; summary = msg }
  else
    None

let unix_error_is_quota = function
  | Unix.EUNKNOWNERR 122 -> true
  | _ -> false

let rec classify_write_failure exn =
  match exn with
  | Resource.Durable_write_visible_but_unconfirmed (_, inner) ->
    classify_write_failure inner
  | Unix.Unix_error (code, _fn, path) ->
    let code_message = Unix.error_message code in
    let lower_message = String.lowercase_ascii code_message in
    let tracked =
      if code = Unix.ENOSPC then Some ("ENOSPC", code_message)
      else if code = Unix.EROFS then Some ("EROFS", code_message)
      else if code = Unix.EIO then Some ("EIO", code_message)
      else if unix_error_is_quota code
              || string_contains lower_message "quota" then
        Some ("EDQUOT", code_message)
      else
        None
    in
    Option.map (fun (label, unix_code) ->
      let target = if path = "" then "<unknown>" else path in
      {
        code = label;
        summary =
          Printf.sprintf "%s while writing %s: %s"
            label target unix_code;
      }) tracked
  | Failure msg when String.equal msg "resource: short write" ->
    Some {
      code = "PARTIAL_WRITE";
      summary = "partial write while updating durable state";
    }
  | Sys_error msg ->
    classify_sys_error msg
  | _ ->
    None

let note_write_failure path exn =
  match classify_write_failure exn with
  | None -> ()
  | Some failure ->
    let target = probe_target path in
    let available_bytes =
      try Some (probe_available_bytes target)
      with _ -> None
    in
    let checked_at = Unix.gettimeofday () in
    with_state (fun state ->
      state := {
        observations =
          replace_observation !state.observations
            { path = target; mode = Read_only; available_bytes;
              last_error = Some failure.summary; checked_at;
              sticky_until_write_success = true };
        last_probe_error = !state.last_probe_error;
      })

let note_write_success path =
  let path = probe_target path in
  clear_sticky_observations_for_success path;
  try
    let available_bytes = probe_available_bytes path in
    ignore (update_from_available_bytes ~force:true ~path available_bytes)
  with exn ->
    note_probe_failure ~path exn;
    Logs.warn (fun m ->
      m "disk_health: free-space probe failed for %s after write success: %s"
        path (Printexc.to_string exn))

let new_session_block_message ?(preflight=preflight_state_mutation) () =
  match preflight () with
  | Error err -> Some err
  | Ok () when is_read_only () ->
    Some (status_summary () ^ " Free space and retry.")
  | Ok () ->
    None

module For_testing = struct
  let reset = reset_for_tests
  let mib = mib
  let mode_of_available_bytes = mode_of_available_bytes
  let update_from_available_bytes = update_from_available_bytes
  let classify_write_failure = classify_write_failure
  let note_write_failure = note_write_failure
  let new_session_block_message = new_session_block_message
  let preflight_path_with = preflight_path_with
  let note_write_success = note_write_success
  let set_probe_available_bytes = set_probe_available_bytes_for_tests
end
