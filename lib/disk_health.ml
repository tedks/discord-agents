(** Shared disk-pressure state for durable bot metadata writes.

    The bot only has a few write-critical paths (sessions, runtime
    settings, config, generated MCP config), but they sit under several
    different modules. This module centralizes:
    - proactive free-space checks before writes
    - classification of disk-related write failures
    - a process-wide degraded/read-only state surfaced through health
    - user-facing summaries for commands/control API *)

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

let mib n =
  Int64.mul (Int64.of_int n) (Int64.mul 1024L 1024L)

let warning_threshold_bytes = mib 128
let read_only_threshold_bytes = mib 64

let state_mu = Mutex.create ()

let state = ref {
  mode = Healthy;
  available_bytes = None;
  warning_threshold_bytes;
  read_only_threshold_bytes;
  last_error = None;
  checked_path = None;
  checked_at = None;
}

let with_state f =
  Mutex.lock state_mu;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock state_mu)
    (fun () -> f state)

let reset_for_tests () =
  with_state (fun state ->
    state := {
      mode = Healthy;
      available_bytes = None;
      warning_threshold_bytes;
      read_only_threshold_bytes;
      last_error = None;
      checked_path = None;
      checked_at = None;
    })

let snapshot () =
  with_state (fun state -> !state)

let string_of_mode = function
  | Healthy -> "healthy"
  | Warning -> "warning"
  | Read_only -> "read_only"

let pressure snapshot =
  match snapshot.mode with
  | Healthy -> false
  | Warning | Read_only -> true

let is_read_only () =
  match (snapshot ()).mode with
  | Read_only -> true
  | Healthy | Warning -> false

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

let split_fields line =
  String.split_on_char ' ' line
  |> List.filter (fun field -> field <> "")

let available_bytes_of_df_line line =
  match split_fields line with
  | _filesystem :: _blocks :: _used :: available_kib :: _capacity :: _mount ->
    Int64.mul 1024L (Int64.of_string available_kib)
  | _ ->
    failwith (Printf.sprintf "unexpected df output: %s" line)

let probe_available_bytes_with_df path =
  let argv = [| "df"; "-Pk"; path |] in
  let ic = Unix.open_process_args_in "df" argv in
  let read_result =
    try
      ignore (input_line ic);
      Ok (available_bytes_of_df_line (input_line ic))
    with exn ->
      Error exn
  in
  match read_result, Unix.close_process_in ic with
  | Ok available_bytes, Unix.WEXITED 0 ->
    available_bytes
  | Ok _, Unix.WEXITED code ->
    failwith (Printf.sprintf "df exited with status %d for %s" code path)
  | Ok _, status ->
    failwith (Printf.sprintf "df failed for %s: %s"
      path
      (match status with
       | Unix.WSIGNALED signal -> Printf.sprintf "signaled %d" signal
       | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal
       | Unix.WEXITED _ -> "unexpected exit"))
  | Error exn, _ ->
    raise exn

let update_from_available_bytes ~path available_bytes =
  let mode = mode_of_available_bytes available_bytes in
  let last_error =
    match mode with
    | Healthy -> None
    | Warning | Read_only ->
      Some (low_space_summary ~path ~available_bytes ~mode)
  in
  with_state (fun state ->
    state := {
      mode;
      available_bytes = Some available_bytes;
      warning_threshold_bytes;
      read_only_threshold_bytes;
      last_error;
      checked_path = Some path;
      checked_at = Some (Unix.gettimeofday ());
    });
  match mode with
  | Healthy | Warning -> Ok ()
  | Read_only ->
    Error (Printf.sprintf
      "Bot is in read-only mode due to disk pressure near `%s`: only %s free. Free space and retry."
      path (human_bytes available_bytes))

let note_probe_failure ~path exn =
  let err =
    Printf.sprintf "disk probe failed near %s: %s"
      path (Printexc.to_string exn)
  in
  with_state (fun state ->
    state := {
      !state with
      available_bytes = None;
      last_error = Some err;
      checked_path = Some path;
      checked_at = Some (Unix.gettimeofday ());
    })

let preflight_path_with ~available_bytes_of_path path =
  let path = probe_target path in
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
  preflight_path_with ~available_bytes_of_path:probe_available_bytes_with_df path

let preflight_write path =
  preflight_path path

let preflight_state_mutation () =
  preflight_path (Resource.app_config_dir ())

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
  else if string_contains lower "disk quota exceeded" then
    Some { code = "EDQUOT"; summary = msg }
  else if string_contains lower "read-only file system" then
    Some { code = "EROFS"; summary = msg }
  else
    None

let rec classify_write_failure exn =
  match exn with
  | Resource.Durable_write_visible_but_unconfirmed (_, inner) ->
    classify_write_failure inner
  | Unix.Unix_error (code, _fn, path) ->
    let code_message = Unix.error_message code in
    let tracked =
      if code = Unix.ENOSPC then Some ("ENOSPC", code_message)
      else if code = Unix.EROFS then Some ("EROFS", code_message)
      else if code = Unix.EIO then Some ("EIO", code_message)
      else if string_contains (String.lowercase_ascii code_message) "quota" then
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
    let available_bytes =
      try
        let target = probe_target path in
        Some (probe_available_bytes_with_df target)
      with _ -> None
    in
    with_state (fun state ->
      state := {
        mode = Read_only;
        available_bytes;
        warning_threshold_bytes;
        read_only_threshold_bytes;
        last_error = Some failure.summary;
        checked_path = Some path;
        checked_at = Some (Unix.gettimeofday ());
      })

let note_write_success path =
  ignore (preflight_path path)

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

let new_session_block_message ?(preflight=preflight_state_mutation) () =
  match preflight () with
  | Error err -> Some err
  | Ok () when is_read_only () ->
    Some (status_summary () ^ " Free space and retry.")
  | Ok () ->
    None
