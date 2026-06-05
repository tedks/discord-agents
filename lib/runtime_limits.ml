(** Process resource limit hardening for long-running daemon mode. *)

type nofile_limit = {
  soft : int64;
  hard : int64;
}

let desired_nofile_soft_limit = 65_536L

external get_nofile_limit_raw : unit -> int64 * int64 =
  "discord_agents_get_nofile_limit"

external set_nofile_soft_limit_raw : int64 -> unit =
  "discord_agents_set_nofile_soft_limit"

let get_nofile_limit () =
  let soft, hard = get_nofile_limit_raw () in
  { soft; hard }

let nofile_target ~desired { soft; hard } =
  if soft >= desired then
    None
  else
    let target = min desired hard in
    if target > soft then Some target else None

let bump_nofile ?(desired=desired_nofile_soft_limit) () =
  try
    let before = get_nofile_limit () in
    match nofile_target ~desired before with
    | None ->
      Logs.info (fun m ->
        m "runtime_limits: nofile soft limit already %Ld (hard %Ld)"
          before.soft before.hard)
    | Some target ->
      set_nofile_soft_limit_raw target;
      let after = get_nofile_limit () in
      Logs.info (fun m ->
        m "runtime_limits: raised nofile soft limit from %Ld to %Ld (hard %Ld)"
          before.soft after.soft after.hard)
  with exn ->
    Logs.warn (fun m ->
      m "runtime_limits: failed to raise nofile soft limit: %s"
        (Printexc.to_string exn))
