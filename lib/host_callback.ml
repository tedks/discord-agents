(** Observation of the executor -> writer MCP/control route. This proves only
    that the selected MCP executable can reach the selected control socket;
    it says nothing about Discord connectivity, disk admission or model access.
    The caller must use the writer's resolved MCP path and Control_api.socket_path,
    and the same host/writer entries as session routing. This module does not
    establish that correspondence for an arbitrary caller-supplied context. *)

type context = {
  host : Host.t;
  cwd : string;
  writer : string;
  mcp_path : string option;
  socket_path : string;
}

type t = {
  context : context;
  observed_at : float;
  control : unit Host_capability.observation;
}

let reply = "discord-agents-control-v1\n"

let valid_path path = String.starts_with ~prefix:"/" path && Host_capability.printable path

let command context =
  match context.mcp_path with
  | None -> Error Host_capability.Missing_executable
  | Some path when not (valid_path path && valid_path context.socket_path && valid_path context.cwd) ->
    Error Host_capability.Invalid_context
  | Some path ->
    let argv = [path; "--check-control"; context.socket_path] in
    match context.host.Host.transport with
    | Host.Local -> Ok argv
    | Host.Ssh _ ->
      let destination = Option.value context.host.writer_ssh ~default:context.writer in
      match Host.validate_destination ~field:"writer_ssh" destination with
      | Error _ -> Error Host_capability.Invalid_context
      | Ok _ -> Ok (["ssh"] @ Host_exec.ssh_options @
          ["--"; destination; String.concat " " (List.map Remote_script.quote argv)])

let observe ~(run : Host_capability.runner) ~now context =
  let observed_at = now () in
  let control =
    if not context.host.enabled then Host_capability.Unknown Disabled
    else if not (Host_capability.finite observed_at) then Unknown Invalid_context
    else match command context with
    | Error why -> Unknown why
    | Ok argv ->
      match run ~timeout_s:8 ~max_output_bytes:1024 ~cwd:context.cwd context.host argv with
      | Ok text when text = reply -> Known ()
      | Ok _ -> Unknown Malformed
      | Error error -> Unknown (Host_capability.reason error) in
  { context; observed_at; control }

let current ~now ~max_age ~context snapshot =
  if context <> snapshot.context then
    { snapshot with context; control = Host_capability.Unknown Context_changed }
  else if not (Host_capability.finite now && Host_capability.finite max_age &&
    Host_capability.finite snapshot.observed_at) || max_age < 0. ||
    now < snapshot.observed_at || now -. snapshot.observed_at > max_age then
    { snapshot with control = Unknown Stale }
  else snapshot

let collect ~clock ~now ?ssh context =
  observe ~now context ~run:(fun ~timeout_s ~max_output_bytes ~cwd host argv ->
    Host_probe.run ~clock ~timeout_s ~max_output_bytes ?ssh ~cwd host argv)
