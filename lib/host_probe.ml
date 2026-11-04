(** Capability probes retain their 1 MiB ceiling even though attachment
    transfers use the larger bounded capture transport. *)
type failure = Host_capture.failure =
  | Disabled
  | Process of Probe_process.failure
  | Host of Host_exec.failure

let protocol_budget = Host_capture.protocol_budget
let max_probe_output_bytes = 1024 * 1024

let run ~clock ~timeout_s ~max_output_bytes ?ssh ?cwd (host : Host.t) argv =
  if not host.enabled then Error Disabled
  else if max_output_bytes < 0 || max_output_bytes > max_probe_output_bytes then
    Error (Process (Setup_failed "invalid probe output limit"))
  else Host_capture.run ~clock ~timeout_s ~max_output_bytes ?ssh ?cwd host argv
