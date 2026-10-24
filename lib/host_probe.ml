(** Bounded, read-only command transport for capability probes. This does
    not infer auth or model availability from transport success. *)

type failure =
  | Disabled
  | Process of Probe_process.failure
  | Host of Host_exec.failure

(* The extra budget bounds login-shell noise and protocol markers too.
   The decoded command output is checked separately, never truncated. *)
let protocol_budget = 4096
let max_probe_output_bytes = 1024 * 1024

let run ~clock ~timeout_s ~max_output_bytes ?(ssh = "ssh") ?cwd
    (host : Host.t) argv =
  if not host.enabled then Error Disabled
  else if max_output_bytes < 0 || max_output_bytes > max_probe_output_bytes then
    Error (Process (Setup_failed "invalid probe output limit"))
  else if timeout_s <= 0 || timeout_s > 3600 then
    Error (Process (Setup_failed "probe timeout must be between 1 and 3600 seconds"))
  else
    let cwd = Option.value cwd ~default:"." in
    let nonce = Remote_script.fresh_nonce () in
    match Remote_script.script ~nonce ~mode:Capture ~cwd ~env:[] ~argv with
    | Error message -> Error (Process (Setup_failed message))
    | Ok script ->
      let command = match host.transport with
        | Host.Local -> Ok (["sh"; "-c"; script], "")
        | Host.Ssh _ when String.length (Remote_script.quote script) + 64 > Remote_script.max_arg_bytes ->
          Error (Host_exec.Setup_failed "probe script exceeds the executor argument limit")
        | Host.Ssh destination ->
          Result.map (fun ssh ->
            (* coreutils timeout owns the executor process group. Keep its
               shell alive until inherited stdout closes (the cat), then
               kill that same group even on success. No PID is read from
               disk or signalled after its owner exits. A disconnected
               writer therefore leaves at most timeout_s of probe work.
               A probe that deliberately creates a new session is outside
               this contract, just as on the local transport. *)
            let supervised = Printf.sprintf
              "for probe_tool in timeout cat sh; do command -v \"$probe_tool\" >/dev/null 2>&1 || \
               { echo \"probe supervision requires $probe_tool\" >&2; exit 1; }; done; \
               probe_timeout_version=$(timeout --version 2>/dev/null) || exit 1; \
               case \"$probe_timeout_version\" in 'timeout (GNU coreutils)'*) ;; \
               *) echo 'probe supervision requires GNU coreutils timeout' >&2; exit 1;; esac; \
               timeout -s KILL %d sh -c %s"
              timeout_s
              (Remote_script.quote (Printf.sprintf
                "( sh -c %s ) | cat; kill -KILL 0"
                (Remote_script.quote script)))
            in
            Host_exec.ssh_argv ~ssh ~destination, Remote_script.stdin_line supervised)
            (Host_exec.resolve_ssh ssh)
      in
      match command with
      | Error error -> Error (Host error)
      | Ok (command, input) ->
        match Probe_process.run ~clock ~timeout_s:(float_of_int timeout_s)
            ~max_output_bytes:(max_output_bytes + protocol_budget) ~input command with
        | Error error -> Error (Process error)
        | Ok output ->
          let decoded = Host_exec.classify_capture ~nonce ~cwd
              ~ssh_status:(Host_exec.exit_code output.status) output.text in
          match decoded with
          | Ok text | Error (Host_exec.Exited (_, text))
            when String.length text > max_output_bytes -> Error (Process Output_limit_exceeded)
          | _ -> Result.map_error (fun error -> Host error) decoded
