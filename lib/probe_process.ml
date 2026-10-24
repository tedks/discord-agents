(** Short-lived subprocesses with mandatory time and output bounds.

    Unlike Eio's process manager, we retain the unreaped child until after
    killing its process group. This pins the group id even if the leader
    exits while a descendant still owns the output pipe. Probes must not
    daemonize or escape the group with setsid. Requires an Eio context. *)

type failure =
  | Timed_out
  | Output_limit_exceeded
  | Setup_failed of string

type output = { status : Unix.process_status; text : string }

external exited_without_reaping : int -> bool = "discord_agents_probe_exited"

exception Output_limit

let run ~clock ~timeout_s ~max_output_bytes ?(input = "") argv =
  if not (Float.is_finite timeout_s) || timeout_s <= 0. then
    Error (Setup_failed "probe timeout must be finite and positive")
  else if max_output_bytes < 0 then
    Error (Setup_failed "probe output limit must be non-negative")
  else if argv = [] || List.exists Remote_script.has_nul argv then
    Error (Setup_failed "probe command must be non-empty and contain no NUL bytes")
  else match Resource.executable_in_path "setsid" with
  | None -> Error (Setup_failed "probe supervision requires setsid in PATH")
  | Some setsid ->
    let owned = ref [] in
    let close fd =
      if List.mem fd !owned then begin
        owned := List.filter ((<>) fd) !owned;
        try Unix.close fd with Unix.Unix_error _ -> ()
      end
    in
    let pipe () =
      let r, w = Unix.pipe ~cloexec:true () in
      owned := r :: w :: !owned;
      r, w
    in
    Fun.protect ~finally:(fun () -> List.iter close !owned) (fun () ->
      try
        let stdin_r, stdin_w = pipe () in
        let stdout_r, stdout_w = pipe () in
        let args = setsid :: "--wait" :: "--" :: argv in
        (* Unix.create_process inherits our process group: the new child
           cannot already be its leader, so setsid does not fork. *)
        let pid = Unix.create_process setsid (Array.of_list args)
            stdin_r stdout_w stdout_w in
        close stdin_r;
        close stdout_w;
        let status = ref None in
        let text = Buffer.create (min max_output_bytes 4096) in
        let outcome = Fun.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () ->
              (* No wait/reap before these signals. First prevent a child
                 still in setsid from creating a group after our kill. *)
              (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
              (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
              status := Some (Eio_unix.run_in_systhread (fun () ->
                Host_exec.waitpid_retry pid))))
          (fun () ->
            Unix.set_nonblock stdin_w;
            Unix.set_nonblock stdout_r;
            let rec write off =
              if off < String.length input then
                match Unix.write_substring stdin_w input off (String.length input - off) with
                | n -> Eio.Fiber.yield (); write (off + n)
                | exception Unix.Unix_error (Unix.EINTR, _, _) -> write off
                | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                  Eio_unix.await_writable stdin_w; write off
                | exception Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> ()
            in
            let chunk = Bytes.create 4096 in
            let rec read () =
              match Unix.read stdout_r chunk 0 (Bytes.length chunk) with
              | 0 -> ()
              | n ->
                if n > max_output_bytes - Buffer.length text then raise Output_limit;
                Buffer.add_subbytes text chunk 0 n;
                Eio.Fiber.yield ();
                read ()
              | exception Unix.Unix_error (Unix.EINTR, _, _) -> read ()
              | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                Eio_unix.await_readable stdout_r; read ()
            in
            let rec wait () =
              if not (exited_without_reaping pid) then begin
                Eio.Time.Mono.sleep clock 0.01;
                wait ()
              end
            in
            try
              Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock timeout_s)
                (fun () -> Eio.Fiber.all [
                  (fun () -> Fun.protect ~finally:(fun () -> close stdin_w)
                    (fun () -> write 0));
                  read;
                  wait;
                ]);
              Ok ()
            with
            | Eio.Time.Timeout -> Error Timed_out
            | Output_limit -> Error Output_limit_exceeded)
        in
        Result.map (fun () ->
          { status = Option.get !status; text = Buffer.contents text }) outcome
      with Unix.Unix_error (error, operation, _) ->
        Error (Setup_failed (operation ^ ": " ^ Unix.error_message error)))
