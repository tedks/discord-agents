(** Public, read-only host diagnostics. No cached evidence or admission decisions.
    Display strings are deliberately separate from executor paths and identities. *)

module C = Host_capability

type gate = { mutable busy : bool }
let create_gate () = { busy = false }

let with_deadline ~clock ~seconds f =
  Eio.Fiber.first (fun () -> Ok (f ()))
    (fun () -> Eio.Time.Mono.sleep clock seconds; Error `Timeout)

(* Called on the bot's single Eio domain. There is no yield between checking
   and claiming the slot; reject concurrent requests instead of queuing probes. *)
let with_slot gate f =
  if gate.busy then Error "A host diagnostic is already running. Try again shortly."
  else begin
    gate.busy <- true;
    Fun.protect ~finally:(fun () -> gate.busy <- false) (fun () -> Ok (f ()))
  end

(* ASCII display avoids bidi/format controls and invalid UTF-8 in paths or CLI
   output. Non-ASCII scalars are shown as U+ escapes, never used as paths again.
   Bound the input prefix by codepoints and explicitly label truncation. *)
let display text =
  let b = Buffer.create 128 in
  let rec loop i count =
    if i >= String.length text then ()
    else if count = 512 then Buffer.add_string b " [truncated]"
    else
      let d = String.get_utf_8_uchar text i in
      let n = Uchar.utf_decode_length d in
      if not (Uchar.utf_decode_is_valid d) then Buffer.add_string b "[invalid UTF-8]"
      else begin
        let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if u < 32 || u >= 127 || u = 64 then
          Buffer.add_string b (Printf.sprintf "[U+%04X]" u)
        else begin
          let c = Char.chr u in
          if String.contains "\\`*_{}[]()<>#|~!" c then Buffer.add_char b '\\';
          Buffer.add_char b c
        end
      end;
      loop (i + max 1 n) (count + 1)
  in
  loop 0 0;
  Buffer.contents b

let hosts ~local_name registry =
  let configured = match registry with None -> [] | Some r -> r.Host.hosts in
  let local = Host.lookup configured local_name in
  let entries = List.map (fun (h : Host.t) ->
    if Option.fold ~none:false ~some:(fun (here : Host.t) -> here.name = h.name) local
    then { h with transport = Host.Local } else h) configured in
  if Option.is_some local then entries else Host.local_host local_name :: entries

let resolve ~local_name ~registry name =
  match Host.resolve (hosts ~local_name registry) name with
  | Host.Not_found -> Error "Unknown host. Use list_hosts for configured names."
  | Host.Ambiguous _ -> Error "Ambiguous host. Use the full configured name."
  | Host.Found h when not h.enabled -> Error "This host is disabled; no probe was run."
  | Host.Found h when Host.is_local h && not (Host.same_host h.name local_name) ->
    Error "No SSH route to the configured writer from this machine; no probe was run."
  | Host.Found h -> Ok h

let inventory ~local_name registry =
  let entries = hosts ~local_name registry in
  let line (h : Host.t) =
    let route = if not h.enabled then "disabled; not contacted"
      else if Host.is_local h then
        if Host.same_host h.name local_name then "local writer"
        else "no route from this writer"
      else "SSH executor; reachability unknown" in
    "- " ^ display h.name ^ ": " ^ route in
  `Assoc ["ok", `Bool true;
    "report", `String (String.concat "\n"
      ("Configured hosts (no probes run):" :: List.map line entries));
    "hosts", `List (List.map (fun (h : Host.t) -> `Assoc [
      "name", `String h.name; "enabled", `Bool h.enabled;
      "is_writer", `Bool (Host.same_host h.name local_name)]) entries)]

let reason = function
  | C.Disabled -> "disabled" | Invalid_context -> "invalid context"
  | Missing_executable -> "executable missing" | Unreachable -> "transport unreachable"
  | Timed_out -> "timed out" | Output_limit -> "output limit exceeded"
  | Setup_failed -> "probe setup failed" | Io_failed -> "probe I/O failed"
  | Command_failed -> "probe command failed" | Malformed -> "unrecognized response"
  | Not_supported -> "no advertised probe" | Executable_changed -> "executable changed"
  | Stale -> "stale" | Context_changed -> "context changed"

let observed render = function
  | C.Known value -> render value
  | Unknown why -> "unknown (" ^ reason why ^ ")"

let diagnose ~run ~now ~host ~cwd ~agent ~writer ~mcp_path ~socket_path =
  let capability = C.observe ~run ~now ~host ~cwd ~agent in
  let context = Host_callback.{ host; cwd; writer; mcp_path; socket_path } in
  let callback = Host_callback.observe ~run ~now context in
  let finished = now () in
  let capability = C.current ~now:finished ~max_age:60. ~host ~cwd ~agent capability in
  let callback = Host_callback.current ~now:finished ~max_age:60. ~context callback in
  let flags = List.map (fun (scope, help) ->
    let names xs =
      String.concat ", " (List.map display (List.filteri (fun i _ -> i < 64) xs))
      ^ (if List.length xs > 64 then " [truncated]" else "") in
    "Advertised flags (" ^ (if scope = [] then "root" else String.concat " " scope) ^ "): " ^
      observed (fun (h : C.help) -> if h.flags = [] then "none advertised" else names h.flags) help)
    capability.help in
  let report = String.concat "\n" ([
    "Host: " ^ display host.Host.name;
    "Agent: " ^ Config.string_of_agent_kind agent;
    "Working directory: " ^ display cwd;
    Printf.sprintf "Observation started: %.3f (Unix time)" capability.observed_at;
    "Executable: " ^ observed (function None -> "missing" | Some (e : C.executable) -> display e.path) capability.executable;
    "Version: " ^ observed display capability.version;
    "CLI credential store: " ^ observed (function C.Present -> "present" | Absent -> "absent") capability.credentials;
    "MCP control callback: " ^ observed (fun () -> "reachable") callback.control;
    "Model access: unknown (not queried)";
  ] @ flags @ [
    "Fresh diagnostic only; no session admission decision. Credential-store presence does not establish request credentials, validity or model access.";
    "Uses the current inherited local/SSH environment; per-session overrides are not evaluated. Callback reachability does not establish Discord or disk health.";
    "Display escapes are not executable paths. No login or model request was run.";
  ]) in
  `Assoc ["ok", `Bool true; "report", `String report]
