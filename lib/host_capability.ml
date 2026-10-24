(** Read-only observations of a trusted, configured executor. No observation
    proves that a model request will succeed. Raw auth/transport output is never
    retained. Call [current] before using a snapshot; it is not admission policy. *)

type unknown =
  | Disabled | Invalid_context | Missing_executable | Unreachable
  | Timed_out | Output_limit | Setup_failed | Io_failed | Command_failed
  | Malformed | Not_supported | Executable_changed | Stale | Context_changed

type 'a observation = Known of 'a | Unknown of unknown

type executable = { path : string; stamp : string }
type help = { flags : string list; commands : string list }
type credentials = Present | Absent

type t = {
  host : Host.t;
  cwd : string;
  agent : Config.agent_kind;
  observed_at : float;
  executable : executable option observation;
  version : string observation;
  help : (string list * help observation) list;
  credentials : credentials observation;
}

(** Injection boundary: implementations must enforce both limits and preserve
    exact bytes/status. [Host_probe.run] is the production implementation. *)
type runner = timeout_s:int -> max_output_bytes:int -> cwd:string ->
  Host.t -> string list -> (string, Host_probe.failure) result

let reason = function
  | Host_probe.Disabled -> Disabled
  | Process Timed_out -> Timed_out
  | Process Output_limit_exceeded -> Output_limit
  | Process (Setup_failed _) -> Setup_failed
  | Process (Io_failed _) -> Io_failed
  | Host (Host_exec.Unreachable _) -> Unreachable
  | Host (Host_exec.Exited _) -> Command_failed
  | Host _ -> Setup_failed

let finite value = Float.is_finite value
let printable s = not (String.exists (fun c -> c < ' ' || c = '\127') s)
let text_line s =
  let s = String.trim s in
  if s <> "" && String.length s <= 512 && printable s then Some s else None

(* Metadata only: follows the launcher symlink, including subsecond timestamps.
   This detects ordinary upgrades, not malicious replacement/ABA races. The
   installed CLI and SSH destination are trusted, like normal agent execution. *)
let identity_script =
  "p=$(command -v \"$1\") || exit 3; \
   [ -f \"$p\" ] && [ -x \"$p\" ] || exit 3; \
   case \"$p\" in /*) ;; *) p=\"$(pwd -P)/$p\";; esac; \
   printf '%s\\000' \"$p\"; stat -Lc '%d:%i:%s:%y:%z' -- \"$p\""

let parse_identity = function
  | Error (Host_probe.Host (Host_exec.Exited (3, ""))) -> Known None
  | Error error -> Unknown (reason error)
  | Ok text ->
    match String.split_on_char '\000' text with
    | [path; stamp] when String.starts_with ~prefix:"/" path
        && String.length path <= 4096 && printable path ->
      (match text_line stamp with
       | Some stamp -> Known (Some { path; stamp })
       | None -> Unknown Malformed)
    | _ -> Unknown Malformed

let words text =
  String.split_on_char ' ' (String.map (function '\t' -> ' ' | c -> c) text)
  |> List.filter ((<>) "")

let is_flag_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true
  | _ -> false

let flag_of_word word =
  let rec length i =
    if i < String.length word && is_flag_char word.[i] then length (i + 1) else i in
  let n = length 0 in
  if n > 1 && word.[0] = '-' then Some (String.sub word 0 n) else None

(* Only option declarations count; a flag mentioned in prose is not evidence.
   Help advertises syntax, not runtime support or account entitlement. *)
let parse_help ~agent ~scope text =
  let raw_lines = String.split_on_char '\n' text in
  let lines = List.map String.trim raw_lines in
  let expected = Config.string_of_agent_kind agent :: scope in
  let usage_matches line =
    match words (String.lowercase_ascii line) with
    | "usage:" :: rest ->
      let rec prefix a b = match a, b with
        | [], _ -> true | x::xs, y::ys when x = y -> prefix xs ys | _ -> false in
      prefix expected rest
    | _ -> false in
  (* A generic parent help page must not authorize a status subcommand. *)
  let usage_matches line = usage_matches line &&
    let ws = words (String.lowercase_ascii line) in
    match List.nth_opt ws (1 + List.length expected) with
    | None -> true
    | Some word -> String.starts_with ~prefix:"[" word || String.starts_with ~prefix:"<" word in
  if String.length text > 65536 || not (List.exists usage_matches lines) then Unknown Malformed
  else
    let _, _, flags = List.fold_left (fun (in_options, option_indent, acc) raw ->
      let line = String.trim raw in
      let rec indentation i =
        if i < String.length raw && (raw.[i] = ' ' || raw.[i] = '\t') then indentation (i+1) else i in
      let indent = indentation 0 in
      let heading = String.lowercase_ascii line in
      if indent = 0 && List.mem heading ["options:"; "flags:"] then true, None, acc
      else if indent = 0 && String.ends_with ~suffix:":" line then false, None, acc
      else if not in_options || not (String.starts_with ~prefix:"-" line) ||
        (match option_indent with Some n -> indent > n + (if n <= 2 then 4 else 0) | None -> false)
      then in_options, option_indent, acc else
      (* Stop at the description column, which is separated by two spaces. *)
      let rec column i =
        if i + 1 >= String.length line then String.length line
        else if line.[i] = ' ' && line.[i+1] = ' ' then i else column (i+1) in
      let tokens = words (String.sub line 0 (column 0)) in
      let declaration_token token = Option.is_some (flag_of_word token) ||
        String.starts_with ~prefix:"<" token || String.starts_with ~prefix:"[" token in
      let declarations = if List.for_all declaration_token tokens
        then List.filter_map flag_of_word tokens else [] in
      (* Clap aligns long-only declarations four columns after short aliases;
         wrapped descriptions are deeper than either declaration column. *)
      let option_indent = if declarations = [] then option_indent
        else Some (min indent (Option.value option_indent ~default:indent)) in
      in_options, option_indent, declarations @ acc) (false, None, []) raw_lines in
    let flags = List.sort_uniq String.compare flags in
    let _, commands = List.fold_left (fun (in_commands, acc) raw ->
      let line = String.trim raw in
      let unindented = raw <> "" && raw.[0] <> ' ' && raw.[0] <> '\t' in
      if unindented && String.lowercase_ascii line = "commands:" then true, acc
      else if unindented && String.ends_with ~suffix:":" line then false, acc
      else if in_commands then
        match words line with
        | command :: _ when String.for_all (function 'a'..'z' | '-' -> true | _ -> false) command ->
          true, command :: acc
        | _ -> in_commands, acc
      else in_commands, acc) (false, []) raw_lines in
    Known { flags; commands = List.sort_uniq String.compare commands }

let auth_of_claude result =
  let parse expected text =
    try match Yojson.Safe.from_string text with
    | `Assoc fields ->
      let values = List.filter_map (fun (k,v) -> if k = "loggedIn" then Some v else None) fields in
      (match values with
       | [`Bool value] when value = expected -> Known (if value then Present else Absent)
       | _ -> Unknown Malformed)
    | _ -> Unknown Malformed
    with Yojson.Json_error _ -> Unknown Malformed in
  match result with
  | Ok text -> parse true text
  | Error (Host_probe.Host (Host_exec.Exited (1, text))) -> parse false text
  | Error error -> Unknown (reason error)

let auth_of_codex = function
  (* Documented status exit 0 means credentials are present. Its output may
     contain account/API-key details: discard it, including on errors. *)
  | Ok _ -> Known Present
  | Error (Host_probe.Host (Host_exec.Exited (1, text)))
      when String.trim text = "Not logged in" -> Known Absent
  | Error error -> Unknown (reason error)

let scopes = function Config.Codex -> [[]; ["exec"]; ["exec"; "resume"]] | _ -> [[]]

let unknown_snapshot ~host ~cwd ~agent ~observed_at why = {
  host; cwd; agent; observed_at; executable = Unknown why; version = Unknown why;
  help = List.map (fun scope -> scope, Unknown why) (scopes agent);
  credentials = Unknown why;
}

let observe ~run ~now ~host ~cwd ~agent =
  let observed_at = now () in
  let unknown = unknown_snapshot ~host ~cwd ~agent ~observed_at in
  if not host.Host.enabled then unknown Disabled
  else if not (finite observed_at) || not (String.starts_with ~prefix:"/" cwd) ||
          not (printable cwd) then unknown Invalid_context
  else
    let probe ~cap argv = run ~timeout_s:5 ~max_output_bytes:cap ~cwd host argv in
    let identity () = probe ~cap:8192
      ["sh"; "-c"; identity_script; "capability-identity"; Config.string_of_agent_kind agent]
      |> parse_identity in
    match identity () with
    | Unknown why -> unknown why
    | Known None -> { (unknown Missing_executable) with executable = Known None }
    | Known (Some exe) as executable ->
      let command args = ["sh"; "-c";
        "DISABLE_AUTOUPDATER=1 NO_COLOR=1 LC_ALL=C; export DISABLE_AUTOUPDATER NO_COLOR LC_ALL; exec \"$@\"";
        "capability-cli"; exe.path] @ args in
      let help scope =
        match probe ~cap:65536 (command (scope @ ["--help"])) with
        | Ok text -> parse_help ~agent ~scope text
        | Error error -> Unknown (reason error) in
      let version = match probe ~cap:2048 (command ["--version"]) with
        | Ok text -> (match text_line text with Some v -> Known v | None -> Unknown Malformed)
        | Error error -> Unknown (reason error) in
      let advertised_command page name = match page with
        | Known h when List.mem name h.commands -> Ok ()
        | Known _ -> Error Not_supported
        | Unknown why -> Error why in
      let helps = List.fold_left (fun pages scope ->
        let observed = match List.rev scope with
          | [] -> help []
          | name :: reversed_parent ->
            let parent = List.assoc (List.rev reversed_parent) pages in
            match advertised_command parent name with
            | Error why -> Unknown why
            | Ok () -> help scope in
        pages @ [scope, observed]) [] (scopes agent) in
      let credentials = match agent with
        | Config.Gemini -> Unknown Not_supported
        | Config.Claude | Config.Codex ->
          let parent = if agent = Config.Claude then "auth" else "login" in
          let root = List.assoc [] helps in
          match advertised_command root parent with
          | Error why -> Unknown why
          | Ok () -> match advertised_command (help [parent]) "status" with
          | Error why -> Unknown why
          | Ok () -> match help [parent; "status"] with
            | Unknown why -> Unknown why
            | Known _ ->
              let result = probe ~cap:16384 (command [parent; "status"]) in
              if agent = Config.Claude then auth_of_claude result else auth_of_codex result in
      (* Any change/failure invalidates the whole mixed-generation snapshot. *)
      match identity () with
      | Known (Some after) when after = exe ->
        { host; cwd; agent; observed_at; executable; version; help = helps; credentials }
      | Unknown why -> unknown why
      | Known _ -> unknown Executable_changed

(** Freshness is explicit, caller-chosen and conservative: the timestamp is the
    beginning of collection, not the end. A changed host entry/agent/cwd invalidates
    every field. Even fresh evidence is observational, never a model grant. *)
let current ~now ~max_age ~host ~cwd ~agent snapshot =
  let why =
    if not (Host.equal host snapshot.host) || cwd <> snapshot.cwd || agent <> snapshot.agent
    then Some Context_changed
    else if not (finite now && finite max_age && finite snapshot.observed_at) ||
      max_age < 0. || now < snapshot.observed_at || now -. snapshot.observed_at > max_age
    then Some Stale else None in
  match why with
  | None -> snapshot
  | Some why -> unknown_snapshot ~host ~cwd ~agent
      ~observed_at:snapshot.observed_at why

let advertised_flag snapshot ~scope flag =
  match List.assoc_opt scope snapshot.help with
  | Some (Known help) -> Known (List.mem flag help.flags)
  | Some (Unknown why) -> Unknown why
  | None -> Unknown Not_supported

let collect ~clock ~now ?ssh ~host ~cwd ~agent () =
  observe ~now ~host ~cwd ~agent
    ~run:(fun ~timeout_s ~max_output_bytes ~cwd host argv ->
      Host_probe.run ~clock ~timeout_s ~max_output_bytes ?ssh ~cwd host argv)
