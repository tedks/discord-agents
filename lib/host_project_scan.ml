(** Bounded discovery on one executor. Results are observations, never cleanup
    or admission authority. A failed/incomplete scan is not an empty inventory. *)
type project = { name : string; path : string; is_bare : bool; repository : string option }
type failure = Transport of Host_capability.unknown | Invalid_roots | Malformed
let max_output_bytes = 512 * 1024
let max_projects = 512
let timeout_s = 30
let valid_text s = s <> "" && String.length s <= 4096 &&
  not (String.contains s '\000') && Resource.sanitize_utf8 s = s
let valid_path s = valid_text s && String.starts_with ~prefix:"/" s
let valid_roots roots = List.length roots <= 32 && List.for_all valid_path roots &&
  List.fold_left (fun n s -> n + String.length s) 0 roots <= 8192

(* Only network repository identities can group across hosts. Local and
   ambiguous remotes stay host/path-local. Do not retain URL credentials,
   queries or fragments. Preserve case on arbitrary Git servers. *)
let repository_key url =
  if not (valid_text url) || String.exists (fun c -> c <= ' ' || c = '\127') url then None
  else match Project.parse_github_remote url with
  | Some (owner, repo) -> Some ("github.com/" ^ owner ^ "/" ^ String.lowercase_ascii repo)
  | None ->
    let key host port path =
      if host = "" || path = "" || path = "/" then None
      else Some (String.lowercase_ascii host ^ port ^ "/" ^
        Project.strip_suffix (if path.[0] = '/' then String.sub path 1 (String.length path - 1) else path) ".git") in
    let uri = Uri.of_string url in
    match Uri.scheme uri, Uri.host uri with
    | Some ("https" | "http" | "ssh" | "git" as scheme), Some host
      when Uri.query uri = [] && Uri.fragment uri = None &&
        (Uri.userinfo uri = None || (scheme = "ssh" && Uri.userinfo uri = Some "git")) ->
      key host (match Uri.port uri with None -> "" | Some port -> ":" ^ string_of_int port) (Uri.path uri)
    | _ ->
      if String.starts_with ~prefix:"git@" url && not (String.contains url '?') && not (String.contains url '#') then
        match String.split_on_char ':' (String.sub url 4 (String.length url - 4)) with
        | [host; path] when Result.is_ok (Host.validate_name host) -> key host "" path
        | _ -> None
      else None

let fields keys = function
  | `Assoc xs when List.sort String.compare (List.map fst xs) = List.sort String.compare keys -> xs
  | _ -> raise Exit
let string = function `String s when valid_text s -> s | _ -> raise Exit
let decode text =
  try
    if String.length text > max_output_bytes then raise Exit;
    let top = fields ["version"; "projects"] (Yojson.Safe.from_string text) in
    if List.assoc "version" top <> `Int 1 then raise Exit;
    let projects = match List.assoc "projects" top with
      | `List xs when List.length xs <= max_projects -> xs | _ -> raise Exit in
    let seen = Hashtbl.create 32 in
    let projects = List.map (fun json ->
      let xs = fields ["name"; "path"; "is_bare"; "remote_url"] json in
      let name = string (List.assoc "name" xs) and path = string (List.assoc "path" xs) in
      if not (valid_path path) || Hashtbl.mem seen path then raise Exit;
      Hashtbl.add seen path ();
      let is_bare = match List.assoc "is_bare" xs with `Bool b -> b | _ -> raise Exit in
      let repository = match List.assoc "remote_url" xs with
        | `Null -> None | `String s when valid_text s -> repository_key s | _ -> raise Exit in
      { name; path; is_bare; repository }) projects in
    Ok (List.sort (fun a b -> String.compare a.path b.path) projects)
  with Exit | Yojson.Json_error _ | Invalid_argument _ -> Error Malformed

(* Python is sent as an argument, not installed on the executor. Isolated mode
   avoids cwd/PYTHONPATH imports. Git only reads metadata, with inherited GIT_*
   overrides removed. Every directory entry and project count is bounded; the
   outer transport owns the deadline and all subprocesses. Fail the whole scan
   on missing/unreadable roots, churn, invalid Git metadata or limit overflow. *)
let script = {python|
import json, os, stat, subprocess, sys
LIMIT = 4096
entries_seen = 0
projects = {}
env = {k: v for k, v in os.environ.items() if not k.startswith('GIT_')}
env['GIT_TERMINAL_PROMPT'] = '0'
def git(path, args, absent=False):
    p = subprocess.Popen(['git', '-C', path] + args, stdin=subprocess.DEVNULL,
                         stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
    try:
        data = p.stdout.read(8193)
        if len(data) > 8192:
            raise ValueError('git output limit')
        code = p.wait()
        if absent and code == 1 and data == b'':
            return None
        if code != 0:
            raise ValueError('git metadata unavailable')
        return data.decode('utf-8', errors='strict')
    finally:
        if p.poll() is None:
            p.kill()
        p.wait()
        p.stdout.close()
def children(path):
    global entries_seen
    result = []
    with os.scandir(path) as entries:
        for entry in entries:
            entries_seen += 1
            if entries_seen > LIMIT:
                raise ValueError('directory entry limit')
            # Follow symlinks, matching local project discovery. Depth is fixed;
            # real paths deduplicate aliases. Broken links/churn fail the scan.
            if stat.S_ISDIR(entry.stat(follow_symlinks=True).st_mode):
                result.append(entry.name)
    return sorted(result)
def candidate(path, name):
    path = os.path.realpath(path, strict=True)
    marker = os.path.lexists(os.path.join(path, '.git'))
    bare = all(os.path.exists(os.path.join(path, item)) for item in ('HEAD', 'objects', 'refs'))
    if not (marker or bare):
        return False
    if path in projects:
        return True
    actual_bare = git(path, ['rev-parse', '--is-bare-repository']).strip()
    if actual_bare not in ('true', 'false'):
        raise ValueError('invalid git response')
    remote = git(path, ['config', '--local', '--null', '--get', 'remote.origin.url'], absent=True)
    if remote is not None:
        if not remote.endswith('\0') or '\0' in remote[:-1]:
            raise ValueError('invalid remote response')
        remote = remote[:-1] or None
    projects[path] = dict(name=name, path=path, is_bare=actual_bare == 'true', remote_url=remote)
    if len(projects) > 512:
        raise ValueError('project limit')
    return True
try:
    for base in sys.argv[1:]:
        base = os.path.realpath(base, strict=True)
        for name in children(base):
            path = os.path.join(base, name)
            if candidate(path, name):
                continue
            nested = 0
            for sub in children(path):
                if candidate(os.path.join(path, sub), name + '/' + sub):
                    nested += 1
                    if nested > 10:
                        raise ValueError('nested project limit')
    text = json.dumps(dict(version=1, projects=list(projects.values())), ensure_ascii=True)
    if len(text) > 524288:
        raise ValueError('inventory output limit')
    print(text)
except (OSError, ValueError, UnicodeError):
    # Never emit partial inventory or raw Git/SSH/credential diagnostics.
    sys.exit(1)
|python}

let observe ~run ~host ~roots =
  if not host.Host.enabled then Error (Transport Disabled)
  else if not (valid_roots roots) then Error Invalid_roots
  else match run ~timeout_s ~max_output_bytes ~cwd:"/" host (["python3"; "-I"; "-c"; script] @ roots) with
    | Ok text -> decode text
    | Error error -> Error (Transport (Host_capability.reason error))
let collect ~clock ?ssh ~host ~roots () =
  observe ~host ~roots ~run:(fun ~timeout_s ~max_output_bytes ~cwd host argv ->
    Host_probe.run ~clock ~timeout_s ~max_output_bytes ?ssh ~cwd host argv)
