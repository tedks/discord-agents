(** Read attachment bytes on a configured executor. The executor and SSH are
    trusted; requested paths and their contents are not. Any error rejects the
    entire batch. The writer never opens a remote path. *)

let max_manifest = 8192
let max_path = 4096
let max_args = 8192
let max_bytes = Discord_rest.max_attachment_request_bytes
let max_output_bytes = max_bytes + max_manifest + 8
let timeout_s = 30

let valid_text s = s <> "" && String.length s <= max_path &&
  not (String.contains s '\000') && Resource.sanitize_utf8 s = s
let valid_attachment_path s = valid_text s &&
  not (String.ends_with ~suffix:"/" s) &&
  not (List.mem (Filename.basename s) ["."; ".."])

let script = {python|
import hashlib, json, os, stat, sys
MAX_FILE = 10 * 1024 * 1024
MAX_TOTAL = 25 * 1024 * 1024

def stamp(s):
    return (s.st_dev, s.st_ino, s.st_mode, s.st_size, s.st_mtime_ns, s.st_ctime_ns)

def identity(s):
    return (s.st_dev, s.st_ino, s.st_mode)

def anchored_root(root):
    # Walk every component from a pinned filesystem root. O_NOFOLLOW on
    # the final component alone would not protect mutable ancestors.
    current = '/'
    fd = os.open(current, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    anchors = [(fd, current, stamp(os.fstat(fd)))]
    try:
        for part in root.split(os.sep)[1:]:
            if not part:
                continue
            current = os.path.join(current, part)
            fd = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=fd)
            anchors.append((fd, current, stamp(os.fstat(fd))))
        return anchors
    except Exception:
        for fd, _, _ in reversed(anchors):
            os.close(fd)
        raise

def check_dirs(dirs, ancestors=False):
    for index, (dirfd, dirname, initial) in enumerate(dirs):
        # A sibling created under / or /tmp must not invalidate this read.
        # Pin the identity of all ancestors, and full metadata at the
        # worktree/subdirectory boundary where a changed entry matters.
        fingerprint = identity if ancestors and index < len(dirs) - 1 else stamp
        expected = initial[:3] if fingerprint is identity else initial
        if fingerprint(os.fstat(dirfd)) != expected or fingerprint(os.stat(dirname, follow_symlinks=False)) != expected:
            raise ValueError('directory changed')

def open_checked(anchors, root, path, remaining):
    rel = os.path.relpath(path, root)
    parts = rel.split(os.sep)
    if rel == '.' or any(p in ('', '.', '..') for p in parts):
        raise ValueError('invalid path')
    dirs = []
    try:
        parent = anchors[-1][0]
        current = root
        for part in parts[:-1]:
            current = os.path.join(current, part)
            fd = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=parent)
            dirs.append((fd, current, stamp(os.fstat(fd))))
            parent = fd
        fd = os.open(parts[-1], os.O_RDONLY | os.O_NONBLOCK | os.O_NOFOLLOW, dir_fd=parent)
        try:
            before = os.fstat(fd)
            limit = min(MAX_FILE, remaining)
            if not stat.S_ISREG(before.st_mode) or before.st_size > limit:
                raise ValueError('not a bounded regular file')
            chunks = []
            left = limit + 1
            while left:
                chunk = os.read(fd, min(65536, left))
                if not chunk:
                    break
                chunks.append(chunk)
                left -= len(chunk)
            data = b''.join(chunks)
            if len(data) > limit or len(data) != before.st_size:
                raise ValueError('file size changed')
            if stamp(before) != stamp(os.fstat(fd)) or stamp(before) != stamp(os.stat(path, follow_symlinks=False)):
                raise ValueError('file changed')
            check_dirs(anchors, ancestors=True)
            check_dirs(dirs)
            return data
        finally:
            os.close(fd)
    finally:
        for dirfd, _, _ in reversed(dirs):
            os.close(dirfd)

try:
    workdir = sys.argv[1]
    roots = os.path.realpath(workdir, strict=True)
    if not os.path.isdir(roots):
        raise ValueError('invalid worktree')
    paths = sys.argv[2:]
    if not 1 <= len(paths) <= 10:
        raise ValueError('invalid count')
    anchors = anchored_root(roots)
    try:
        files = []
        total = 0
        for original in paths:
            if original.endswith(os.sep) or os.path.basename(original) in ('.', '..'):
                raise ValueError('invalid path ending')
            requested = original if os.path.isabs(original) else os.path.join(workdir, original)
            canonical = os.path.realpath(requested, strict=True)
            if os.path.commonpath((roots, canonical)) != roots or canonical == roots:
                raise ValueError('outside worktree')
            data = open_checked(anchors, roots, canonical, MAX_TOTAL - total)
            if os.path.realpath(requested, strict=True) != canonical or os.path.realpath(workdir, strict=True) != roots:
                raise ValueError('path changed')
            check_dirs(anchors, ancestors=True)
            total += len(data)
            files.append((os.path.basename(original), data))
        check_dirs(anchors, ancestors=True)
    finally:
        for fd, _, _ in reversed(anchors):
            os.close(fd)
    manifest = dict(version=1, files=[dict(name=name, size=len(data), md5=hashlib.md5(data, usedforsecurity=False).hexdigest()) for name, data in files])
    header = json.dumps(manifest, ensure_ascii=True, separators=(',', ':')).encode('ascii')
    if len(header) > 8192:
        raise ValueError('manifest limit')
    out = sys.stdout.buffer
    out.write(('%08x' % len(header)).encode('ascii'))
    out.write(header)
    for _, data in files:
        out.write(data)
    out.flush()
except Exception:
    # No path, file contents, or private diagnostics on stdout/stderr.
    sys.exit(1)
|python}

exception Malformed
let fields keys = function
  | `Assoc xs when List.sort String.compare (List.map fst xs) = List.sort String.compare keys -> xs
  | _ -> raise Malformed
let int = function `Int n when n >= 0 -> n | _ -> raise Malformed
let string = function `String s -> s | _ -> raise Malformed
let decode ~paths text =
  try
    let n = String.length text in
    if n < 8 || n > max_output_bytes then raise Malformed;
    let prefix = String.sub text 0 8 in
    if not (String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) prefix) then raise Malformed;
    let header_len = int_of_string ("0x" ^ prefix) in
    if header_len < 1 || header_len > max_manifest || 8 + header_len > n then raise Malformed;
    let top = fields ["version"; "files"]
      (Yojson.Safe.from_string (String.sub text 8 header_len)) in
    if List.assoc "version" top <> `Int 1 then raise Malformed;
    let entries = match List.assoc "files" top with `List xs -> xs | _ -> raise Malformed in
    if List.length entries <> List.length paths then raise Malformed;
    let position = ref (8 + header_len) in
    let files = List.map2 (fun path entry ->
      let fields = fields ["name"; "size"; "md5"] entry in
      let name = string (List.assoc "name" fields) in
      if name <> Filename.basename path then raise Malformed;
      let size = int (List.assoc "size" fields) in
      if size > Discord_rest.max_attachment_file_bytes || size > n - !position then raise Malformed;
      let digest = string (List.assoc "md5" fields) in
      if String.length digest <> 32 || not (String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) digest) then raise Malformed;
      let contents = String.sub text !position size in
      position := !position + size;
      if Digest.to_hex (Digest.string contents) <> digest then raise Malformed;
      Discord_rest.{filename = sanitize_attachment_filename name;
                    content_type = content_type_of_filename name; contents}) paths entries in
    if !position <> n || !position - 8 - header_len > max_bytes then raise Malformed;
    Ok files
  with Malformed | Invalid_argument _ | Failure _ | Yojson.Json_error _ -> Error "malformed attachment response"

let load ~clock ?ssh ~host ~working_dir paths =
  let error message = Error (Printf.sprintf "attachment read on %s: %s" host.Host.name message) in
  if not (valid_text working_dir && String.starts_with ~prefix:"/" working_dir) then error "invalid worktree"
  else if paths = [] || List.length paths > Discord_rest.max_attachment_count ||
          not (List.for_all valid_attachment_path paths) ||
          List.fold_left (fun n s -> n + String.length s) (String.length working_dir) paths > max_args then
    error "invalid attachment paths"
  else
    match Host_capture.run ~clock ~timeout_s ~max_output_bytes ?ssh ~cwd:"/" host
      (["python3"; "-I"; "-c"; script; working_dir] @ paths) with
    | Error failure ->
      let reason = match Host_capability.reason failure with
        | Host_capability.Unreachable -> "executor unreachable"
        | Timed_out -> "deadline exceeded"
        | Output_limit -> "response size exceeded"
        | Disabled -> "executor disabled"
        | _ -> "executor read failed" in
      error reason
    | Ok text -> Result.map_error (fun msg ->
        Printf.sprintf "attachment read on %s: %s" host.Host.name msg)
        (decode ~paths text)
