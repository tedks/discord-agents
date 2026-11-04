module A = Discord_agents.Host_attachment
module H = Discord_agents.Host
let check label value = Alcotest.(check bool) label true value
let writer = H.local_host "writer"
let executor = {writer with H.name="executor"; transport=H.Ssh "executor"}
let write path data = Out_channel.with_open_bin path (fun oc -> output_string oc data)
let rec remove path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR -> Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path); Unix.rmdir path
  | _ -> Unix.unlink path
let fake_ssh = Filename.concat (Sys.getcwd ())
  (if Sys.file_exists "fake_ssh.py" then "fake_ssh.py" else "test/fake_ssh.py")
let test_real () =
  let dir = Filename.temp_dir "attachments" "" |> Unix.realpath in
  Fun.protect ~finally:(fun () -> remove dir) (fun () ->
    let ssh = Filename.concat dir "ssh" in
    write ssh ("#!/bin/sh\nexec python3 -I " ^ Filename.quote fake_ssh ^ " \"$@\"\n");
    Unix.chmod ssh 0o755;
    write (Filename.concat dir "big.bin") (String.init (1024 * 1024 + 7) (fun i -> Char.chr (i mod 256)));
    write (Filename.concat dir "empty.txt") "";
    Unix.mkdir (Filename.concat dir "sub") 0o700;
    Unix.symlink "../big.bin" (Filename.concat dir "sub/link.bin");
    Unix.mkfifo (Filename.concat dir "pipe") 0o600;
    let outside = Filename.temp_file "attachment-outside" "" in
    Fun.protect ~finally:(fun () -> Unix.unlink outside) (fun () ->
      write outside "private";
      Unix.symlink outside (Filename.concat dir "outside");
      Eio_main.run (fun env ->
        let clock = Eio.Stdenv.mono_clock env in
        let load ?(host=executor) paths = A.load ~clock ~ssh ~host ~working_dir:dir paths in
        let paths = ["big.bin"; Filename.concat dir "empty.txt"; "sub/link.bin"] in
        (match load paths with
         | Ok files ->
           check "three files" (List.length files = 3);
           check "binary bytes" ((List.hd files).contents = In_channel.with_open_bin (Filename.concat dir "big.bin") In_channel.input_all);
           check "empty" ((List.nth files 1).contents = "");
           check "safe symlink" ((List.nth files 2).contents = (List.hd files).contents);
           check "local parity" (Result.is_ok (load ~host:writer ["big.bin"]))
         | Error e -> Alcotest.fail e);
        List.iter (fun path -> check "unsafe path refused" (Result.is_error (load [path])))
          ["../" ^ Filename.basename outside; "outside"; "pipe"; "missing"];
        check "unreachable" (Result.is_error (load ~host:{executor with transport=H.Ssh "unreachable"} ["big.bin"]));
        check "too many" (Result.is_error (load (List.init 11 (fun _ -> "big.bin"))));
        check "overlong" (Result.is_error (load [String.make 4097 'a']));
        check "invalid UTF8" (Result.is_error (load ["\255"]));
        List.iter (fun path ->
          check "invalid terminal path component" (not (A.valid_attachment_path path));
          check "odd path refused" (Result.is_error (load [path])))
          ["big.bin/"; "big.bin/."; "big.bin/.."; "."; ".."; "sub/link.bin/."];
        check "over file limit" (let path=Filename.concat dir "large" in
          let oc=open_out_bin path in seek_out oc (10*1024*1024+1); output_char oc 'x'; close_out oc;
          Result.is_error (load ["large"]));
        check "aggregate limit" (
          let paths = List.init 3 (fun i -> let name="batch" ^ string_of_int i in
            let oc=open_out_bin (Filename.concat dir name) in
            seek_out oc (9*1024*1024-1); output_char oc 'x'; close_out oc; name) in
          Result.is_error (load paths)))))
let replace_once ~old ~replacement text =
  let i = Str.search_forward (Str.regexp_string old) text 0 in
  String.sub text 0 i ^ replacement ^
  String.sub text (i + String.length old) (String.length text - i - String.length old)

let test_mutating_paths () =
  let root = Filename.temp_dir "attachment-race" "" |> Unix.realpath in
  let moved = root ^ ".moved" in
  let sibling = root ^ ".sibling" in
  Fun.protect ~finally:(fun () ->
    if Sys.file_exists root then remove root;
    if Sys.file_exists moved then remove moved;
    if Sys.file_exists sibling then remove sibling) (fun () ->
    write (Filename.concat root "file.txt") "stable";
    Unix.symlink "file.txt" (Filename.concat root "link.txt");
    Eio_main.run (fun env ->
      let clock = Eio.Stdenv.mono_clock env in
      let run ?(path="file.txt") script = Discord_agents.Host_capture.run ~clock ~timeout_s:3
        ~max_output_bytes:4096 ~cwd:"/" writer
        ["python3"; "-I"; "-c"; script; root; path] in
      check "original script positive control" (Result.is_ok (run A.script));
      List.iter (fun path -> check "script refuses odd ending" (Result.is_error (run ~path A.script)))
        ["file.txt/"; "file.txt/."; "file.txt/.."];
      let marker = "            data = b''.join(chunks)\n" in
      let changed_file = replace_once ~old:marker
        ~replacement:(marker ^ "            os.utime(path, ns=(before.st_atime_ns, before.st_mtime_ns + 1000000000))\n") A.script in
      check "file metadata change refused" (Result.is_error (run changed_file));
      let open_marker = "            data = open_checked(anchors, roots, canonical, MAX_TOTAL - total)\n" in
      let changed_path = replace_once ~old:open_marker
        ~replacement:("            os.unlink(requested)\n            os.symlink('/etc/passwd', requested)\n" ^ open_marker) A.script in
      check "path replacement refused" (Result.is_error (run ~path:"link.txt" changed_path));
      let anchor_marker = "            check_dirs(anchors, ancestors=True)\n" in
      let sibling_change = replace_once ~old:anchor_marker
        ~replacement:("            os.mkdir(roots + '.sibling')\n" ^ anchor_marker) A.script in
      check "unrelated ancestor entry permitted" (Result.is_ok (run sibling_change));
      Unix.rmdir sibling;
      let changed_root = replace_once ~old:anchor_marker
        ~replacement:("            os.rename(roots, roots + '.moved')\n            os.mkdir(roots)\n            os.link(os.path.join(roots + '.moved', 'file.txt'), os.path.join(roots, 'file.txt'))\n" ^ anchor_marker) A.script in
      let no_anchor_check = Str.global_replace
        (Str.regexp_string "check_dirs(anchors, ancestors=True)") "pass" changed_root in
      check "fixture reaches root check" (Result.is_ok (run no_anchor_check));
      remove root; Unix.rename moved root;
      check "root replacement refused" (Result.is_error (run changed_root))))

let running pid =
  match In_channel.with_open_bin (Printf.sprintf "/proc/%d/stat" pid) In_channel.input_all with
  | text -> let state = text.[String.rindex text ')' + 2] in state <> 'Z' && state <> 'X'
  | exception Sys_error _ -> false

let test_cancellation () =
  let dir = Filename.temp_dir "attachment-cancel" "" |> Unix.realpath in
  Fun.protect ~finally:(fun () -> remove dir) (fun () ->
    let pidfile = Filename.concat dir "pid" in
    let ssh = Filename.concat dir "ssh" in
    write ssh ("#!/bin/sh\necho $$ > " ^ Filename.quote pidfile ^ "\nexec sleep 300\n");
    Unix.chmod ssh 0o755;
    write (Filename.concat dir "file.txt") "contents";
    Eio_main.run (fun env ->
      let clock = Eio.Stdenv.mono_clock env in
      let rec started () =
        if not (Sys.file_exists pidfile) then (Eio.Time.Mono.sleep clock 0.01; started ()) in
      let outcome = Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 3.) (fun () ->
        Eio.Fiber.first
          (fun () -> ignore (A.load ~clock ~ssh ~host:executor ~working_dir:dir ["file.txt"]); `Completed)
          (fun () -> started (); `Canceled)) in
      check "read canceled after transport started" (outcome = `Canceled);
      let pid = In_channel.with_open_bin pidfile (fun ic -> int_of_string (input_line ic)) in
      let rec await n =
        if n > 0 && running pid then (Eio.Time.Mono.sleep clock 0.01; await (n-1)) in
      await 100;
      check "owned transport stopped" (not (running pid))))

let test_transport_loss () =
  let dir = Filename.temp_dir "attachment-drop" "" |> Unix.realpath in
  Fun.protect ~finally:(fun () -> remove dir) (fun () ->
    let ssh = Filename.concat dir "drop-ssh" in
    write ssh {py|#!/usr/bin/env python3
import os, subprocess, sys
args = sys.argv[1:]
command = ' '.join(args[args.index('--') + 2:])
line = sys.stdin.buffer.readline()
child = subprocess.Popen(['sh', '-c', command], stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                         start_new_session=True)
out, _ = child.communicate(input=line, timeout=10)
if b'discord_agents.remote_started' not in out or len(out) < 2048:
    sys.exit(254)
sys.stdout.buffer.write(out[:1024])
sys.stdout.buffer.flush()
sys.exit(255)
|py};
    Unix.chmod ssh 0o755;
    write (Filename.concat dir "data.bin") (String.make (1024 * 1024) 'q');
    Eio_main.run (fun env ->
      let clock = Eio.Stdenv.mono_clock env in
      let result = A.load ~clock ~ssh ~host:executor ~working_dir:dir ["data.bin"] in
      check "partial transport output refused" (Result.is_error result);
      match result with
      | Error e -> check "raw transport bytes discarded" (not (String.contains e 'q'))
      | Ok _ -> Alcotest.fail "partial transport accepted"))

let test_decoder () =
  let manifest = {|{"version":1,"files":[{"name":"a.txt","size":3,"md5":"900150983cd24fb0d6963f7d28e17f72"}]}|} in
  let frame = Printf.sprintf "%08x%sabc" (String.length manifest) manifest in
  check "valid frame" (Result.is_ok (A.decode ~paths:["a.txt"] frame));
  let upper = String.sub frame 0 7 ^ "A" ^ String.sub frame 8 (String.length frame - 8) in
  let underscore = String.sub frame 0 5 ^ "_" ^ String.sub frame 6 (String.length frame - 6) in
  List.iter (fun frame -> check "bad frame" (Result.is_error (A.decode ~paths:["a.txt"] frame)))
    [upper; underscore; "noise" ^ frame; frame ^ "x"; String.sub frame 0 (String.length frame-1);
     Printf.sprintf "%08x%sabd" (String.length manifest) manifest;
     "ffffffff" ^ manifest ^ "abc"]
let () = Alcotest.run "host attachment" ["reader", [
  Alcotest.test_case "real local and SSH reads" `Quick test_real;
  Alcotest.test_case "strict framing" `Quick test_decoder;
  Alcotest.test_case "deterministic file and root replacement" `Quick test_mutating_paths;
  Alcotest.test_case "canceled transport" `Quick test_cancellation;
  Alcotest.test_case "partial transport loss" `Quick test_transport_loss;
]]
