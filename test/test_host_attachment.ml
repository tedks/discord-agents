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
        check "over file limit" (let path=Filename.concat dir "large" in
          let oc=open_out_bin path in seek_out oc (10*1024*1024+1); output_char oc 'x'; close_out oc;
          Result.is_error (load ["large"]));
        check "aggregate limit" (
          let paths = List.init 3 (fun i -> let name="batch" ^ string_of_int i in
            let oc=open_out_bin (Filename.concat dir name) in
            seek_out oc (9*1024*1024-1); output_char oc 'x'; close_out oc; name) in
          Result.is_error (load paths)))))
let test_decoder () =
  let manifest = {|{"version":1,"files":[{"name":"a.txt","size":3,"md5":"900150983cd24fb0d6963f7d28e17f72"}]}|} in
  let frame = Printf.sprintf "%08x%sabc" (String.length manifest) manifest in
  check "valid frame" (Result.is_ok (A.decode ~paths:["a.txt"] frame));
  List.iter (fun frame -> check "bad frame" (Result.is_error (A.decode ~paths:["a.txt"] frame)))
    ["noise" ^ frame; frame ^ "x"; String.sub frame 0 (String.length frame-1);
     Printf.sprintf "%08x%sabd" (String.length manifest) manifest;
     "ffffffff" ^ manifest ^ "abc"]
let () = Alcotest.run "host attachment" ["reader", [
  Alcotest.test_case "real local and SSH reads" `Quick test_real;
  Alcotest.test_case "strict framing" `Quick test_decoder;
]]
