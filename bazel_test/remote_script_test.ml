module R = Remote_script

let expect condition message = if not condition then failwith message

let rejected = function Error _ -> true | Ok _ -> false

let () =
  expect (R.quote "a'b" = "'a'\\''b'") "single quote was not escaped";
  expect (R.is_env_name "GOOD_2") "valid environment name rejected";
  expect (not (R.is_env_name "lockdir")) "reserved name accepted";
  expect (not (R.is_env_name "A-B")) "invalid environment name accepted";
  expect (R.is_lock_key "session.1-a") "valid lock key rejected";
  expect (not (R.is_lock_key ".hidden")) "hidden lock key accepted";
  expect (rejected (R.script ~nonce:"n" ~mode:R.Capture ~cwd:"."
                      ~env:[] ~argv:[])) "empty command accepted";
  expect (rejected (R.script ~nonce:"n" ~mode:R.Capture ~cwd:"relative"
                      ~env:[] ~argv:["echo"])) "relative directory accepted";
  expect (rejected (R.script ~nonce:"n" ~mode:R.Capture ~cwd:"/tmp"
                      ~env:[("lockdir", "x")] ~argv:["echo"]))
    "reserved environment entry accepted";
  expect (R.stdin_line "abc" = "YWJj\n") "base64 stdin line changed";
  expect (R.parse_marker ~nonce:"n" (R.marker R.started_tag "n" "17")
          = R.Started 17) "start marker not recognized";
  expect (R.parse_marker ~nonce:"n" (R.marker R.exit_tag "n" "255")
          = R.Exit 255) "exit marker not recognized";
  expect (R.parse_marker ~nonce:"n" (R.marker R.exit_tag "other" "0")
          = R.Not_marker) "foreign nonce accepted"
