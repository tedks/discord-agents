(** discord-agents — Discord harness for AI agents.

    Usage: discord-agents [--test [CHANNEL_ID]]

    Reads configuration from ~/.config/discord-agents/config.json
    (or the path given by --config), discovers projects in the configured
    base directories, connects to Discord, and waits for commands.

    --test           Run REST + gateway smoke test, then exit
    --test CHANNEL   Also send a test message to the given channel *)

let setup_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

(** Smoke test: verify REST client works, optionally send a test message,
    then connect to gateway briefly to confirm Hello/Identify/READY. *)
let run_test ~sw ~env config test_channel =
  Logs.info (fun m -> m "test: creating REST client...");
  let rest = Discord_agents.Discord_rest.create ~sw ~env
    ~token:config.Discord_agents.Config.discord_token in
  (* Test 1: GET /gateway/bot *)
  Logs.info (fun m -> m "test: calling GET /gateway/bot...");
  (match Discord_agents.Discord_rest.get_gateway rest with
   | Ok url -> Logs.info (fun m -> m "test: gateway URL = %s" url)
   | Error e -> Logs.err (fun m -> m "test: get_gateway failed: %s" e); exit 1);
  (* Test 2: optionally send a message *)
  (match test_channel with
   | Some channel_id ->
     Logs.info (fun m -> m "test: sending message to channel %s..." channel_id);
     (match Discord_agents.Discord_rest.create_message rest
              ~channel_id ~content:"discord-agents test: REST client works" () with
      | Ok msg ->
        Logs.info (fun m -> m "test: message sent, id=%s" msg.Discord_agents.Discord_types.id)
      | Error e ->
        Logs.err (fun m -> m "test: create_message failed: %s" e); exit 1)
   | None ->
     Logs.info (fun m -> m "test: skipping message send (no channel_id given)"));
  (* Test 3: connect to gateway, wait for READY, then exit *)
  Logs.info (fun m -> m "test: connecting to gateway...");
  let ready = ref false in
  let gateway = Discord_agents.Discord_gateway.create
    ~token:config.discord_token
    ~intents:Discord_agents.Discord_gateway.default_intents
    ~handler:(fun event ->
      match event with
      | Discord_agents.Discord_gateway.Connected user ->
        Logs.info (fun m -> m "test: READY as %s — gateway works" user.Discord_agents.Discord_types.username);
        ready := true
      | _ -> ())
  in
  (* Run gateway in a sub-switch so we can cancel it cleanly *)
  let clock = Eio.Stdenv.clock env in
  let deadline = Eio.Time.now clock +. 15.0 in
  (try
    Eio.Switch.run @@ fun test_sw ->
    Eio.Fiber.fork ~sw:test_sw (fun () ->
      Discord_agents.Discord_gateway.connect ~sw:test_sw ~env gateway);
    (* Poll for READY or timeout *)
    let rec wait () =
      if !ready then begin
        Logs.info (fun m -> m "test: all tests passed");
        Eio.Switch.fail test_sw Exit
      end else if Eio.Time.now clock > deadline then begin
        Logs.err (fun m -> m "test: timed out waiting for READY");
        Eio.Switch.fail test_sw (Failure "timeout")
      end else begin
        Eio.Time.sleep clock 0.5;
        wait ()
      end
    in
    wait ()
  with Exit -> ())

let () =
  setup_logging ();
  (* Parse args *)
  let args = Array.to_list Sys.argv |> List.tl in
  let test_mode = List.mem "--test" args in
  let test_channel =
    match args with
    | "--test" :: ch :: _ when not (String.length ch > 0 && ch.[0] = '-') -> Some ch
    | _ -> None
  in
  Logs.info (fun m -> m "discord-agents starting%s" (if test_mode then " (test mode)" else ""));
  let config = Discord_agents.Config.load () in
  if config.discord_token = "" then (
    Logs.err (fun m -> m "no discord token configured");
    Logs.err (fun m -> m "set discord_token in %s" (Discord_agents.Config.config_path ()));
    exit 1
  );
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  if test_mode then
    run_test ~sw ~env config test_channel
  else begin
    let bot = Discord_agents.Bot.create ~sw ~env config in
    Discord_agents.Bot.run ~sw ~env bot
  end
