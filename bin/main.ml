(** discord-agents — Discord harness for AI agents.

    Usage: discord-agents [--config PATH]

    Reads configuration from ~/.config/discord-agents/config.json
    (or the path given by --config), discovers projects in the configured
    base directories, connects to Discord, and waits for commands. *)

let setup_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

let () =
  setup_logging ();
  Logs.info (fun m -> m "discord-agents starting");
  let config = Discord_agents.Config.load () in
  if config.discord_token = "" then (
    Logs.err (fun m -> m "no discord token configured");
    Logs.err (fun m -> m "set discord_token in %s" (Discord_agents.Config.config_path ()));
    exit 1
  );
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let bot = Discord_agents.Bot.create ~sw ~env config in
  Discord_agents.Bot.run ~sw ~env bot
