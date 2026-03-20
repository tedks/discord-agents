(lang dune 3.14)

(name discord_agents)

(generate_opam_files true)

(source
 (github tedks/discord-agents))

(authors "Ted Suzman")

(maintainers "Ted Suzman")

(license MIT)

(package
 (name discord_agents)
 (synopsis "Discord harness for AI agents (Claude, Codex, Gemini)")
 (description
  "A Discord bot that manages AI agent sessions across projects. Creates channels per project, threads per agent session, and bridges communication between Discord and agent subprocesses.")
 (depends
  (ocaml
   (>= 5.1.0))
  dune
  (eio_main
   (>= 1.0))
  (cohttp-eio
   (>= 6.0))
  uri
  ca-certs
  (mirage-crypto-rng
   (>= 1.0))
  (yojson
   (>= 2.0))
  ppx_yojson_conv
  ppx_deriving
  (tls-eio
   (>= 1.0))
  (logs
   (>= 0.7))
  fmt))
