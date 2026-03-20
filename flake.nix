{
  description = "Discord harness for AI agents (Claude, Codex, Gemini)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
        deps = with ocamlPackages; [
          eio_main
          cohttp-eio
          httpun-ws
          httpun-eio
          yojson
          ppx_yojson_conv
          ppx_deriving
          tls-eio
          logs
          fmt
          uri
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with ocamlPackages; [
            ocaml
            dune_3
            findlib
            ocaml-lsp
            ocamlformat
          ] ++ deps ++ [
            pkgs.gmp
          ];

          shellHook = ''
            echo "discord-agents OCaml dev shell"
            echo "  ocaml $(ocaml -version 2>/dev/null | head -1)"
            echo "  dune $(dune --version 2>/dev/null)"
          '';
        };

        packages.default = ocamlPackages.buildDunePackage {
          pname = "discord_agents";
          version = "0.1.0";
          src = ./.;
          buildInputs = deps;
        };
      });
}
