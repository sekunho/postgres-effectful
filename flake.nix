{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        name = "hasql-effectful";

        overlays = [ haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            hasql-effectful =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc9101";

                shell = {
                  buildInputs = with pkgs; [
                    # postgresql_17.lib
                    pkg-config
                    haskellPackages.implicit-hie
                  ];

                  tools = {
                    cabal = {};
                    hlint = {};
                    haskell-language-server = {};
                    fourmolu = {};
                  };
                };
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        flake = pkgs.hasql-effectful.flake {};
      in flake // {
        packages = rec {
          default = hasql-effectful;
          hasql-effectful = flake.packages."${name}:lib:${name}";
          hasql-effectful-test = flake.packages."${name}:test:hasql-effectful-test";
        };
      }
  );
}
