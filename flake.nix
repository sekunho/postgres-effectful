{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        name = "hasql-effectful";

        overlays = [ haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            hasql-effectful =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc925";

                shell = {
                  buildInputs = with pkgs; [
                    postgresql.lib
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
          hasql-pool-effectful = flake.packages."${name}:lib:hasql-pool-effectful";
          hasql-pool-effectful-test = flake.packages."${name}:test:hasql-effectful-test";
        };
      }
  );
}
