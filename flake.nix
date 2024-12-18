{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
  };

  outputs = { self, nixpkgs, flake-parts, process-compose-flake, services-flake }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      imports = [
        inputs.process-compose-flake.flakeModule
      ];

      flake = {
        nixosModules = { };
      };

      perSystem = { config, system, ... }:
        let
          pkgs = import nixpkgs { inherit system; };
          haskellPackages = pkgs.haskell.packages.ghc910;
        in
        {
          process-compose.dev-services = {
            imports = [ inputs.services-flake.processComposeModules.default ];

            services.postgres."db" = {
              enable = true;
              package = pkgs.postgresql_17;

              initialScript.before = ''
                CREATE USER test LOGIN PASSWORD 'test' SUPERUSER;
              '';

              initialDatabases = [
                { name = "test_db"; schemas = []; }
              ];
            };
          };

          devShells = {
            default =
              haskellPackages.shellFor {
                POSTGRES_HOST = "127.0.0.1";
                POSTGRES_USER = "test";
                POSTGRES_PASSWORD = "test";
                POSTGRES_DB_NAME = "test_db";
                packages = p: [ ];

                buildInputs = [
                  haskellPackages.haskell-language-server
                  haskellPackages.fourmolu
                  haskellPackages.cabal-install
                  haskellPackages.cabal-fmt

                  pkgs.postgresql_17
                  pkgs.sqlite
                  pkgs.watchexec
                  pkgs.zlib
                  pkgs.git
                  pkgs.nil
                  pkgs.nixpkgs-fmt
                ];
              };
          };
        };
    };
}
