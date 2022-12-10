{ hasql-effectful, hasql-pool-effectful, pkgs, system }:
  let
    database      = "postgres";
    schema        = "public";
    username      = "postgres";
    password      = "";
    postgrestPort = 5432;

    # NixOS module shared between server and client
    sharedModule = {
      # Since it's common for CI not to have $DISPLAY available, we have to explicitly tell the tests "please don't expect any screen available"
      virtualisation.graphics = false;
    };
  in pkgs.nixosTest ({
    # NixOS tests are run inside a virtual machine, and here we specify system of the machine.
    nodes = {
      server = { config, pkgs, ... }: {
        imports = [ sharedModule ];
        networking.firewall.allowedTCPPorts = [ postgrestPort ];

        services.postgresql = {
          enable = true;

          initialScript = pkgs.writeText "initialScript.sql" ''
            create schema ${schema};
            create role ${username} inherit login password '${password}';
          '';

        };

        users = {
          mutableUsers = false;

          users = {
            # For ease of debugging the VM as the `root` user
            root.password = "";
            # Create a system user that matches the database user so that we
            # can use peer authentication.  The tutorial defines a password,
            # but it's not necessary.
            "${username}".isSystemUser = true;
          };
        };
      };


      client = {
        imports = [ sharedModule ];
      };
    };

    # Disable linting for simpler debugging of the testScript
    skipLint = true;

    testScript = ''
      import json
      import sys
      start_all()
    '';

  })
