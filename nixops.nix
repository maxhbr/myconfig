let
  secrets = import ./nixops-secrets.nix;
in {
  network.description = "myconfig";
  x1extremeG2 =
    { config, pkgs, ... }:
    (import ./nixos/host-x1extremeG2 { inherit config pkgs; }) //
    { config =
        { deployment =
            { inherit (secrets.x1extremeG2) targetHost;
              targetEnv = "none";
            };
        };
    };
  vserver =
    { config, pkgs, ... }:
    (import ./nixos/host-vserver { inherit config pkgs; }) //
    { config =
        { deployment =
            { inherit (secrets.vserver) targetHost;
              targetEnv = "none";
            };
        };
    };

}
