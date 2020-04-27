let
  secrets = import ./nixops-secrets.nix;
  hostFromConfig = hostName:
    { config, pkgs, ... }:
    (import ( ./nixos/host- + hostName) { inherit config pkgs; }) //
    { config =
        { deployment =
            { inherit (secrets."${hostName}") targetHost;
              targetEnv = "none";
            };
        };
    };

in
{ network.description = "myconfig";
  x1extremeG2 = hostFromConfig "x1extremeG2";
  vserver = hostFromConfig "vserver";
}
