let
  secrets = import ./nixops-secrets.nix;
  hostFromConfig = hostName:
    { config, pkgs, ... }:
    { config =
        { deployment =
            { inherit (secrets."${hostName}") targetHost;
              targetEnv = "none";
            };
          assertions =
            [ { assertion = config.networking.hostName == hostName;
                message = "hostname should be set!";
              }
            ];
        };
      imports = [(./nixos/host- + hostName)];
    };

in
{ network.description = "myconfig";
  x1extremeG2 = hostFromConfig "x1extremeG2";
  vserver = hostFromConfig "vserver";
}
