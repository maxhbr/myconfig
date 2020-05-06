let
  secrets = import ./nixops-secrets.nix;
  hostFromConfig = hostName: addConfig:
    { config, pkgs, ... }:
    { config =
        { deployment =
            { inherit (secrets."${hostName}") targetHost;
              targetEnv = "none";
              # none.sshPrivateKey = 
              # none.sshPublicKey =
              # none.sshPublicKeyDeployed = 
            };
          assertions =
            [ { assertion = config.networking.hostName == hostName;
                message = "hostname should be set!";
              }
            ];
        } // secrets."${hostName}".config;
      imports =
        [ (./nixos/host- + hostName)
          addConfig
        ];
    };
in
{ network.description = "myconfig";
  x1extremeG2 = hostFromConfig "x1extremeG2" {};
  vserver = hostFromConfig "vserver"
    { deployment.keys =
        { wg-private = {
            text = builtins.readFile ../wireguard-keys/vserver/private;
            user = "root";
            group = "root";
            permissions = "0400";
          };
          wg-public = {
            text = builtins.readFile ../wireguard-keys/vserver/public;
            user = "root";
            group = "root";
            permissions = "0400";
          };
       };
    };
  T470p = hostFromConfig "T470p" {};
  T470s = hostFromConfig "T470s" {};
}
