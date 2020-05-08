let
  secrets = import ./nixops-secrets.nix;
  hostFromConfig = hostName: addConfig:
    { config, pkgs, ... }@args:
    { config =
        { deployment.targetEnv = "none";
          assertions =
            [ { assertion = config.networking.hostName == hostName;
                message = "hostname should be set!";
              }
            ];
        } // secrets."${hostName}";
      imports =
        [ (./nixos/host- + hostName)
          (addConfig args)
        ];
    };
in
{ network.description = "myconfig";
  x1extremeG2 = hostFromConfig "x1extremeG2"
    ( {lib, ...}:
      { deployment.targetHost = lib.mkDefault "10.199.199.2";
      });
  workstation = hostFromConfig "workstation"
    ( {lib, ...}:
      { deployment.targetHost =  lib.mkDefault "10.199.199.5";
        deployment.keys =
          { wg-private =
              { text = builtins.readFile ../wireguard-keys/workstation/private;
                user = "root";
                group = "root";
                permissions = "0400";
              };
            wg-public =
              { text = builtins.readFile ../wireguard-keys/workstation/public;
                user = "root";
                group = "root";
                permissions = "0400";
              };
          };
      });
  vserver = hostFromConfig "vserver"
    ( {lib, ...}:
      { deployment.targetHost = lib.mkDefault "10.199.199.1";
        deployment.keys =
          { wg-private =
              { text = builtins.readFile ../wireguard-keys/vserver/private;
                user = "root";
                group = "root";
                permissions = "0400";
              };
            wg-public =
              { text = builtins.readFile ../wireguard-keys/vserver/public;
                user = "root";
                group = "root";
                permissions = "0400";
              };
          };
      });
  T470p = hostFromConfig "T470p" ({...}: {});
  T470s = hostFromConfig "T470s" ({...}: {});
}
