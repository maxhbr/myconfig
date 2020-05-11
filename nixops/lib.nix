let
  secretsDir = ./secrets;
in
{ mkHost =
    hostName:
    addConfig:
    { config, lib, ... }@args:
    { imports =
        [ (../nixos/host- + hostName)
          (secretsDir + "/${hostName}")
          (addConfig args)
          { deployment =
              let
                ipFile = (secretsDir + "/${hostName}/ip");
              in lib.mkIf (builtins.pathExists ipFile)
              { targetHost = builtins.readFile ipFile;
              };
          }
          { assertions =
              [ { assertion = config.networking.hostName == hostName;
                  message = "hostname should be set!";
                }
                # { assertion = secretsConfig.users.users.mhuber.hashedPassword != null;
                #   message = "password should be overwritten in ./nixops-secrets.nix";
                # }
              ];
          }
        ];
    };
  deployWireguardKeys = hostName:
    { deployment.keys =
        { wg-private =
            { text = builtins.readFile (secretsDir + "/${hostName}/wireguard-keys/private");
              user = "root";
              group = "root";
              permissions = "0400";
            };
          wg-public =
            { text = builtins.readFile (secretsDir + "/${hostName}/wireguard-keys/public");
              user = "root";
              group = "root";
              permissions = "0444";
            };
        };
    };
  deploySSHUserKeys = hostName: algo:
    { deployment.keys =
        { "id_${algo}" =
            { text = builtins.readFile (secretsDir + "/${hostName}/ssh/id_${algo}");
              destDir = "/home/mhuber/.ssh";
              user = "mhuber";
              group = "mhuber";
              permissions = "0400";
            };
          "id_${algo}.pub" =
            { text = builtins.readFile (secretsDir + "/${hostName}/ssh/id_${algo}.pub");
              destDir = "/home/mhuber/.ssh";
              user = "mhuber";
              group = "mhuber";
              permissions = "0444";
            };
        };
    };
}
