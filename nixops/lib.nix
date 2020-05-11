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
                ipFile = (secretsDir + "/${hostName}/ip")
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
}
