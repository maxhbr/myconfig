let
  secretsPath = ../secrets-myconfig;
  secretsLib = import ./nixops-lib.nix secretsPath;
  secrets = import secretsPath secretsLib;
  hostFromConfig = hostName: addConfig:
    { config, lib, ... }@args:
    let
      secretsConfig = secrets."${hostName}" args;
    in
    { config =
        { deployment.targetEnv = "none";
          assertions =
            [ { assertion = config.networking.hostName == hostName;
                message = "hostname should be set!";
              }
              { assertion = secretsConfig.users.users.mhuber.hashedPassword != null;
                message = "password should be overwritten in ./nixops-secrets.nix";
              }
            ];
        } // (lib.mkMerge secretsConfig);
      imports =
        [ (./nixos/host- + hostName)
          (addConfig args)
        ];
    };
  workstationAsBuildMachine =
    { nix.buildMachines =
        [{ hostName = "workstation";
           system = "x86_64-linux";
           maxJobs = 6;
           speedFactor = 2;
           supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
           mandatoryFeatures = [ ];
        }];
      nix.distributedBuilds = true;
      # optional, useful when the builder has a faster internet connection than yours
      nix.extraOptions =
        '' builders-use-substitutes = true
        '';
    };
in
{ network.description = "myconfig";
  x1extremeG2 = hostFromConfig "x1extremeG2"
    ( {lib, ...}:
      { deployment.targetHost = lib.mkDefault "10.199.199.2";
        environment.shellAliases =
          { upg-workstation = "upg-fast --target workstation";
            upg-workstation-reboot = "upg-fast --target workstation --reboot";
            upg-vserver = "upg-fast --target vserver";
            upg-vserver-reboot = "upg-fast --target vserver --reboot";
          };
      } // workstationAsBuildMachine );
  workstation = hostFromConfig "workstation"
    ( {lib, ...}:
      { deployment.targetHost =  lib.mkDefault "10.199.199.5";
      });
  vserver = hostFromConfig "vserver"
    ( {lib, ...}:
      { deployment.targetHost = lib.mkDefault "10.199.199.1";
      });
  T470p = hostFromConfig "T470p"
    ({...}: {});
  T470s = hostFromConfig "T470s"
    ({...}: {});
}
