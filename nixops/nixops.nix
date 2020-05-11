# let
#   workstationAsBuildMachine =
#     { nix.buildMachines =
#         [{ hostName = "workstation";
#            system = "x86_64-linux";
#            maxJobs = 6;
#            speedFactor = 2;
#            supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
#            mandatoryFeatures = [ ];
#         }];
#       nix.distributedBuilds = true;
#       # optional, useful when the builder has a faster internet connection than yours
#       nix.extraOptions =
#         '' builders-use-substitutes = true
#         '';
#     };
# in
with (import ./lib.nix);
{ network.description = "myconfig";
  x1extremeG2 = mkHost "x1extremeG2"
    ( {lib, ...}:
      { deployment.targetHost = lib.mkDefault "10.199.199.2";
        environment.shellAliases =
          { upg-workstation = "upg-fast --target workstation";
            upg-workstation-reboot = "upg-fast --target workstation --reboot";
            upg-vserver = "upg-fast --target vserver";
            upg-vserver-reboot = "upg-fast --target vserver --reboot";
          };
      });
  workstation = mkHost "workstation"
    ( {lib, ...}:
      { deployment.targetHost =  lib.mkDefault "10.199.199.5";
      });
  vserver = mkHost "vserver"
    ( {lib, ...}:
      { deployment.targetHost = lib.mkDefault "10.199.199.1";
      });
  T470p = mkHost "T470p"
    ({...}: {});
  T470s = mkHost "T470s"
    ({...}: {});
}
