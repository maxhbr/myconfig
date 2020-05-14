with (import ./lib.nix);
{ network.description = "myconfig";
  x1extremeG2 = mkHost "x1extremeG2"
    ( {lib, ...}:
      { config =
          { deployment.targetHost = lib.mkDefault "10.199.199.2";
            environment.shellAliases =
              { upg-workstation = "upg-fast --target workstation";
                upg-workstation-reboot = "upg-fast --target workstation --reboot";
                upg-vserver = "upg-fast --target vserver";
                upg-vserver-reboot = "upg-fast --target vserver --reboot";
              };
          };
        imports =
          [ (deployWireguardKeys "x1extremeG2")
            (deploySSHUserKeys "x1extremeG2" "rsa")
            (deploySSHUserKeys "x1extremeG2" "ed25519")
            (deploySSHUserKeys "x1extremeG2" "dsa")
            (deploySSHUserKeys "x1extremeG2" "ecdsa")
            (setupNixServe
              [ (getSecret "workstation" "ssh/id_rsa.pub")
                (getSecret "vserver" "ssh/id_rsa.pub")
              ])
            {
              nix.trustedBinaryCaches = [ ("ssh://nix-ssh@" + (getSecret "workstation" "ip")) ];
            }
            { nix.buildMachines =
                [{ hostName = (getSecret "workstation" "ip");
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
            }
          ];
       });
  workstation = mkHost "workstation"
    ( {lib, ...}:
      { config =
          { deployment.targetHost =  lib.mkDefault "10.199.199.5";
            services.wakeonlan.interfaces =
              [ { interface = "enp39s0";
                  method = "magicpacket";
              } ];
          };
        imports =
          [ (deployWireguardKeys "workstation")
            (deploySSHUserKeys "workstation" "rsa")
            (setupNixServe
              [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
                (getSecret "vserver" "ssh/id_rsa.pub")
              ])
          ];
      });
  vserver = mkHost "vserver"
    ( {lib, ...}:
      { config =
          { deployment.targetHost = lib.mkDefault "10.199.199.1";
          };
        imports =
          [ (deployWireguardKeys "vserver")
            (deploySSHUserKeys "vserver" "rsa")
          ];
      });
  # T470p = mkHost "T470p"
  #   ({...}: {});
  T470s = mkHost "T470s"
    ( {...}:
      { imports =
          [ (deployWireguardKeys "T470s")
          ];
      });
}
