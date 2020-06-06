let
  vserverIp = "10.199.199.1";
  x1extremeG2Ip = "10.199.199.2";
  workstationIp = "10.199.199.5";
in
with (import ./lib.nix);
{ network.description = "myconfig";
  vserver = mkHost "vserver"
    ( {lib, ...}:
      { config =
          { deployment.targetHost = lib.mkDefault vserverIp;
          };
        imports =
          [ (deployWireguardKeys "vserver")
            (deploySSHUserKeys "vserver" "rsa")
          ];
      });
  x1extremeG2 = mkHost "x1extremeG2"
    ( {lib, ...}:
      { config =
          { deployment.targetHost = lib.mkDefault x1extremeG2Ip;
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
              nix.trustedBinaryCaches = [ ("ssh://nix-ssh@${workstationIp}") ];
            }
            (setupBuildSlave (getSecret "workstation" "ip") 2 (getSecret "x1extremeG2" "ssh/id_ed25519") "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdhwPve+1dfpOwUKZ5c1Js/1sQeQGe1yvfcfGm0pk9W")
            (setupBuildSlave workstationIp 0.5 (getSecret "x1extremeG2" "ssh/id_ed25519") "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdhwPve+1dfpOwUKZ5c1Js/1sQeQGe1yvfcfGm0pk9W")
          ];
       });
  workstation = mkHost "workstation"
    ( {lib, ...}:
      { config =
          { deployment.targetHost = lib.mkDefault workstationIp;
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
            (setupAsBuildMachine
               [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
               ])
          ];
      });
  nas = mkHost "nas"
    ( {lib, ...}:
      { config =
          {
          };
        imports =
          [ (deployWireguardKeys "nas")
            (deploySSHUserKeys "nas" "rsa")
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
