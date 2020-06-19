with (import ../lib.nix);
mkHostNixops "nas"
( {lib, ...}:
  { config =
      { deployment.targetHost = lib.mkDefault "10.199.199.6";
      };
    imports =
      [ (deployWireguardKeys "nas")
        (deploySSHUserKeys "nas" "rsa")
        (setupNixServe
          [ (getSecret "workstation" "ssh/id_rsa.pub")
            (getSecret "vserver" "ssh/id_rsa.pub")
          ])
        {
          nix.trustedBinaryCaches =
            [ ("ssh://nix-ssh@" + (getSecret "workstation" "ip"))
            ];
        }
        (setupBuildSlave (getSecret "workstation" "ip") 2 (getSecret "x1extremeG2" "ssh/id_ed25519") "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdhwPve+1dfpOwUKZ5c1Js/1sQeQGe1yvfcfGm0pk9W")
        (setupAsBackupTarget
           "/mnt/2x4t/backup"
           [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
           ])
      ];
  }
)
