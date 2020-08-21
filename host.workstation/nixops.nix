with (import ../lib.nix);
mkHostNixops "workstation"
( {lib, ...}:
  { config =
      { deployment.targetHost = lib.mkDefault "10.199.199.5";
        services.wakeonlan.interfaces =
          [ { interface = "enp39s0";
              method = "magicpacket";
          } ];
      };
    imports =
      [ (fixIp "workstation" "enp39s0")
        ../secrets/common/wifi.QS3j.nix
        (deployWireguardKeys "workstation")
        (deploySSHUserKeys "workstation" "rsa")
        (setupNixServe
           [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
             (getSecret "vserver" "ssh/id_rsa.pub")
           ])
        (setupAsBuildMachine
           [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
           ])
        # (setupAsBackupTarget
        #    "/mnt/4x500/backup"
        #    [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
        #    ])
      ];
  }
)
