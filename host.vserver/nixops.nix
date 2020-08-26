with (import ../lib.nix);
mkHostNixops "vserver"
( {lib, ...}:
  { config =
      { deployment.targetHost = lib.mkDefault "10.199.199.1";
      };
    imports =
      [ (deployWireguardKeys "vserver")
        (deploySSHUserKeys "vserver" "rsa")
        (setupSyncthing "vserver"
           ( (mkSyncthingDevice "x1extremeG2" false) //
             (mkSyncthingDevice "workstation" false) //
             (mkSyncthingDevice "nas" false) //
             (import ../secrets/common/syncthing.SM-G960F.nix)
           )
           { "/home/mhuber/Sync" =
               { id = "sync";
                 devices = [ "x1extremeG2" ];
                 type = "sendreceive";
               };
           })
      ];
  }
)
