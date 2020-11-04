with (import ../lib.nix);
mkHostNixops "workstation" ({ lib, ... }: {
  config = { deployment.targetHost = lib.mkDefault "10.199.199.5"; };
  imports = [
    (fixIp "workstation" "enp39s0")
    ../secrets/common/wifi.home.nix
    (deployWireguardKeys "workstation")
    (deploySSHUserKeys "workstation" "rsa")
    (setupNixServe [
      (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
      (getSecret "vserver" "ssh/id_rsa.pub")
    ])
    (setupAsBuildMachine [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub") ])
    # (setupAsBackupTarget
    #    "/mnt/4x500/backup"
    #    [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub")
    #    ])
    (setupSyncthing "workstation" ((mkSyncthingDevice "x1extremeG2" true)
      // (mkSyncthingDevice "nas" false) // (mkSyncthingDevice "vserver" false)
      // (import ../secrets/common/syncthing.SM-G960F.nix)) {
        "/home/mhuber/Sync" = {
          id = "sync";
          devices = [ "x1extremeG2" ];
          type = "sendreceive";
        };
      })
  ];
})
