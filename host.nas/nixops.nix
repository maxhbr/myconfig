with (import ../lib.nix);
mkHostNixops "nas" ({ lib, ... }: {
  config = { deployment.targetHost = lib.mkDefault "10.199.199.6"; };
  imports = [
    (deployWireguardKeys "nas")
    (deploySSHUserKeys "nas" "rsa")
    (setupNixServe [
      (getSecret "workstation" "ssh/id_rsa.pub")
      (getSecret "vserver" "ssh/id_rsa.pub")
    ])
    {
      nix.trustedBinaryCaches =
        [ ("ssh://nix-ssh@" + (getSecret "workstation" "ip")) ];
    }
    (setupBuildSlave (getSecret "workstation" "ip") 2 [ "x86_64-linux" ]
      (getSecret "x1extremeG2" "ssh/id_ed25519")
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdhwPve+1dfpOwUKZ5c1Js/1sQeQGe1yvfcfGm0pk9W")
    (setupAsBackupTarget "/mnt/2x4t/backup"
      [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub") ])
    (fixIp "nas" "enp3s0")
    {
      deployment.keys = {
        adminpass = {
          text = getSecret "nas" "nextcloud/adminpass";
          destDir = "/etc/nextcloud";
          user = "nextcloud";
          group = "root";
          permissions = "0440";
        };
        "nextcloud.crt" = {
          text = getSecret "nas" "nextcloud/nextcloud.crt";
          destDir = "/etc/nextcloud";
          user = "nginx";
          group = "root";
          permissions = "0440";
        };
        "nextcloud.key" = {
          text = getSecret "nas" "nextcloud/nextcloud.key";
          destDir = "/etc/nextcloud";
          user = "nginx";
          group = "root";
          permissions = "0440";
        };
        "nextcloud-exporter-pass" = {
          text = getSecret "nas" "nextcloud/nextcloud-exporter-pass";
          destDir = "/etc/nextcloud";
          user = "prometheus";
          group = "root";
          permissions = "0440";
        };
      };
    }
    (setupSyncthing "nas" (
      (mkSyncthingDevice "x1extremeG2" true)
      // (mkSyncthingDevice "workstation" false)
      // (mkSyncthingDevice "vserver" false)
      // (import ../secrets/common/syncthing.Pixel5.nix)) {
        "/mnt/2x4t/Sync" = {
          id = "sync";
          devices = [ "x1extremeG2" ];
          type = "sendreceive";
        };
      })
  ];
})
