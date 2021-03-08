with (import ../lib.nix);
mkHostNixops "nas" ({ config, lib, ... }: {
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
    # (setupBuildSlave (getSecret "workstation" "ip") 2 [ "x86_64-linux" ]
    #   (getSecret "x1extremeG2" "ssh/id_ed25519")
    #   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEdhwPve+1dfpOwUKZ5c1Js/1sQeQGe1yvfcfGm0pk9W")
    (setupAsBackupTarget "/mnt/2x4t/backup"
      [ (getSecret "x1extremeG2" "ssh/id_ed25519.pub") ])
    (fixIp "nas" "enp3s0")
    {
      system.activationScripts.mkTlsDir =
        "mkdir -p /etc/tls && chmod 777 /etc/tls";
    }
    (lib.mkIf config.services.nextcloud.enable {
      deployment.keys = {
        adminpass = {
          text = getSecret "nas" "nextcloud/adminpass";
          destDir = "/etc/nextcloud";
          user = "nextcloud";
          group = "root";
          permissions = "0440";
        };
      };
    })
    (lib.mkIf config.services.nginx.enable {
      deployment.keys = {
        "nginx.crt" = {
          text = getSecret "nas" "tls/nginx.crt";
          destDir = "/etc/tls";
          user = "nginx";
          group = "root";
          permissions = "0440";
        };
        "nginx.key" = {
          text = getSecret "nas" "tls/nginx.key";
          destDir = "/etc/tls";
          user = "nginx";
          group = "root";
          permissions = "0440";
        };
      };
    })
    (lib.mkIf config.services.haproxy.enable {
      deployment.keys = {
        "nginx.pem" = {
          text = getSecret "nas" "tls/nginx.pem";
          destDir = "/etc/tls";
          user = "haproxy";
          group = "root";
          permissions = "0440";
        };
      };
    })
    (lib.mkIf config.services.grafana.enable {
      deployment.keys = {
        "grafana-adminPasswordFile" = {
          text = getSecret "nas" "grafana/adminPasswordFile";
          destDir = "/etc";
          user = "grafana";
          group = "root";
          permissions = "0440";
        };
      };
    })
    (lib.mkIf config.services.prometheus.enable {
      deployment.keys = {
        "nextcloud-exporter-pass" = {
          text = getSecret "nas" "nextcloud/nextcloud-exporter-pass";
          destDir = "/etc/nextcloud";
          user = "prometheus";
          group = "root";
          permissions = "0440";
        };
      };
    })
    (setupSyncthing "nas" ((mkSyncthingDevice "x1extremeG2" true)
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
