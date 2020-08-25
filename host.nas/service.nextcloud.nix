{ pkgs, config, ... }:
{ config =
    { services.nextcloud =
        { enable = true;
          hostName = "nas";
          package = pkgs.nextcloud19;
          home = "/mnt/2x4t/nextcloud";
          https = true;
          config =
            { dbtype = "pgsql";
              dbuser = "nextcloud";
              dbhost = "/run/postgresql";
              dbname = "nextcloud";
              #dbpassFile = "/etc/nextcloud/adminpass";
              adminuser = "Admin";
              adminpassFile = "/etc/nextcloud/adminpass";
              extraTrustedDomains =
                (with (import ../lib.nix);
                  [ (getSecretNoNewline "nas" "ip")
                    "10.199.199.6"
                  ]
                );
              overwriteProtocol = "https";
            };
          maxUploadSize = "20G";
        };
      services.nginx.virtualHosts."nas" =
        { addSSL = true;
          sslCertificate = "/etc/nextcloud/nextcloud.crt";
          sslCertificateKey = "/etc/nextcloud/nextcloud.key";
          listen =
            map (addr : {inherit addr; port = 443; ssl = true;}) (config.services.nextcloud.config.extraTrustedDomains);
        };
      services.postgresql = {
        enable = true;
        ensureDatabases = [ "nextcloud" ];
        ensureUsers = [
          { name = "nextcloud";
            ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
          }
        ];
      };
      systemd.services."nextcloud-setup" = {
        requires = ["postgresql.service"];
        after = ["postgresql.service"];
      };
      networking.firewall.allowedTCPPorts = [ 80 443 ];
    };
}
