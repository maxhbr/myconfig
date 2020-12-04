{ pkgs, config, lib, ... }: {
  imports = [
    (lib.mkIf config.services.prometheus.enable {
      services.prometheus.exporters.nextcloud = {
        url = "https://localhost:443";
        username = "nextcloud-exporter";
        passwordFile = "/etc/nextcloud/nextcloud-exporter-pass";
      };
    })
  ];
  config = {
    services.nextcloud = {
      enable = true;
      hostName = config.networking.hostName;
      package = pkgs.nextcloud19;
      home = "/mnt/2x4t/nextcloud";
      https = true;
      config = {
        dbtype = "pgsql";
        dbuser = "nextcloud";
        dbhost = "/run/postgresql";
        dbname = "nextcloud";
        adminuser = "Admin";
        adminpassFile = "/etc/nextcloud/adminpass";
        extraTrustedDomains = (with (import ../lib.nix);
          makeOptionalListBySecrets [
            (getSecretNoNewline "${config.networking.hostName}" "ip")
            "10.199.199.6"
          ]);
        overwriteProtocol = "https";
      };
      maxUploadSize = "20G";
    };
    services.nginx.virtualHosts."${config.networking.hostName}" = {
      addSSL = true;
      sslCertificate = "/etc/nextcloud/nextcloud.crt";
      sslCertificateKey = "/etc/nextcloud/nextcloud.key";
      listen = map (addr: {
        inherit addr;
        port = 443;
        ssl = true;
      }) (config.services.nextcloud.config.extraTrustedDomains
        ++ [ config.services.nextcloud.hostName ]);
    };
    services.postgresql = {
      enable = true;
      ensureDatabases = [ "nextcloud" ];
      ensureUsers = [{
        name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }];
    };
    systemd.services."nextcloud-setup" = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };
    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
