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
    services = {
      nextcloud = {
        enable = true;
        hostName = config.networking.hostName;
        package = pkgs.nextcloud20;
        home = "/mnt/2x4t/nextcloud";
        https = true;
        autoUpdateApps.enable = true;
        autoUpdateApps.startAt = "05:00:00";
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
      postgresql = {
        enable = true;
        ensureDatabases = [ "nextcloud" ];
        ensureUsers = [{
          name = "nextcloud";
          ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
        }];
      };
    };
    systemd.services."nextcloud-setup" = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };
    systemd.services.nginx = {
      # wait until all network interfaces initialize before starting Grafana
      after = [ "nextcloud.target" ];
      wants = [ "nextcloud.target" ];
    };
  };
}
