{ pkgs, config, lib, myconfig, ... }: {
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
    myconfig.secrets = {
      "nextcloud-pass" = {
        dest = "/etc/nextcloud/adminpass";
        owner = "nextcloud";
        group = "nextcloud";
        permissions = "0440";
      };
    };
    services = {
      nextcloud = {
        enable = true;
        hostName = config.networking.hostName;
        adminpassFile = "/etc/nextcloud/adminpass";
        package = pkgs.nextcloud22;
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
          extraTrustedDomains = [
            myconfig.metadatalib.get.hosts."${config.networking.hostName}".ip4
            myconfig.metadatalib.get.hosts."${config.networking.hostName}".wireguard.wg0.ip4
          ];
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
