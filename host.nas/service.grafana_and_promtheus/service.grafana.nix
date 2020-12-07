# https://community.grafana.com/t/installing-on-nixos/6712/2
{ config, pkgs, ... }: {
  config = {
    services = {
      grafana = {
        enable = true;
        addr = "127.0.0.1";
        protocol = "http";
        domain = config.networking.hostName;
        port = 2342;
        rootUrl = "%(protocol)s://%(domain)s:%(http_port)s/";
        dataDir = "/var/lib/grafana";
        auth.anonymous = {
          enable = true;
          org_name = "Org";
          org_role = "Editor";
        };
        security = {
          adminUser = "admin";
          adminPasswordFile = "/etc/grafana-adminPasswordFile";
        };
      };
    };
    systemd.services.grafana = {
      # wait until all network interfaces initialize before starting Grafana
      after = [ "network-interfaces.target" ];
      wants = [ "network-interfaces.target" ];
    };
  };
}
