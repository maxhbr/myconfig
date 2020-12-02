# https://community.grafana.com/t/installing-on-nixos/6712/2
{ config, pkgs, ... }:
{
  config = {
    services = {
      grafana = {
        enable = true;
        domain = config.networking.hostName;
        port = 2342;
        addr = "127.0.0.1";
        rootUrl = "%(protocol)s://%(domain)s:%(http_port)s/grafana/";
        protocol = "http";
        dataDir = "/var/lib/grafana";
      };

      nginx.virtualHosts."${config.services.grafana.domain}" = {
        locations."/grafana" = {
          proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
          proxyWebsockets = true;
        };
      };
    };
    networking.firewall = {
      allowedTCPPorts = [ config.services.grafana.port ];
    };
    systemd.services.grafana = {
      # wait until all network interfaces initialize before starting Grafana
      after = [ "network-interfaces.target" ];
      wants = [ "network-interfaces.target" ];
    };
  };
}
