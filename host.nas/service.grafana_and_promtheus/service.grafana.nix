# https://community.grafana.com/t/installing-on-nixos/6712/2
{ config, pkgs, lib, ... }:
let
  useReverseProxy = false;
in {
  config = {
    services = {
      grafana = {
        enable = true;
        domain = config.networking.hostName;
        port = 2342;
        addr = if useReverseProxy
               then "127.0.0.1"
               else "0.0.0.0";
        rootUrl = "%(protocol)s://%(domain)s:%(http_port)s/";
        protocol = "http";
        dataDir = "/var/lib/grafana";
      };

      nginx.virtualHosts = lib.mkIf useReverseProxy {
        "${config.services.grafana.domain}" = {
          locations."/grafana/" = {
            proxyPass = "http://${config.services.grafana.addr}:${toString config.services.grafana.port}/";
            proxyWebsockets = true;
          };
        };
      };
    };
    networking.firewall = lib.mkIf (! useReverseProxy) {
      allowedTCPPorts = [ config.services.grafana.port ];
    };
    systemd.services.grafana = {
      # wait until all network interfaces initialize before starting Grafana
      after = [ "network-interfaces.target" ];
      wants = [ "network-interfaces.target" ];
    };
  };
}
