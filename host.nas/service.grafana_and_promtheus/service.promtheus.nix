{ config, pkgs, lib, ... }:
let
  useReverseProxy = false;
in {
  config = {
    services = {
      prometheus = {
        enable = true;
        listenAddress = if useReverseProxy
                        then "127.0.0.1"
                        else "0.0.0.0";
        webExternalUrl = "https://nas/prometheus/";
        port = 9001;
        exporters = {
          node = {
            enable = true;
            enabledCollectors = [
              # mine:
              # "unifi"
              # "nextcloud"
              # "wireguard"

              # general
              "conntrack"
              "diskstats"
              "entropy"
              "filefd"
              "filesystem"
              "loadavg"
              "mdadm"
              "meminfo"
              "netdev"
              "netstat"
              "stat"
              "time"
              "vmstat"
              "systemd"
              "logind"
              "interrupts"
              "ksmd"
            ];
            port = 9002;
          };
        };
        scrapeConfigs = [
          {
            job_name = config.networking.hostName;
            static_configs = [{
              targets = [ "${config.services.prometheus.listenAddress}:${toString config.services.prometheus.exporters.node.port}" ];
            }];
          }
        ];
      };
      grafana.provision = {
        enable = true;
        datasources = [
          {
            name = "prometheus";
            type = "prometheus";
            url = "http://localhost:9001";
            isDefault = true;
          }
        ];
      };
      nginx.virtualHosts = lib.mkIf useReverseProxy {
        "${config.networking.hostName}" = {
          locations."/prometheus" = {
            proxyPass = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
            proxyWebsockets = true;
          };
        };
      };
    };
    networking.firewall = lib.mkIf (! useReverseProxy) {
      allowedTCPPorts = [ config.services.prometheus.port ];
    };
  };
}
