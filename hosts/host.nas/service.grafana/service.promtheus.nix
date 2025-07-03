{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = {
    services = {
      prometheus = {
        enable = true;
        listenAddress = "127.0.0.1";
        webExternalUrl = "https://${config.networking.hostName}/prometheus/";
        port = 9090;
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
            port = 9100;
            openFirewall = true;
            firewallFilter = "-i br0 -p tcp -m tcp --dport 9100";
          };
        };
        scrapeConfigs = [
          {
            job_name = config.networking.hostName;
            static_configs = [
              {
                targets = [
                  "${config.services.prometheus.listenAddress}:${toString config.services.prometheus.exporters.node.port}"
                ];
              }
            ];
          }
        ];
      };
      grafana.provision = {
        enable = true;
        datasources = [
          {
            name = "prometheus";
            type = "prometheus";
            url = "http://${config.services.prometheus.listenAddress}:9090/prometheus/";
            isDefault = true;
          }
        ];
      };
    };
  };
}
