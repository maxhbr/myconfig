# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  wgIp = myconfig.metadatalib.getWgIp cfg.host_hostname;
in
{
  options.myconfig.observability.host = with lib; {
    retentionPeriod = mkOption {
      type = types.str;
      default = "90d";
      description = "VictoriaMetrics retention period.";
    };

    grafana = {
      adminPassword = mkOption {
        type = types.str;
        default = "admin";
        description = ''
          Initial Grafana admin password (placeholder, override via
          private overlay).
        '';
      };
      secretKey = mkOption {
        type = types.str;
        default = "SW2YcwTIb9zpOOhoPsMm";
        description = ''
          Grafana secret_key (placeholder, override via private overlay).
        '';
      };
    };
  };

  config = lib.mkIf hostCfg.enable {
    services.victoriametrics = {
      enable = true;

      listenAddress = "${wgIp}:${toString cfg.remoteWritePort}";
      retentionPeriod = hostCfg.retentionPeriod;

      basicAuthUsername = cfg.basicAuthUsername;
      basicAuthPasswordFile = toString cfg.basicAuthPasswordFile;
    };

    services.grafana = {
      enable = true;

      settings = {
        server = {
          domain = "localhost";
          http_addr = "127.0.0.1";
          http_port = cfg.grafanaPort;
        };

        security = {
          admin_user = "admin";
          admin_password = lib.mkDefault hostCfg.grafana.adminPassword;
          secret_key = lib.mkDefault hostCfg.grafana.secretKey;
        };

        "auth.anonymous" = {
          enabled = false;
        };
      };

      provision.datasources.settings.datasources = [
        {
          name = "VictoriaMetrics";
          type = "prometheus";
          url = "http://${wgIp}:${toString cfg.remoteWritePort}";
          access = "proxy";
          isDefault = true;

          basicAuth = true;
          basicAuthUser = cfg.basicAuthUsername;
          secureJsonData = {
            basicAuthPassword = "$__file{${toString cfg.basicAuthPasswordFile}}";
          };
        }
        {
          name = "Loki";
          type = "loki";
          url = "http://${wgIp}:${toString cfg.lokiPort}";
          access = "proxy";
          isDefault = false;
        }
      ];
    };

    networking.firewall.allowedTCPPorts = [
      cfg.remoteWritePort # VictoriaMetrics remote_write endpoint
      cfg.grafanaPort # Grafana UI (only if you want it reachable directly over LAN)
      cfg.lokiPort # Loki HTTP endpoint
    ];
  };
}
