# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  wgIp = myconfig.metadatalib.getWgIp cfg.host_hostname;
in
{
  options.myconfig.observability.client = with lib; {
    nodeExporterPort = mkOption {
      type = types.port;
      default = 9100;
      description = "Port the node_exporter listens on (loopback only).";
    };

    vmagentPort = mkOption {
      type = types.port;
      default = 8429;
      description = "Port the vmagent self-scrape endpoint listens on.";
    };

    scrapeInterval = mkOption {
      type = types.str;
      default = "15s";
      description = "Prometheus scrape interval used by vmagent.";
    };

    enabledNodeCollectors = mkOption {
      type = types.listOf types.str;
      default = [
        "logind"
        "systemd"
      ];
      description = "Extra collectors to enable in node_exporter.";
    };

    enableDcgmExporter = mkEnableOption "dcgm-exporter for NVIDIA GPU metrics";

    dcgmExporterPort = mkOption {
      type = types.port;
      default = 9400;
      description = "Port the dcgm-exporter listens on (loopback only).";
    };
  };

  config = lib.mkIf clientCfg.enable {
    services.prometheus.exporters.node = {
      enable = true;
      port = clientCfg.nodeExporterPort;

      # Important: only local vmagent can scrape this.
      listenAddress = "127.0.0.1";

      enabledCollectors = clientCfg.enabledNodeCollectors;
    };

    # dcgm-exporter is not yet a NixOS module in nixpkgs (only the package
    # `prometheus-dcgm-exporter` exists), so we run it as a plain systemd
    # service. Requires an NVIDIA GPU with the driver loaded.
    systemd.services.prometheus-dcgm-exporter = lib.mkIf clientCfg.enableDcgmExporter {
      description = "Prometheus dcgm-exporter (NVIDIA GPU metrics)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.prometheus-dcgm-exporter}/bin/dcgm-exporter --address=127.0.0.1:${toString clientCfg.dcgmExporterPort}";
        Restart = "on-failure";
        RestartSec = "5s";
        DynamicUser = true;
        SupplementaryGroups = [ "video" ];
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
      };
    };

    services.vmagent = {
      enable = true;

      prometheusConfig = {
        global = {
          scrape_interval = clientCfg.scrapeInterval;

          external_labels = {
            host = config.networking.hostName;
          };
        };

        scrape_configs = [
          {
            job_name = "node";
            static_configs = [
              { targets = [ "127.0.0.1:${toString clientCfg.nodeExporterPort}" ]; }
            ];
          }
          {
            job_name = "vmagent";
            static_configs = [
              { targets = [ "127.0.0.1:${toString clientCfg.vmagentPort}" ]; }
            ];
          }
        ]
        ++ lib.optionals clientCfg.enableDcgmExporter [
          {
            job_name = "dcgm";
            static_configs = [
              { targets = [ "127.0.0.1:${toString clientCfg.dcgmExporterPort}" ]; }
            ];
          }
        ]
        ++ lib.optionals config.services.litellm.enable [
          {
            job_name = "litellm";
            metrics_path = "/metrics";
            static_configs = [
              { targets = [ "${config.services.litellm.host}:${toString config.services.litellm.port}" ]; }
            ];
          }
        ]
        # When netdata is enabled, scrape its Prometheus endpoint.
        # See https://www.netdata.cloud/blog/netdata-prometheus-grafana-stack/
        ++ lib.optionals config.services.netdata.enable [
          {
            job_name = "netdata";
            metrics_path = "/api/v1/allmetrics";
            params = {
              format = [ "prometheus_all_hosts" ];
              help = [ "no" ];
            };
            honor_labels = true;
            static_configs = [
              { targets = [ "127.0.0.1:19999" ]; }
            ];
          }
        ];
      };

      remoteWrite = {
        url = "http://${wgIp}:${toString cfg.remoteWritePort}/api/v1/write";

        # Depending on your nixpkgs vmagent module version, basic auth may either
        # be supported here or may need to go through extraArgs.
        basicAuthUsername = cfg.basicAuthUsername;
        basicAuthPasswordFile = toString cfg.basicAuthPasswordFile;
      };
    };
  };
}
