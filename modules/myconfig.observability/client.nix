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
  };

  config = lib.mkIf clientCfg.enable {
    services.prometheus.exporters.node = {
      enable = true;
      port = clientCfg.nodeExporterPort;

      # Important: only local vmagent can scrape this.
      listenAddress = "127.0.0.1";

      enabledCollectors = clientCfg.enabledNodeCollectors;
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
