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
  };

  imports = [
    ./client.dcgm-exporter.nix
    ./client.system-age.nix
    ./client.co2-exporter.nix
    ./client.weather-exporter.nix
  ];

  config = lib.mkIf clientCfg.enable {
    services.prometheus.exporters.node = {
      enable = true;
      port = clientCfg.nodeExporterPort;

      # Important: only local vmagent can scrape this.
      listenAddress = "127.0.0.1";

      enabledCollectors = clientCfg.enabledNodeCollectors;

      # Point the textfile collector at the directory managed by
      # client.system-age.nix (and any other module that drops a
      # `*.prom` file there). Only configure the flag when the
      # system-age sub-module is actually enabled, otherwise leave
      # node_exporter at its defaults.
      extraFlags = lib.optionals clientCfg.systemAge.enable [
        "--collector.textfile.directory=${clientCfg.systemAge.textfileDir}"
      ];
    };

    services.vmagent = {
      enable = true;

      # Bind the self-scrape HTTP endpoint to loopback only — no
      # external consumer needs this port.
      extraArgs = [ "-httpListenAddr=127.0.0.1:${toString clientCfg.vmagentPort}" ];

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
        ++ lib.optionals config.services.litellm.enable (
          let
            # `host` may be a wildcard (e.g. "0.0.0.0") for external exposure;
            # use localhost for the in-host scrape target.
            litellmHost =
              if config.services.litellm.host == "0.0.0.0" then "localhost" else config.services.litellm.host;
          in
          [
            {
              job_name = "litellm";
              metrics_path = "/metrics";
              static_configs = [
                { targets = [ "${litellmHost}:${toString config.services.litellm.port}" ]; }
              ];
            }
          ]
        )
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
