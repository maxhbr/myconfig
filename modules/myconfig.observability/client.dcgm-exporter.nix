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
    enableDcgmExporter = mkEnableOption "dcgm-exporter for NVIDIA GPU metrics";
    dcgmExporterPort = mkOption {
      type = types.port;
      default = 9400;
      description = "Port the dcgm-exporter listens on (loopback only).";
    };
  };
  config = lib.mkIf (clientCfg.enable && clientCfg.enableDcgmExporter) {
    # dcgm-exporter is not yet a NixOS module in nixpkgs (only the package
    # `prometheus-dcgm-exporter` exists), so we run it as a plain systemd
    # service. Requires an NVIDIA GPU with the driver loaded.
    systemd.services.prometheus-dcgm-exporter = {
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
      prometheusConfig = {
        scrape_configs = [
          {
            job_name = "dcgm";
            static_configs = [
              { targets = [ "127.0.0.1:${toString clientCfg.dcgmExporterPort}" ]; }
            ];
          }
        ];
      };
    };
  };
}
