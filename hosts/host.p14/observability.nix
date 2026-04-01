# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{ pkgs, ... }:
{
  # Observability - Prometheus + Grafana
  services.prometheus = {
    enable = true;
    port = 9090;
    listenAddress = "127.0.0.1";
    scrapeConfigs = [
      {
        job_name = "node_exporter";
        static_configs = [ { targets = [ "localhost:9100" ]; } ];
      }
      {
        job_name = "prometheus";
        static_configs = [ { targets = [ "localhost:9090" ]; } ];
      }
    ];
    exporters.node = {
      enable = true;
      port = 9100;
      enabledCollectors = [ "logind" ];
    };
  };
  services.grafana = {
    enable = true;
    port = 3000;
    settings = {
      server.http_addr = "127.0.0.1";
      security.admin_user = "admin";
      security.admin_password = "***";
      security.secret_key = "SW2YcwTIb9zpOOhoPsMm";
      anonymous.enabled = false;
    };
    settings.server.domain = "localhost";
  };
  networking.firewall.allowedTCPPorts = [
    9090
    9100
    3000
  ];
}
