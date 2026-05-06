# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.myconfig.observability;
in
{
  imports = [
    ./host.nix
    ./host.loki.nix
    ./host.uptime.nix
    ./host.system-age.nix
    ./host.litellm.nix
    ./client.nix
    ./client.alloy.nix
  ];

  options.myconfig.observability = with lib; {
    client = {
      enable = mkEnableOption "myconfig.observability.client (node_exporter + vmagent + alloy)";
    };
    host = {
      enable = mkEnableOption "myconfig.observability.host (VictoriaMetrics + Loki + Grafana)";
    };

    host_hostname = mkOption {
      type = types.str;
      default = "nuc";
      description = ''
        Hostname of the central observability host that runs
        VictoriaMetrics and Grafana. Used by clients to know where to
        push metrics via vmagent remote_write.
      '';
    };

    remoteWritePort = mkOption {
      type = types.port;
      default = 8428;
      description = "Port where VictoriaMetrics accepts remote_write traffic.";
    };

    grafanaPort = mkOption {
      type = types.port;
      default = 3000;
      description = "HTTP port for the Grafana UI on the observability host.";
    };

    lokiPort = mkOption {
      type = types.port;
      default = 3100;
      description = ''
        HTTP port where Loki listens for log ingestion (push API) and
        queries from Grafana. Used by Alloy on clients to push logs.
      '';
    };

    basicAuthUsername = mkOption {
      type = types.str;
      default = "vmagent";
      description = "Basic-auth username shared between vmagent and VictoriaMetrics.";
    };

    basicAuthPasswordFile = mkOption {
      type = types.path;
      default = pkgs.writeText "victoriametrics-basic-auth-password" "unsafe:isheix9aitoo8pooThie";
      defaultText = lib.literalExpression ''pkgs.writeText "..." "unsafe:..."'';
      description = ''
        File containing the basic-auth password used for vmagent
        remote_write and VictoriaMetrics ingestion. The default is an
        insecure placeholder; override via the private overlay.
      '';
    };
  };

  config = lib.mkIf (cfg.host.enable || cfg.client.enable) {
    myconfig.deployedServices.services = {
      "${cfg.host_hostname}" = [
        {
          name = "grafana";
          port = 3000;
        }
      ];
    };
  };
}
