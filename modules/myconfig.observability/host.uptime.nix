# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Uptime / black-box monitoring for every URL in
# `myconfig.deployedServices.services`.
#
# Architecture:
#   * `prometheus-blackbox-exporter` runs on the observability host on
#     127.0.0.1, configured to perform HTTPS probes that ignore TLS
#     issues (Caddy uses `tls internal` so all probed sites have
#     self-signed certs).
#   * The local `vmagent` (provided by the observability *client*
#     module, which the host should also enable) gets an extra
#     `blackbox` scrape job whose targets are derived from
#     `config.myconfig.deployedServices.services` across all hosts.
#
# The Grafana dashboard for these metrics is provisioned by
# ./host.home-lab-status.nix together with the NixOS system-age panels.
#
# Metrics produced (per target):
#   * `probe_success`          1/0 - did the probe pass all checks?
#   * `probe_http_status_code` raw HTTP status code
#   * `probe_duration_seconds` total probe time
#   * `probe_ssl_earliest_cert_expiry` cert expiry (informational, since
#                                       TLS verification is disabled)
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  uptimeCfg = hostCfg.uptime;

  allServices = config.myconfig.deployedServices.services;

  # Build the list of URLs to probe from the deployedServices catalog.
  # Each service is reachable through Caddy at
  #   https://${name}.${host}.wg0.maxhbr.local
  # regardless of `forceHttps` (Caddy serves both, but HTTPS always
  # exists). We always probe HTTPS so the probe also exercises the
  # reverse proxy + TLS termination.
  urlsForHost = host: map (s: "https://${s.name}.${host}.wg0.maxhbr.local/") allServices.${host};

  probeTargets = lib.concatMap urlsForHost (lib.attrNames allServices);

  blackboxConfig = {
    modules = {
      # HTTP(S) probe used for every deployedService URL.
      # `insecure_skip_verify` is mandatory because Caddy issues
      # self-signed certs via `tls internal`.
      http_2xx = {
        prober = "http";
        timeout = "5s";
        http = {
          preferred_ip_protocol = "ip4";
          valid_http_versions = [
            "HTTP/1.1"
            "HTTP/2.0"
          ];
          # 401/403 still mean "service is up and responding"; many
          # probed services (open-webui, qdrant, ...) require auth.
          valid_status_codes = [
            200
            201
            204
            301
            302
            303
            307
            308
            401
            403
          ];
          method = "GET";
          fail_if_ssl = false;
          fail_if_not_ssl = false;
          tls_config = {
            insecure_skip_verify = true;
          };
          follow_redirects = true;
        };
      };
    };
  };

  blackboxConfigFile = pkgs.writeText "blackbox-exporter.yml" (builtins.toJSON blackboxConfig);
in
{
  options.myconfig.observability.host.uptime = with lib; {
    enable = mkEnableOption "uptime monitoring of myconfig.deployedServices via blackbox_exporter";

    blackboxPort = mkOption {
      type = types.port;
      default = 9115;
      description = "Loopback port the blackbox_exporter listens on.";
    };

    scrapeInterval = mkOption {
      type = types.str;
      default = "30s";
      description = "How often vmagent should run blackbox probes.";
    };

    extraTargets = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [ "https://example.com/" ];
      description = "Additional URLs to probe beyond myconfig.deployedServices.";
    };
  };

  config = lib.mkIf (hostCfg.enable && uptimeCfg.enable) {
    assertions = [
      {
        assertion = cfg.client.enable;
        message = ''
          myconfig.observability.host.uptime requires
          myconfig.observability.client.enable = true on the same host
          (the local vmagent runs the probe scrape job and pushes the
          results into VictoriaMetrics).
        '';
      }
    ];

    services.prometheus.exporters.blackbox = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = uptimeCfg.blackboxPort;
      configFile = blackboxConfigFile;
    };

    # Add a single `blackbox` scrape job to the local vmagent. It uses
    # the standard relabeling pattern: the probe URL travels through
    # `__param_target` and is rewritten into the actual scrape target
    # `127.0.0.1:<blackboxPort>`. The original URL ends up as the
    # `instance` label, which makes dashboards readable.
    services.vmagent.prometheusConfig.scrape_configs = [
      {
        job_name = "blackbox";
        metrics_path = "/probe";
        scrape_interval = uptimeCfg.scrapeInterval;
        params = {
          module = [ "http_2xx" ];
        };
        static_configs = [
          { targets = probeTargets ++ uptimeCfg.extraTargets; }
        ];
        relabel_configs = [
          {
            source_labels = [ "__address__" ];
            target_label = "__param_target";
          }
          {
            source_labels = [ "__param_target" ];
            target_label = "instance";
          }
          # Extract the probed host (e.g. "nuc", "thing") from the
          # deployedServices URL pattern so dashboards can filter by
          # host symmetrically with system-level metrics. Targets that
          # don't match the pattern (e.g. `extraTargets` pointing at
          # external URLs) get no label.
          {
            source_labels = [ "__param_target" ];
            regex = "https?://[^.]+\\.([^.]+)\\.wg0\\.maxhbr\\.local/.*";
            target_label = "probed_host";
            replacement = "\${1}";
          }
          {
            target_label = "__address__";
            replacement = "127.0.0.1:${toString uptimeCfg.blackboxPort}";
          }
        ];
      }
    ];
  };
}
