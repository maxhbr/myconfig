# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Prometheus scrape job for llama-swap's own native `/metrics`
# endpoint.
#
# Recent llama-swap builds expose their own Prometheus endpoint —
# distinct from the OpenAI-compatible `/v1/models` JSON endpoint
# scraped by ``client.llama-server.nix`` via a separate Python
# exporter — with host-level system gauges gathered by the llama-swap
# process itself:
#
#   * llamaswap_cpu_util_percent{core}                     per-core CPU utilisation (%)
#   * llamaswap_memory_total_bytes                         total system memory (bytes)
#   * llamaswap_memory_used_bytes                          used system memory (bytes)
#   * llamaswap_memory_free_bytes                          free system memory (bytes)
#   * llamaswap_swap_total_bytes                           total swap (bytes)
#   * llamaswap_swap_used_bytes                            used swap (bytes)
#   * llamaswap_load_average{interval="1m"|"5m"|"15m"}     load average
#   * llamaswap_network_bytes_total{interface,direction}   cumulative network I/O (counter)
#
# Verified against the live endpoint on host `thing`
# (``curl -k https://gfx1151.thing.wg0.maxhbr.local/metrics``) — the
# metric names/labels above are exactly what that instance reports,
# not upstream documentation guesses.
#
# This module intentionally does NOT try to auto-derive the scrape
# target from ``myconfig.ai.llama-cpp``, unlike
# ``client.llama-server.nix``: on `thing` the llama-swap instance for
# gfx1151 runs inside a NixOS container
# (``containers.llama-cpp-33657``) whose config tree lives at
# ``config.containers.llama-cpp-33657.config.*``, not at the
# top-level ``config.myconfig.ai.llama-cpp`` that
# ``client.llama-server.nix`` reads (that one is populated by the
# host's *own* llama-cpp instance, e.g. rtx5090 on port 33656 — a
# separate service from gfx1151). Since the container is configured
# with ``privateNetwork = false``, it shares the host's network
# namespace, so the endpoint is reachable at ``127.0.0.1:<port>``
# from the host's own vmagent — no public HTTPS hop through Caddy is
# needed. Hosts opt in explicitly with the concrete host/port (see
# ``hosts/host.thing/myconfig.observability.llama-swap-metrics.nix``).
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  swapCfg = clientCfg.llamaSwapMetrics;
in
{
  options.myconfig.observability.client.llamaSwapMetrics = with lib; {
    enable = mkEnableOption {
      text = ''
        Prometheus scrape job for llama-swap's native `/metrics`
        endpoint (host CPU/memory/swap/network gauges reported by
        the llama-swap process itself). Distinct from
        ``client.llamaServer.enable``, which scrapes the
        OpenAI-compatible `/v1/models` endpoint via a separate
        Python exporter and may point at a different llama-cpp
        instance on the same host. Opt-in — enable explicitly on
        hosts that run a llama-swap instance exposing this endpoint.
      '';
      default = false;
    };

    scrapeHost = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Hostname or IP of the llama-swap instance to scrape.";
    };

    scrapePort = mkOption {
      type = types.port;
      default = 33657;
      description = "Port of the llama-swap `/metrics` endpoint.";
    };

    metricsPath = mkOption {
      type = types.str;
      default = "/metrics";
      description = "HTTP path of the llama-swap Prometheus endpoint.";
    };

    scrapeIntervalSeconds = mkOption {
      type = types.int;
      default = 15;
      description = ''
        How often vmagent scrapes the llama-swap `/metrics` endpoint
        (in seconds). Defaults to ``15`` to match the vmagent default
        scrape interval.
      '';
    };
  };

  config = lib.mkIf swapCfg.enable {
    assertions = [
      {
        assertion = clientCfg.enable;
        message = ''
          myconfig.observability.client.llamaSwapMetrics requires
          myconfig.observability.client to be enabled on the same
          host: the scrape job is provisioned on the local vmagent,
          which is set up by the client module.
        '';
      }
    ];

    services.vmagent.prometheusConfig = {
      scrape_configs = [
        {
          job_name = "llama-swap";
          scrape_interval = "${toString swapCfg.scrapeIntervalSeconds}s";
          metrics_path = swapCfg.metricsPath;
          static_configs = [
            { targets = [ "${swapCfg.scrapeHost}:${toString swapCfg.scrapePort}" ]; }
          ];
        }
      ];
    };
  };
}
