# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "llama-swap": visualises the host-level system
# gauges reported by llama-swap's own native `/metrics` Prometheus
# endpoint (job=`llama-swap`, provisioned by
# ``client.llama-swap-metrics.nix``). This is distinct from the
# "Llama-server models" dashboard (``host.llama-server.nix``), which
# visualises model roster/state/timing metrics produced by a separate
# Python exporter polling `/v1/models`.
#
# Metrics (verified against the live endpoint on host `thing` via
# ``curl -k https://gfx1151.thing.wg0.maxhbr.local/metrics``):
#
#   * llamaswap_cpu_util_percent{core}                   per-core CPU utilisation (%)
#   * llamaswap_memory_total_bytes                       total system memory (bytes)
#   * llamaswap_memory_used_bytes                        used system memory (bytes)
#   * llamaswap_memory_free_bytes                        free system memory (bytes)
#   * llamaswap_swap_total_bytes                         total swap (bytes)
#   * llamaswap_swap_used_bytes                          used swap (bytes)
#   * llamaswap_load_average{interval="1m"|"5m"|"15m"}   load average
#   * llamaswap_network_bytes_total{interface,direction} cumulative network I/O (counter)
#
# This module runs only on the observability *host* (where Grafana
# is) and provisions the dashboard under the "AI" Grafana folder,
# alongside the "Llama-server models" and "LiteLLM" dashboards.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  swapCfg = hostCfg.llamaSwap;

  llamaSwapDashboard = {
    uid = "myconfig-llama-swap";
    title = "llama-swap";
    tags = [
      "myconfig"
      "llm"
      "llama-swap"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "30s";
    time = {
      from = "now-6h";
      to = "now";
    };
    annotations.list = [ ];
    templating.list = [
      {
        name = "host";
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(llamaswap_memory_total_bytes, host)";
        refresh = 2;
        includeAll = false;
        multi = false;
        sort = 1;
      }
    ];
    panels = [
      # ================================================================
      # Row 0: Overview — stat panels
      # ================================================================
      {
        id = 1;
        type = "stat";
        title = "Exporter up";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
          x = 0;
          y = 0;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "background";
          graphMode = "none";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          mappings = [
            {
              options = {
                "0" = {
                  color = "red";
                  text = "DOWN";
                };
                "1" = {
                  color = "green";
                  text = "UP";
                };
              };
              type = "value";
            }
          ];
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "red";
                value = null;
              }
              {
                color = "green";
                value = 1;
              }
            ];
          };
        };
        targets = [
          {
            expr = "up{host=~\"$host\", job=\"llama-swap\"}";
            legendFormat = "{{host}} / {{instance}}";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Memory used";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
          x = 6;
          y = 0;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "area";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "percentunit";
          decimals = 1;
          min = 0;
          max = 1;
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
              {
                color = "yellow";
                value = 0.8;
              }
              {
                color = "red";
                value = 0.95;
              }
            ];
          };
        };
        targets = [
          {
            expr = "llamaswap_memory_used_bytes{host=~\"$host\"} / llamaswap_memory_total_bytes{host=~\"$host\"}";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 3;
        type = "stat";
        title = "Swap used";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
          x = 12;
          y = 0;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "area";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "percentunit";
          decimals = 1;
          min = 0;
          max = 1;
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
              {
                color = "yellow";
                value = 0.3;
              }
              {
                color = "red";
                value = 0.7;
              }
            ];
          };
        };
        targets = [
          {
            expr = ''
              (llamaswap_swap_used_bytes{host=~"$host"} / clamp_min(llamaswap_swap_total_bytes{host=~"$host"}, 1))
                or vector(0)
            '';
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 4;
        type = "stat";
        title = "Load average (1m)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
          x = 18;
          y = 0;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "area";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 2;
        };
        targets = [
          {
            expr = "llamaswap_load_average{host=~\"$host\", interval=\"1m\"}";
            refId = "A";
            instant = true;
          }
        ];
      }

      # ================================================================
      # Row 5: CPU
      # ================================================================
      {
        id = 10;
        type = "timeseries";
        title = "Per-core CPU utilisation";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 9;
          w = 24;
          x = 0;
          y = 5;
        };
        fieldConfig.defaults = {
          unit = "percent";
          min = 0;
          max = 100;
          custom = {
            drawStyle = "line";
            lineInterpolation = "linear";
            fillOpacity = 10;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = "llamaswap_cpu_util_percent{host=~\"$host\"}";
            legendFormat = "core {{core}}";
            refId = "A";
          }
        ];
      }

      # ================================================================
      # Row 14: Memory & swap
      # ================================================================
      {
        id = 20;
        type = "timeseries";
        title = "Memory usage";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 14;
        };
        fieldConfig.defaults = {
          unit = "bytes";
          min = 0;
          custom = {
            drawStyle = "line";
            fillOpacity = 15;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = "llamaswap_memory_total_bytes{host=~\"$host\"}";
            legendFormat = "total";
            refId = "A";
          }
          {
            expr = "llamaswap_memory_used_bytes{host=~\"$host\"}";
            legendFormat = "used";
            refId = "B";
          }
          {
            expr = "llamaswap_memory_free_bytes{host=~\"$host\"}";
            legendFormat = "free";
            refId = "C";
          }
        ];
      }
      {
        id = 21;
        type = "timeseries";
        title = "Swap usage";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 14;
        };
        fieldConfig.defaults = {
          unit = "bytes";
          min = 0;
          custom = {
            drawStyle = "line";
            fillOpacity = 15;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = "llamaswap_swap_total_bytes{host=~\"$host\"}";
            legendFormat = "total";
            refId = "A";
          }
          {
            expr = "llamaswap_swap_used_bytes{host=~\"$host\"}";
            legendFormat = "used";
            refId = "B";
          }
        ];
      }

      # ================================================================
      # Row 22: Load average & network
      # ================================================================
      {
        id = 30;
        type = "timeseries";
        title = "Load average";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 22;
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          custom = {
            drawStyle = "line";
            fillOpacity = 5;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "max"
          ];
        };
        targets = [
          {
            expr = "llamaswap_load_average{host=~\"$host\"}";
            legendFormat = "{{interval}}";
            refId = "A";
          }
        ];
      }
      {
        id = 31;
        type = "timeseries";
        title = "Network throughput (by interface/direction)";
        description = "rate(llamaswap_network_bytes_total[5m]) — bytes/s.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 22;
        };
        fieldConfig.defaults = {
          unit = "Bps";
          min = 0;
          custom = {
            drawStyle = "line";
            fillOpacity = 10;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = "rate(llamaswap_network_bytes_total{host=~\"$host\"}[5m])";
            legendFormat = "{{interface}} {{direction}}";
            refId = "A";
          }
        ];
      }
    ];
  };

  llamaSwapDashboardFile = pkgs.writeText "llama-swap-dashboard.json" (
    builtins.toJSON llamaSwapDashboard
  );
in
{
  options.myconfig.observability.host.llamaSwap = with lib; {
    provisionDashboard = mkEnableOption {
      text = ''
        Provision the "llama-swap" Grafana dashboard, which
        visualises the host-level system gauges (CPU, memory, swap,
        load average, network) reported by llama-swap's native
        `/metrics` endpoint via
        ``client.llama-swap-metrics.nix``. Auto-enabled when the
        observability host is active.
      '';
      default = false;
    };
  };

  config = lib.mkMerge [
    # Auto-provision when the observability host is active
    (lib.mkIf hostCfg.enable {
      myconfig.observability.host.llamaSwap.provisionDashboard = lib.mkDefault true;
    })
    # Dashboard provisioning
    (lib.mkIf swapCfg.provisionDashboard {
      services.grafana.provision.dashboards.settings = {
        apiVersion = lib.mkDefault 1;
        providers = [
          {
            name = "myconfig-llama-swap";
            type = "file";
            disableDeletion = true;
            updateIntervalSeconds = 60;
            # Group with the other AI-related dashboards ("Llama-server
            # models", "LiteLLM") under the "AI" Grafana folder.
            folder = "AI";
            options.path = pkgs.runCommand "llama-swap-dashboards" { } ''
              mkdir -p $out
              cp ${llamaSwapDashboardFile} $out/llama-swap.json
            '';
          }
        ];
      };
    })
  ];
}
