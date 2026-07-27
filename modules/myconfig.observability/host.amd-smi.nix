# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard for AMD GPU metrics from amd_smi_exporter.
#
# The metrics are produced by the amd_smi_exporter systemd service
# (client.amd-smi-exporter.nix) and scraped by the local vmagent
# (job=`amd-smi`) which pushes them into the central VictoriaMetrics
# instance via remote_write.
#
# This module only runs on the observability *host* (where Grafana is)
# and provisions a dashboard so the metrics are immediately visualised.
#
# Metric reference (from the pinned amd_smi_exporter rev; see
# client.amd-smi-exporter.nix). Note the upstream quirk that the GPU
# index is carried in a *per-metric* label whose name equals the metric
# name minus the `amd_` prefix (e.g. `amd_gpu_power{gpu_power="0"}`),
# and a second `productname` label carries the card series name.
#
#   amd_gpu_current_temperature{gpu_current_temperature, productname}
#       – GPU temperature, RAW millidegrees Celsius (÷1000 → °C)
#   amd_gpu_power{gpu_power, productname}
#       – GPU power draw, RAW microwatts (÷1e6 → W)
#   amd_gpu_power_cap{gpu_power_cap, productname}
#       – GPU power cap, RAW microwatts (÷1e6 → W)
#   amd_gpu_SCLK{gpu_SCLK, productname}
#       – Shader (system) clock, RAW hertz (Grafana `hertz` auto-scales)
#   amd_gpu_MCLK{gpu_MCLK, productname}
#       – Memory clock, RAW hertz
#   amd_gpu_use_percent{gpu_use_percent, productname}
#       – GPU busy percent (0..100)
#   amd_gpu_memory_use_percent{gpu_memory_use_percent, productname}
#       – GPU memory busy percent (0..100)
#   amd_num_gpus{num_gpus}
#       – Number of GPUs the exporter enumerated
#
# NOTE: the exporter applies *no* scaling — values are the raw amdsmi
# (rsmi) return values. rsmi conventions are millidegrees C for
# temperature, microwatts for power, and Hz for clocks; the scaling
# below encodes those. If a panel reads 1000x off against `rocm-smi`
# on live hardware, revisit the divisors here.
#
# See: https://github.com/amd/amd_smi_exporter
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  amdSmiCfg = hostCfg.amdSmi;

  # Helper to build a timeseries panel (Grafana v10+ native panel).
  # targets is a list of { expr, legendFormat, refId } attrsets.
  mkMultiTimeseriesPanel =
    {
      id,
      title,
      targets,
      unit ? "short",
      gridPos,
      yMin ? null,
      yMax ? null,
    }:
    {
      inherit id title gridPos;
      type = "timeseries";
      datasource = {
        type = "prometheus";
        uid = "victoriametrics";
      };
      fieldConfig = {
        defaults = {
          color.mode = "palette-classic";
          custom = {
            lineWidth = 2;
            fillOpacity = 10;
            pointSize = 5;
            showPoints = "never";
            spanNulls = false;
          };
          unit = unit;
          min = yMin;
          max = yMax;
        };
        overrides = [ ];
      };
      options = {
        tooltip.mode = "multi";
        legend = {
          displayMode = "table";
          placement = "right";
          calcs = [
            "mean"
            "max"
            "lastNotNull"
          ];
        };
      };
      targets = map (t: {
        inherit (t) expr legendFormat refId;
        datasource = {
          type = "prometheus";
          uid = "victoriametrics";
        };
      }) targets;
    };

  # Single-series convenience wrapper around mkMultiTimeseriesPanel.
  mkTimeseriesPanel =
    {
      id,
      title,
      expr,
      legendFormat ? "{{productname}} ({{host}})",
      unit ? "short",
      gridPos,
      yMin ? null,
      yMax ? null,
    }:
    mkMultiTimeseriesPanel {
      inherit
        id
        title
        unit
        gridPos
        yMin
        yMax
        ;
      targets = [
        {
          inherit expr legendFormat;
          refId = "A";
        }
      ];
    };

  # Helper to build a stat panel for a single aggregate value.
  mkStatPanel =
    {
      id,
      title,
      expr,
      unit ? "short",
      gridPos,
      thresholds ? [
        {
          color = "green";
          value = null;
        }
      ],
    }:
    {
      inherit id title gridPos;
      type = "stat";
      datasource = {
        type = "prometheus";
        uid = "victoriametrics";
      };
      fieldConfig = {
        defaults = {
          inherit unit thresholds;
          color.mode = "thresholds";
          mappings = [ ];
        };
        overrides = [ ];
      };
      options = {
        colorMode = "background";
        graphMode = "area";
        reduceOptions = {
          calcs = [ "lastNotNull" ];
          fields = "";
          values = false;
        };
        textMode = "auto";
      };
      targets = [
        {
          inherit expr;
          datasource = {
            type = "prometheus";
            uid = "victoriametrics";
          };
          refId = "A";
          instant = true;
        }
      ];
    };

  amdSmiDashboard = {
    uid = "myconfig-amd-smi";
    title = "AMD GPU (amd_smi)";
    tags = [
      "myconfig"
      "gpu"
      "amd"
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
        label = "Host";
        type = "query";
        datasource = {
          type = "prometheus";
          uid = "victoriametrics";
        };
        query = "label_values(amd_gpu_current_temperature, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "productname";
        label = "GPU";
        type = "query";
        datasource = {
          type = "prometheus";
          uid = "victoriametrics";
        };
        query = ''label_values(amd_gpu_current_temperature{host=~"$host"}, productname)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];

    panels =
      let
        filter = ''{host=~"$host", productname=~"$productname"}'';
      in
      [
        # -- Stat panels (top row, y=0) --
        (mkStatPanel {
          id = 1;
          title = "GPU Count";
          expr = "count(amd_gpu_current_temperature${filter})";
          gridPos = {
            h = 4;
            w = 4;
            x = 0;
            y = 0;
          };
        })
        (mkStatPanel {
          id = 2;
          title = "Avg Temperature";
          expr = "avg(amd_gpu_current_temperature${filter}) / 1000";
          unit = "celsius";
          gridPos = {
            h = 4;
            w = 5;
            x = 4;
            y = 0;
          };
          thresholds = [
            {
              color = "green";
              value = null;
            }
            {
              color = "yellow";
              value = 75;
            }
            {
              color = "red";
              value = 90;
            }
          ];
        })
        (mkStatPanel {
          id = 3;
          title = "Total Power";
          expr = "sum(amd_gpu_power${filter}) / 1e6";
          unit = "watt";
          gridPos = {
            h = 4;
            w = 5;
            x = 9;
            y = 0;
          };
          thresholds = [
            {
              color = "green";
              value = null;
            }
            {
              color = "yellow";
              value = 100;
            }
            {
              color = "red";
              value = 200;
            }
          ];
        })
        (mkStatPanel {
          id = 4;
          title = "Avg Utilization";
          expr = "avg(amd_gpu_use_percent${filter})";
          unit = "percent";
          gridPos = {
            h = 4;
            w = 5;
            x = 14;
            y = 0;
          };
          thresholds = [
            {
              color = "blue";
              value = null;
            }
            {
              color = "green";
              value = 20;
            }
            {
              color = "yellow";
              value = 80;
            }
            {
              color = "red";
              value = 95;
            }
          ];
        })
        (mkStatPanel {
          id = 5;
          title = "Avg Memory Utilization";
          expr = "avg(amd_gpu_memory_use_percent${filter})";
          unit = "percent";
          gridPos = {
            h = 4;
            w = 5;
            x = 19;
            y = 0;
          };
        })

        # -- Time-series panels --
        # Row y=4: Temperature | Power (draw + cap)
        (mkTimeseriesPanel {
          id = 10;
          title = "Temperature";
          expr = "amd_gpu_current_temperature${filter} / 1000";
          unit = "celsius";
          yMin = 0;
          gridPos = {
            h = 8;
            w = 12;
            x = 0;
            y = 4;
          };
        })
        (mkMultiTimeseriesPanel {
          id = 11;
          title = "Power";
          unit = "watt";
          yMin = 0;
          gridPos = {
            h = 8;
            w = 12;
            x = 12;
            y = 4;
          };
          targets = [
            {
              expr = "amd_gpu_power${filter} / 1e6";
              legendFormat = "Power – {{productname}} ({{host}})";
              refId = "A";
            }
            {
              expr = "amd_gpu_power_cap${filter} / 1e6";
              legendFormat = "Power Cap – {{productname}} ({{host}})";
              refId = "B";
            }
          ];
        })
        # Row y=12: GPU Utilization | Memory Utilization
        (mkTimeseriesPanel {
          id = 12;
          title = "GPU Utilization";
          expr = "amd_gpu_use_percent${filter}";
          unit = "percent";
          yMin = 0;
          yMax = 100;
          gridPos = {
            h = 8;
            w = 12;
            x = 0;
            y = 12;
          };
        })
        (mkTimeseriesPanel {
          id = 13;
          title = "Memory Utilization";
          expr = "amd_gpu_memory_use_percent${filter}";
          unit = "percent";
          yMin = 0;
          yMax = 100;
          gridPos = {
            h = 8;
            w = 12;
            x = 12;
            y = 12;
          };
        })
        # Row y=20: Shader Clock (SCLK) | Memory Clock (MCLK)
        (mkTimeseriesPanel {
          id = 14;
          title = "Shader Clock (SCLK)";
          expr = "amd_gpu_SCLK${filter}";
          unit = "hertz";
          yMin = 0;
          gridPos = {
            h = 8;
            w = 12;
            x = 0;
            y = 20;
          };
        })
        (mkTimeseriesPanel {
          id = 15;
          title = "Memory Clock (MCLK)";
          expr = "amd_gpu_MCLK${filter}";
          unit = "hertz";
          yMin = 0;
          gridPos = {
            h = 8;
            w = 12;
            x = 12;
            y = 20;
          };
        })
      ];
  };

  amdSmiDashboardFile = pkgs.writeText "amd-smi-dashboard.json" (builtins.toJSON amdSmiDashboard);
in
{
  options.myconfig.observability.host.amdSmi = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard for AMD GPU metrics from
        amd_smi_exporter (job=`amd-smi`, scraped by vmagent on clients
        with `myconfig.observability.client.amdSmiExporter.enable`).
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && amdSmiCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = 1;
      providers = [
        {
          name = "myconfig-amd-smi";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "amd-smi-dashboards" { } ''
            mkdir -p $out
            cp ${amdSmiDashboardFile} $out/amd-smi.json
          '';
        }
      ];
    };
  };
}
