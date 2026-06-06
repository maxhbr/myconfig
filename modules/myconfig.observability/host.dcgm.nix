# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard for NVIDIA DCGM Exporter GPU metrics.
#
# The metrics are produced by dcgm-exporter (either the native
# systemd service or the OCI container defined in
# hosts/host.thing/nvidia.dcgm-exporter.nix) and scraped by the
# local vmagent (job=`dcgm`) which pushes them into the central
# VictoriaMetrics instance via remote_write.
#
# This module only runs on the observability *host* (where Grafana is)
# and provisions a dashboard so the metrics are immediately visualised.
#
# Metrics reference (from dcgm-exporter default counters):
#   DCGM_FI_DEV_GPU_TEMP       – GPU temperature (°C)
#   DCGM_FI_DEV_MEMORY_TEMP    – Memory temperature (°C)
#   DCGM_FI_DEV_POWER_USAGE    – Power draw (W)
#   DCGM_FI_DEV_SM_CLOCK       – SM clock (MHz)
#   DCGM_FI_DEV_MEM_CLOCK      – Memory clock (MHz)
#   DCGM_FI_DEV_GPU_UTIL       – GPU utilization (%)
#   DCGM_FI_DEV_FB_USED        – Framebuffer memory used (MiB)
#   DCGM_FI_PROF_PIPE_TENSOR_ACTIVE – Tensor core utilization (0..1)
#
# See: https://github.com/NVIDIA/dcgm-exporter
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  dcgmCfg = hostCfg.dcgm;

  # Helper to build a timeseries panel (Grafana v10+ native panel).
  # targets is a list of { expr, legendFormat, refId, unit? } attrsets.
  # unit / yMin / yMax apply to the default field config (first series);
  # per-series unit overrides can be added via fieldConfig.overrides if needed.
  mkMultiTimeseriesPanel =
    {
      id,
      title,
      targets, # list of { expr, legendFormat, refId }
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

  # Helper to build a timeseries panel (Grafana v10+ native panel).
  mkTimeseriesPanel =
    {
      id,
      title,
      expr,
      legendFormat ? "GPU {{gpu}} ({{host}})",
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

  dcgmDashboard = {
    uid = "myconfig-dcgm";
    title = "NVIDIA GPU (DCGM)";
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
        query = "label_values(DCGM_FI_DEV_GPU_TEMP, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "gpu";
        label = "GPU";
        type = "query";
        datasource = {
          type = "prometheus";
          uid = "victoriametrics";
        };
        query = ''label_values(DCGM_FI_DEV_GPU_TEMP{host=~"$host"}, gpu)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];

    # -- Row: Overview stats ------------------------------------------------
    panels =
      let
        filter = ''{host=~"$host", gpu=~"$gpu"}'';
      in
      [
        # -- Stat panels (top row) --
        (mkStatPanel {
          id = 1;
          title = "GPU Count";
          expr = "count(DCGM_FI_DEV_GPU_TEMP${filter})";
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
          expr = "avg(DCGM_FI_DEV_GPU_TEMP${filter})";
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
              value = 85;
            }
          ];
        })
        (mkStatPanel {
          id = 3;
          title = "Total Power";
          expr = "sum(DCGM_FI_DEV_POWER_USAGE${filter})";
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
              value = 300;
            }
            {
              color = "red";
              value = 450;
            }
          ];
        })
        (mkStatPanel {
          id = 4;
          title = "Avg Utilization";
          expr = "avg(DCGM_FI_DEV_GPU_UTIL${filter})";
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
          title = "Total FB Used";
          expr = "sum(DCGM_FI_DEV_FB_USED${filter})";
          unit = "decmbytes";
          gridPos = {
            h = 4;
            w = 5;
            x = 19;
            y = 0;
          };
        })

        # -- Time-series panels --
        # Row y=4: Temperature (combined) | Power Usage
        (mkMultiTimeseriesPanel {
          id = 10;
          title = "Temperature";
          unit = "celsius";
          yMin = 0;
          gridPos = {
            h = 8;
            w = 12;
            x = 0;
            y = 4;
          };
          targets = [
            {
              expr = "DCGM_FI_DEV_GPU_TEMP${filter}";
              legendFormat = "GPU Temp – GPU {{gpu}} ({{host}})";
              refId = "A";
            }
            {
              expr = "DCGM_FI_DEV_MEMORY_TEMP${filter}";
              legendFormat = "Mem Temp – GPU {{gpu}} ({{host}})";
              refId = "B";
            }
          ];
        })
        (mkTimeseriesPanel {
          id = 11;
          title = "Power Usage";
          expr = "DCGM_FI_DEV_POWER_USAGE${filter}";
          unit = "watt";
          yMin = 0;
          gridPos = {
            h = 8;
            w = 12;
            x = 12;
            y = 4;
          };
        })
        # Row y=12: Utilization (combined) | Framebuffer Memory Used
        (mkMultiTimeseriesPanel {
          id = 12;
          title = "Utilization";
          unit = "percent";
          yMin = 0;
          yMax = 100;
          gridPos = {
            h = 8;
            w = 12;
            x = 0;
            y = 12;
          };
          targets = [
            {
              expr = "DCGM_FI_DEV_GPU_UTIL${filter}";
              legendFormat = "GPU Util – GPU {{gpu}} ({{host}})";
              refId = "A";
            }
            {
              expr = "DCGM_FI_PROF_PIPE_TENSOR_ACTIVE${filter} * 100";
              legendFormat = "Tensor Core – GPU {{gpu}} ({{host}})";
              refId = "B";
            }
          ];
        })
        (mkTimeseriesPanel {
          id = 13;
          title = "Framebuffer Memory Used";
          expr = "DCGM_FI_DEV_FB_USED${filter}";
          unit = "decmbytes";
          yMin = 0;
          gridPos = {
            h = 8;
            w = 12;
            x = 12;
            y = 12;
          };
        })
        # Row y=20: SM Clock | Memory Clock
        (mkTimeseriesPanel {
          id = 14;
          title = "SM Clock";
          expr = "DCGM_FI_DEV_SM_CLOCK${filter}";
          unit = "rothertz";
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
          title = "Memory Clock";
          expr = "DCGM_FI_DEV_MEM_CLOCK${filter}";
          unit = "rothertz";
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

  dcgmDashboardFile = pkgs.writeText "dcgm-dashboard.json" (builtins.toJSON dcgmDashboard);
in
{
  options.myconfig.observability.host.dcgm = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard for NVIDIA GPU metrics from
        dcgm-exporter (job=`dcgm`, scraped by vmagent on clients
        with `enableDcgmExporter = true`).
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && dcgmCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = 1;
      providers = [
        {
          name = "myconfig-dcgm";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "dcgm-dashboards" { } ''
            mkdir -p $out
            cp ${dcgmDashboardFile} $out/dcgm.json
          '';
        }
      ];
    };
  };
}
