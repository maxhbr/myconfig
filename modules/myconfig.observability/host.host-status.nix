# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "Host status": per-host system resource overview
# sourced from the netdata Prometheus endpoint (see
# `client.nix` — vmagent scrapes `127.0.0.1:19999/api/v1/allmetrics`
# with `format=prometheus_all_hosts` whenever `services.netdata.enable
# = true`, see https://www.netdata.cloud/blog/netdata-prometheus-grafana-stack/).
#
# The dashboard centres around `netdata_app_cpu_utilization_percentage_average`
# for per-application CPU usage and complements it with the most useful
# system-wide metrics netdata emits in this naming scheme:
#   - `netdata_system_cpu_percentage_average` (overall CPU breakdown)
#   - `netdata_system_load_load_average` (1/5/15 min load average)
#   - `netdata_system_ram_MiB_average` (memory usage by state)
#   - `netdata_system_swap_MiB_average` (swap usage)
#   - `netdata_disk_space_GiB_average` (per-mountpoint disk usage)
#   - `netdata_disk_io_KiB_persec_average` (per-disk I/O)
#   - `netdata_net_net_kilobits_persec_average` (per-NIC throughput)
#   - `netdata_apps_mem_MiB_average` (per-application memory)
#
# All panels are filtered by a `host` template variable so a single
# dashboard works for every host that runs netdata + the observability
# client.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  statusCfg = hostCfg.hostStatus;

  # ---------------------------------------------------------------------
  # Overview row — quick at-a-glance numbers
  # ---------------------------------------------------------------------
  overviewPanels = [
    {
      id = 100;
      type = "stat";
      title = "CPU utilisation (system, %)";
      description = ''
        Total CPU utilisation reported by netdata
        (`netdata_system_cpu_percentage_average`, summed over all
        non-idle dimensions).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 6;
        w = 6;
        x = 0;
        y = 1;
      };
      options = {
        reduceOptions = {
          calcs = [ "lastNotNull" ];
          fields = "";
          values = false;
        };
        colorMode = "background";
        graphMode = "area";
        textMode = "value_and_name";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "percent";
        min = 0;
        max = 100;
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "green";
              value = null;
            }
            {
              color = "yellow";
              value = 70;
            }
            {
              color = "orange";
              value = 85;
            }
            {
              color = "red";
              value = 95;
            }
          ];
        };
      };
      targets = [
        {
          expr = ''sum by (host) (netdata_system_cpu_percentage_average{host=~"$host", dimension!="idle", dimension!="iowait"})'';
          legendFormat = "{{host}}";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 101;
      type = "stat";
      title = "Load average (1m)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 6;
        w = 6;
        x = 6;
        y = 1;
      };
      options = {
        reduceOptions = {
          calcs = [ "lastNotNull" ];
          fields = "";
          values = false;
        };
        colorMode = "background";
        graphMode = "area";
        textMode = "value_and_name";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "short";
        decimals = 2;
      };
      targets = [
        {
          expr = ''netdata_system_load_load_average{host=~"$host", dimension="load1"}'';
          legendFormat = "{{host}}";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 102;
      type = "stat";
      title = "RAM used (%)";
      description = ''
        `100 * used / (used + free + cached + buffers)` derived from
        `netdata_system_ram_MiB_average`.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 6;
        w = 6;
        x = 12;
        y = 1;
      };
      options = {
        reduceOptions = {
          calcs = [ "lastNotNull" ];
          fields = "";
          values = false;
        };
        colorMode = "background";
        graphMode = "area";
        textMode = "value_and_name";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "percent";
        min = 0;
        max = 100;
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "green";
              value = null;
            }
            {
              color = "yellow";
              value = 75;
            }
            {
              color = "orange";
              value = 90;
            }
            {
              color = "red";
              value = 97;
            }
          ];
        };
      };
      targets = [
        {
          expr = ''
            100 * sum by (host) (netdata_system_ram_MiB_average{host=~"$host", dimension="used"})
              /
            sum by (host) (netdata_system_ram_MiB_average{host=~"$host"})
          '';
          legendFormat = "{{host}}";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 103;
      type = "stat";
      title = "Swap used (%)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 6;
        w = 6;
        x = 18;
        y = 1;
      };
      options = {
        reduceOptions = {
          calcs = [ "lastNotNull" ];
          fields = "";
          values = false;
        };
        colorMode = "background";
        graphMode = "area";
        textMode = "value_and_name";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "percent";
        min = 0;
        max = 100;
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "green";
              value = null;
            }
            {
              color = "yellow";
              value = 25;
            }
            {
              color = "orange";
              value = 60;
            }
            {
              color = "red";
              value = 90;
            }
          ];
        };
      };
      targets = [
        {
          expr = ''
            100 * sum by (host) (netdata_system_swap_MiB_average{host=~"$host", dimension="used"})
              /
            clamp_min(sum by (host) (netdata_system_swap_MiB_average{host=~"$host"}), 1)
          '';
          legendFormat = "{{host}}";
          refId = "A";
          instant = true;
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # CPU row — the headline metric requested
  # ---------------------------------------------------------------------
  cpuPanels = [
    {
      id = 200;
      type = "timeseries";
      title = "CPU per application (%)";
      description = ''
        `netdata_app_cpu_utilization_percentage_average` —
        per-application CPU utilisation as reported by netdata's
        `apps.plugin`.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 10;
        w = 24;
        x = 0;
        y = 8;
      };
      fieldConfig.defaults = {
        unit = "percent";
        custom = {
          drawStyle = "line";
          lineInterpolation = "linear";
          fillOpacity = 10;
          stacking = {
            mode = "normal";
            group = "A";
          };
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "max"
          "mean"
        ];
      };
      targets = [
        {
          expr = ''
            topk(15,
              sum by (dimension) (
                netdata_app_cpu_utilization_percentage_average{host=~"$host"}
              )
            )
          '';
          legendFormat = "{{dimension}}";
          refId = "A";
        }
      ];
    }
    {
      id = 201;
      type = "timeseries";
      title = "CPU breakdown (system)";
      description = ''
        `netdata_system_cpu_percentage_average` — system-wide CPU
        utilisation broken down by user / system / iowait / softirq /
        etc.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 18;
      };
      fieldConfig.defaults = {
        unit = "percent";
        max = 100;
        custom = {
          drawStyle = "line";
          lineInterpolation = "linear";
          fillOpacity = 20;
          stacking = {
            mode = "normal";
            group = "A";
          };
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
          expr = ''netdata_system_cpu_percentage_average{host=~"$host", dimension!="idle"}'';
          legendFormat = "{{dimension}}";
          refId = "A";
        }
      ];
    }
    {
      id = 202;
      type = "timeseries";
      title = "Load average (1 / 5 / 15 min)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 18;
      };
      fieldConfig.defaults.unit = "short";
      options.legend = {
        displayMode = "table";
        placement = "bottom";
        calcs = [
          "lastNotNull"
          "max"
          "mean"
        ];
      };
      targets = [
        {
          expr = ''netdata_system_load_load_average{host=~"$host"}'';
          legendFormat = "{{dimension}}";
          refId = "A";
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Memory row
  # ---------------------------------------------------------------------
  memoryPanels = [
    {
      id = 300;
      type = "timeseries";
      title = "RAM (MiB, by state)";
      description = ''
        `netdata_system_ram_MiB_average` — system memory broken down
        into used / free / cached / buffers.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 27;
      };
      fieldConfig.defaults = {
        unit = "mbytes";
        custom = {
          drawStyle = "line";
          fillOpacity = 20;
          stacking = {
            mode = "normal";
            group = "A";
          };
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
          expr = ''netdata_system_ram_MiB_average{host=~"$host"}'';
          legendFormat = "{{dimension}}";
          refId = "A";
        }
      ];
    }
    {
      id = 301;
      type = "timeseries";
      title = "Memory per application (MiB)";
      description = ''
        `netdata_apps_mem_MiB_average` — top 15 applications by
        resident memory usage as reported by netdata's `apps.plugin`.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 27;
      };
      fieldConfig.defaults.unit = "mbytes";
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "max"
        ];
      };
      targets = [
        {
          expr = ''
            topk(15,
              sum by (dimension) (
                netdata_apps_mem_MiB_average{host=~"$host"}
              )
            )
          '';
          legendFormat = "{{dimension}}";
          refId = "A";
        }
      ];
    }
    {
      id = 302;
      type = "timeseries";
      title = "Swap (MiB)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 6;
        w = 24;
        x = 0;
        y = 35;
      };
      fieldConfig.defaults = {
        unit = "mbytes";
        custom = {
          drawStyle = "line";
          fillOpacity = 20;
          stacking = {
            mode = "normal";
            group = "A";
          };
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "bottom";
        calcs = [ "lastNotNull" ];
      };
      targets = [
        {
          expr = ''netdata_system_swap_MiB_average{host=~"$host"}'';
          legendFormat = "{{dimension}}";
          refId = "A";
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Disk + network row
  # ---------------------------------------------------------------------
  diskNetPanels = [
    {
      id = 400;
      type = "timeseries";
      title = "Disk space used (GiB)";
      description = ''
        `netdata_disk_space_GiB_average` — used space per mounted
        filesystem (one series per mountpoint, dimension="used").
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 42;
      };
      fieldConfig.defaults.unit = "gbytes";
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
          expr = ''netdata_disk_space_GiB_average{host=~"$host", dimension="used"}'';
          legendFormat = "{{family}}";
          refId = "A";
        }
      ];
    }
    {
      id = 401;
      type = "timeseries";
      title = "Disk I/O (KiB/s)";
      description = ''
        `netdata_disk_io_KiB_persec_average` — read/write throughput
        per block device.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 42;
      };
      fieldConfig.defaults.unit = "KiBs";
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
          expr = ''netdata_disk_io_KiB_persec_average{host=~"$host"}'';
          legendFormat = "{{family}} {{dimension}}";
          refId = "A";
        }
      ];
    }
    {
      id = 402;
      type = "timeseries";
      title = "Network throughput (kbit/s)";
      description = ''
        `netdata_net_net_kilobits_persec_average` — per-interface
        receive/transmit bandwidth.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 24;
        x = 0;
        y = 50;
      };
      fieldConfig.defaults.unit = "Kbits";
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
          expr = ''netdata_net_net_kilobits_persec_average{host=~"$host"}'';
          legendFormat = "{{family}} {{dimension}}";
          refId = "A";
        }
      ];
    }
  ];

  overviewRow = {
    id = 1;
    type = "row";
    title = "Overview";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 0;
    };
    panels = [ ];
  };

  cpuRow = {
    id = 2;
    type = "row";
    title = "CPU";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 7;
    };
    panels = [ ];
  };

  memoryRow = {
    id = 3;
    type = "row";
    title = "Memory";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 26;
    };
    panels = [ ];
  };

  diskNetRow = {
    id = 4;
    type = "row";
    title = "Disk & Network";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 41;
    };
    panels = [ ];
  };

  hostStatusDashboard = {
    uid = "myconfig-host-status";
    title = "Host status";
    tags = [
      "myconfig"
      "host"
      "status"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "30s";
    time = {
      from = "now-3h";
      to = "now";
    };
    templating.list = [
      {
        name = "host";
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        # Source the host list from a metric netdata always emits
        # whenever it's running, so the variable populates even before
        # any application-level metrics show up.
        query = "label_values(netdata_system_uptime_seconds_average, host)";
        refresh = 2;
        includeAll = false;
        multi = false;
        sort = 1;
      }
    ];
    annotations.list = [ ];
    panels = [
      overviewRow
    ]
    ++ overviewPanels
    ++ [ cpuRow ]
    ++ cpuPanels
    ++ [ memoryRow ]
    ++ memoryPanels
    ++ [ diskNetRow ]
    ++ diskNetPanels;
  };

  hostStatusDashboardFile = pkgs.writeText "host-status-dashboard.json" (
    builtins.toJSON hostStatusDashboard
  );
in
{
  options.myconfig.observability.host.hostStatus = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision the per-host "Host status" Grafana dashboard, which
        uses the netdata Prometheus endpoint
        (`netdata_app_cpu_utilization_percentage_average`,
        `netdata_system_*`, `netdata_disk_*`, `netdata_net_*`,
        `netdata_apps_*`) to show CPU, memory, disk and network for a
        single selected host.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && statusCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = 1;
      providers = [
        {
          name = "myconfig-host-status";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "host-status-dashboards" { } ''
            mkdir -p $out
            cp ${hostStatusDashboardFile} $out/host-status.json
          '';
        }
      ];
    };
  };
}
