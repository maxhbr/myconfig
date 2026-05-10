# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Combined Grafana dashboard for "home-lab status": NixOS system age /
# activation info (from ./client.system-age.nix), uptime/blackbox
# probes for every deployedService (from ./host.uptime.nix), and
# log-shipping freshness from the `loki:host_logs_recent_count`
# recording rule (from ./host.loki.nix).
#
# The underlying feature modules still own their data sources
# (textfile collector + blackbox_exporter + vmagent scrape jobs +
# Loki ruler); this module only owns the dashboard UI and replaces
# the previous separate dashboards (`myconfig-system-age` and
# `myconfig-uptime`).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  statusCfg = hostCfg.homeLabStatus;

  # ---------------------------------------------------------------------
  # System-status row (NixOS system age + uptime + activation info)
  # ---------------------------------------------------------------------
  systemPanels = [
    {
      id = 10;
      type = "stat";
      title = "Current system age (per host)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
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
      fieldConfig = {
        defaults = {
          unit = "s";
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
              {
                # > 7 days = yellow
                color = "yellow";
                value = 604800;
              }
              {
                # > 30 days = orange
                color = "orange";
                value = 2592000;
              }
              {
                # > 90 days = red
                color = "red";
                value = 7776000;
              }
            ];
          };
        };
      };
      targets = [
        {
          expr = ''nixos_system_age_seconds{host=~"$host"}'';
          legendFormat = "{{host}}";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 11;
      type = "stat";
      title = "System uptime (per host)";
      description = ''
        `netdata_system_uptime_seconds_average` from each host's local
        netdata exporter — the wall-clock uptime of the machine
        itself (resets only on reboot), complementing the system-age
        panel which tracks time-since-NixOS-activation.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
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
      fieldConfig = {
        defaults = {
          unit = "s";
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "red";
                value = null;
              }
              {
                # > 5 min — host has finished booting
                color = "yellow";
                value = 300;
              }
              {
                # > 1 h — running steadily
                color = "green";
                value = 3600;
              }
            ];
          };
        };
      };
      targets = [
        {
          expr = ''netdata_system_uptime_seconds_average{host=~"$host"}'';
          legendFormat = "{{host}}";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 12;
      type = "timeseries";
      title = "System age over time (per host)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 24;
        x = 0;
        y = 9;
      };
      fieldConfig = {
        defaults = {
          unit = "s";
          custom = {
            drawStyle = "line";
            lineInterpolation = "stepAfter";
            fillOpacity = 10;
            scaleDistribution = {
              type = "log";
              log = 10;
            };
          };
        };
      };
      targets = [
        {
          expr = ''nixos_system_age_seconds{host=~"$host"}'';
          legendFormat = "{{host}}";
          refId = "A";
        }
      ];
    }
    {
      id = 13;
      type = "table";
      title = "Activation time + NixOS label per host";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 24;
        x = 0;
        y = 17;
      };
      transformations = [
        {
          id = "joinByField";
          options = {
            byField = "host";
            mode = "outer";
          };
        }
        {
          id = "organize";
          options = {
            excludeByName = {
              Time = true;
              "Time 1" = true;
              "Time 2" = true;
              "__name__ 1" = true;
              "__name__ 2" = true;
              "job 1" = true;
              "job 2" = true;
              "instance 1" = true;
              "instance 2" = true;
            };
            renameByName = {
              "Value #A" = "activation_timestamp";
              "Value #B" = "info";
            };
          };
        }
      ];
      targets = [
        {
          expr = ''nixos_system_activation_timestamp_seconds{host=~"$host"}'';
          legendFormat = "{{host}}";
          refId = "A";
          format = "table";
          instant = true;
        }
        {
          expr = ''nixos_system_info{host=~"$host"}'';
          legendFormat = "{{host}}";
          refId = "B";
          format = "table";
          instant = true;
        }
      ];
      fieldConfig = {
        defaults = { };
        overrides = [
          {
            matcher = {
              id = "byName";
              options = "activation_timestamp";
            };
            properties = [
              {
                id = "unit";
                value = "dateTimeAsIso";
              }
            ];
          }
        ];
      };
    }
  ];

  # ---------------------------------------------------------------------
  # Service-status row (blackbox probes against deployedServices)
  # ---------------------------------------------------------------------
  # ---------------------------------------------------------------------
  # Logs-status row (freshness of journal logs ingested into Loki)
  #
  # Sourced from the `loki:host_logs_recent_count` recording rule that
  # ./host.loki.nix has Loki's embedded ruler emit into VictoriaMetrics.
  # The metric is only sampled while a host is producing logs, so
  # `time() - timestamp(last_over_time(...))` gives the seconds since
  # that host's most recent log line was received by Loki — a
  # client-side or network outage will make this number grow.
  # ---------------------------------------------------------------------
  logsPanels = [
    {
      id = 30;
      type = "stat";
      title = "Time since last log received (per host)";
      description = ''
        Seconds since Loki last ingested a journal log line from each
        host, derived from the `loki:host_logs_recent_count` recording
        rule. Stale values (> 5 min) typically mean the client's
        Alloy agent is down or the network path to Loki is broken.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 47;
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
      fieldConfig = {
        defaults = {
          unit = "s";
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
              {
                # > 5 min — slightly stale
                color = "yellow";
                value = 300;
              }
              {
                # > 30 min — likely client down
                color = "orange";
                value = 1800;
              }
              {
                # > 2 h — definitely down
                color = "red";
                value = 7200;
              }
            ];
          };
        };
      };
      targets = [
        {
          # Subquery: sample `loki:host_logs_recent_count` at 1m
          # resolution over the last 24h and take the timestamp of the
          # last sample present (i.e. the last time the host emitted
          # any log lines). Subtracting from `time()` yields the age
          # in seconds. PromQL automatically broadcasts the scalar
          # `time()` across the per-host vector returned by
          # `timestamp(last_over_time(...))`.
          expr = ''
            time() - timestamp(last_over_time(loki:host_logs_recent_count{host=~"$host"}[24h:1m]))
          '';
          legendFormat = "{{host}}";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 31;
      type = "timeseries";
      title = "Time since last log received over time (per host)";
      description = ''
        Same metric as the stat panel but plotted over the dashboard
        time range, useful for spotting intermittent log-shipping
        outages.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 47;
      };
      fieldConfig = {
        defaults = {
          unit = "s";
          custom = {
            drawStyle = "line";
            lineInterpolation = "linear";
            fillOpacity = 10;
          };
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "mean"
          "max"
        ];
      };
      targets = [
        {
          expr = ''
            time() - timestamp(last_over_time(loki:host_logs_recent_count{host=~"$host"}[24h:1m]))
          '';
          legendFormat = "{{host}}";
          refId = "A";
        }
      ];
    }
  ];

  servicePanels = [
    {
      id = 20;
      type = "stat";
      title = "Service status (1 = up)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 10;
        w = 24;
        x = 0;
        y = 27;
      };
      options = {
        reduceOptions = {
          calcs = [ "lastNotNull" ];
          fields = "";
          values = false;
        };
        colorMode = "background";
        graphMode = "none";
        textMode = "value_and_name";
        orientation = "auto";
      };
      fieldConfig = {
        defaults = {
          mappings = [
            {
              type = "value";
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
      };
      targets = [
        {
          expr = ''probe_success{job="blackbox", probed_host=~"$host"}'';
          legendFormat = "{{instance}}";
          refId = "A";
        }
      ];
    }
    {
      id = 21;
      type = "timeseries";
      title = "HTTP status code";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 37;
      };
      targets = [
        {
          expr = ''probe_http_status_code{job="blackbox", probed_host=~"$host"}'';
          legendFormat = "{{instance}}";
          refId = "A";
        }
      ];
    }
    {
      id = 22;
      type = "timeseries";
      title = "Probe duration (s)";
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 37;
      };
      targets = [
        {
          expr = ''probe_duration_seconds{job="blackbox", probed_host=~"$host"}'';
          legendFormat = "{{instance}}";
          refId = "A";
        }
      ];
    }
  ];

  systemRow = {
    id = 1;
    type = "row";
    title = "System status";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 0;
    };
    panels = [ ];
  };

  serviceRow = {
    id = 2;
    type = "row";
    title = "Service status";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 26;
    };
    panels = [ ];
  };

  logsRow = {
    id = 3;
    type = "row";
    title = "Logs status";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 46;
    };
    panels = [ ];
  };

  homeLabDashboard = {
    uid = "myconfig-home-lab-status";
    title = "Home-lab status";
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "30s";
    time = {
      from = "now-24h";
      to = "now";
    };
    templating.list = [
      {
        name = "host";
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        # Source the host list from a metric every client emits so
        # the variable is populated even if some hosts have no
        # deployedServices probed by blackbox.
        query = "label_values(nixos_system_age_seconds, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];
    annotations.list = [ ];
    panels = [
      systemRow
    ]
    ++ systemPanels
    ++ [ serviceRow ]
    ++ servicePanels
    ++ [ logsRow ]
    ++ logsPanels;
  };

  homeLabDashboardFile = pkgs.writeText "home-lab-status-dashboard.json" (
    builtins.toJSON homeLabDashboard
  );
in
{
  options.myconfig.observability.host.homeLabStatus = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision the combined "Home-lab status" Grafana dashboard,
        which merges the previous "NixOS system age" and "Service
        uptime" dashboards into a single view with three collapsible
        rows (System status / Service status / Logs status).
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && statusCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = 1;
      providers = [
        {
          name = "myconfig-home-lab-status";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "home-lab-status-dashboards" { } ''
            mkdir -p $out
            cp ${homeLabDashboardFile} $out/home-lab-status.json
          '';
        }
      ];
    };
  };
}
