# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Combined Grafana dashboard for "home-lab status": NixOS system age /
# activation info (from ./client.system-age.nix) plus uptime/blackbox
# probes for every deployedService (from ./host.uptime.nix).
#
# The two underlying feature modules still own their data sources
# (textfile collector + blackbox_exporter + vmagent scrape jobs); this
# module only owns the dashboard UI and replaces the previous two
# separate dashboards (`myconfig-system-age` and `myconfig-uptime`).
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
          expr = "nixos_system_age_seconds";
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
          expr = "netdata_system_uptime_seconds_average";
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
          expr = "nixos_system_age_seconds";
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
          expr = "nixos_system_activation_timestamp_seconds";
          legendFormat = "{{host}}";
          refId = "A";
          format = "table";
          instant = true;
        }
        {
          expr = "nixos_system_info";
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
          expr = ''probe_success{job="blackbox"}'';
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
          expr = ''probe_http_status_code{job="blackbox"}'';
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
          expr = ''probe_duration_seconds{job="blackbox"}'';
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
    templating.list = [ ];
    annotations.list = [ ];
    panels = [ systemRow ] ++ systemPanels ++ [ serviceRow ] ++ servicePanels;
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
        uptime" dashboards into a single view with two collapsible
        rows (System status / Service status).
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
