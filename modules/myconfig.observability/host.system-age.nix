# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard for `nixos_system_age_seconds`, scraped from each
# observability client (see ./client.system-age.nix).
#
# Companion to ./host.nix; only takes effect on the central
# observability host (myconfig.observability.host.enable = true).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  ageCfg = hostCfg.systemAge;

  systemAgeDashboard = {
    uid = "myconfig-system-age";
    title = "NixOS system age";
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "1m";
    time = {
      from = "now-7d";
      to = "now";
    };
    templating.list = [ ];
    annotations.list = [ ];
    panels = [
      {
        id = 1;
        type = "stat";
        title = "Current system age (per host)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
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
        id = 2;
        type = "timeseries";
        title = "System age over time (per host)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 10;
        };
        fieldConfig = {
          defaults = {
            unit = "s";
            custom = {
              drawStyle = "line";
              lineInterpolation = "stepAfter";
              fillOpacity = 10;
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
        id = 3;
        type = "table";
        title = "Activation time + NixOS label per host";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 20;
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
  };

  systemAgeDashboardFile = pkgs.writeText "system-age-dashboard.json" (
    builtins.toJSON systemAgeDashboard
  );
in
{
  options.myconfig.observability.host.systemAge = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard that visualises
        nixos_system_age_seconds / nixos_system_activation_timestamp_seconds
        across all observability clients.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && ageCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = 1;
      providers = [
        {
          name = "myconfig-system-age";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "system-age-dashboards" { } ''
            mkdir -p $out
            cp ${systemAgeDashboardFile} $out/system-age.json
          '';
        }
      ];
    };

    # Register this dashboard with the host-level playlist.
    myconfig.observability.host.playlist.dashboardUids = [ systemAgeDashboard.uid ];
  };
}
