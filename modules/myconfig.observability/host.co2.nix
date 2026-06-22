# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "Indoor CO2 & temperature" for the metrics
# produced by the TFA Dostmann AirCO2NTROL Mini USB sensor, scraped
# from `air_co2_exporter` (https://github.com/huhamhire/air-co2-exporter).
#
# The exporter is installed on any client where
# `myconfig.observability.client.co2Exporter.enable = true`; that
# module also adds the `co2` scrape job to the host-local vmagent
# which then remote-writes into the central VictoriaMetrics, tagged
# with `host=<networking.hostName>`.
#
# Metrics produced by the exporter:
#   - `air_co2{tag="<…>"}`   CO2 concentration in ppm (gauge)
#   - `air_temp{tag="<…>"}`  Ambient temperature in °C (gauge)
#
# The dashboard provides:
#   - Big current CO2 + temperature stat tiles with IAQ-coloured
#     thresholds (green < 600, yellow ≤ 1000, orange ≤ 1500, red
#     above) following common indoor-air-quality guidance.
#   - 24h timeseries of CO2 and temperature.
#   - Distribution of CO2 over the dashboard time range, so you can
#     see what fraction of the day was spent in each IAQ category.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  co2Cfg = hostCfg.co2;

  # Standard IAQ thresholds used in every panel that visualises CO2.
  # Values are in ppm. Pure ambient air is ~420 ppm.
  co2Thresholds = {
    mode = "absolute";
    steps = [
      {
        color = "green";
        value = null;
      }
      {
        color = "yellow";
        value = 600;
      }
      {
        color = "orange";
        value = 1000;
      }
      {
        color = "red";
        value = 1500;
      }
    ];
  };

  # Reasonable temperature comfort band (°C).
  tempThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "blue";
        value = null;
      }
      {
        color = "green";
        value = 18;
      }
      {
        color = "yellow";
        value = 24;
      }
      {
        color = "red";
        value = 28;
      }
    ];
  };

  # Label-filter fragment reused in every query so dashboard template
  # variables are honoured everywhere.
  filt = ''host=~"$host", tag=~"$tag"'';

  co2Dashboard = {
    uid = "myconfig-co2";
    title = "Indoor CO2 & temperature";
    tags = [
      "myconfig"
      "environment"
      "co2"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "30s";
    time = {
      from = "now-24h";
      to = "now";
    };
    annotations.list = [ ];
    templating.list = [
      {
        name = "host";
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(air_co2, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "tag";
        label = "tag";
        type = "query";
        datasource = "VictoriaMetrics";
        query = ''label_values(air_co2{host=~"$host"}, tag)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];
    panels = [
      # ---------------------------------------------------------------
      # Row 0..6: current values
      # ---------------------------------------------------------------
      {
        id = 1;
        type = "stat";
        title = "Current CO2 (ppm)";
        description = ''
          Latest CO2 reading. Coloured against standard indoor air
          quality guidance: green < 600 ppm, yellow ≤ 1000 ppm,
          orange ≤ 1500 ppm, red above.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 8;
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
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "ppm";
          decimals = 0;
          thresholds = co2Thresholds;
        };
        targets = [
          {
            expr = "avg(air_co2{${filt}})";
            legendFormat = "CO2";
            refId = "A";
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Current temperature (°C)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 8;
          x = 8;
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
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "celsius";
          decimals = 1;
          thresholds = tempThresholds;
        };
        targets = [
          {
            expr = "avg(air_temp{${filt}})";
            legendFormat = "temperature";
            refId = "A";
          }
        ];
      }
      {
        id = 3;
        type = "stat";
        title = "Max CO2 (last 24h)";
        description = ''
          Highest CO2 value seen in the dashboard's time window —
          useful for catching short spikes (closed-door meetings,
          cooking, etc.) that the current-value tile would miss.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 8;
          x = 16;
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
          unit = "ppm";
          decimals = 0;
          thresholds = co2Thresholds;
        };
        targets = [
          {
            expr = "max_over_time(air_co2{${filt}}[$__range])";
            legendFormat = "max CO2";
            refId = "A";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 6..18: timeseries
      # ---------------------------------------------------------------
      {
        id = 10;
        type = "timeseries";
        title = "CO2 over time (ppm)";
        description = ''
          CO2 concentration per host/tag. Threshold bands at 600,
          1000 and 1500 ppm mirror the IAQ guidance used in the stat
          tiles above.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 6;
        };
        fieldConfig.defaults = {
          unit = "ppm";
          decimals = 0;
          thresholds = co2Thresholds;
          custom = {
            drawStyle = "line";
            lineWidth = 2;
            fillOpacity = 10;
            spanNulls = false;
            thresholdsStyle.mode = "dashed";
          };
          color.mode = "thresholds";
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "min"
            "max"
            "mean"
          ];
        };
        targets = [
          {
            expr = "air_co2{${filt}}";
            legendFormat = "{{host}} / {{tag}}";
            refId = "A";
          }
        ];
      }
      {
        id = 11;
        type = "timeseries";
        title = "Temperature over time (°C)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 24;
          x = 0;
          y = 16;
        };
        fieldConfig = {
          defaults = {
            unit = "celsius";
            decimals = 1;
            custom = {
              drawStyle = "line";
              lineWidth = 2;
              fillOpacity = 10;
              spanNulls = false;
            };
          };
          overrides = [
            {
              matcher = {
                id = "byName";
                options = "Augsburg temp";
              };
              properties = [
                {
                  id = "custom.lineStyle";
                  value = {
                    dash = [
                      8
                      8
                    ];
                    fill = "dash";
                  };
                }
                {
                  id = "custom.fillOpacity";
                  value = 0;
                }
              ];
            }
          ];
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "min"
            "max"
            "mean"
          ];
        };
        targets = [
          {
            expr = "air_temp{${filt}}";
            legendFormat = "{{host}} / {{tag}}";
            refId = "A";
          }
          {
            expr = ''weather_temperature_celsius{location="Augsburg"}'';
            legendFormat = "Augsburg temp";
            refId = "B";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 24..32: time-in-band — how long was the air "bad"?
      # ---------------------------------------------------------------
      {
        id = 20;
        type = "stat";
        title = "Time above 1000 ppm";
        description = ''
          Share of the dashboard time window during which the CO2
          reading exceeded 1000 ppm (the threshold above which most
          people start reporting drowsiness / loss of concentration).
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 24;
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
                value = 0.05;
              }
              {
                color = "orange";
                value = 0.2;
              }
              {
                color = "red";
                value = 0.5;
              }
            ];
          };
        };
        targets = [
          {
            expr = "avg_over_time((air_co2{${filt}} > bool 1000)[$__range:1m])";
            legendFormat = "share > 1000 ppm";
            refId = "A";
          }
        ];
      }
      {
        id = 21;
        type = "stat";
        title = "Time above 1500 ppm";
        description = ''
          Share of the dashboard time window in the "poor" CO2 band
          (> 1500 ppm). Anything above 0 here is a strong nudge to
          open a window.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 24;
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
                color = "orange";
                value = 0.01;
              }
              {
                color = "red";
                value = 0.1;
              }
            ];
          };
        };
        targets = [
          {
            expr = "avg_over_time((air_co2{${filt}} > bool 1500)[$__range:1m])";
            legendFormat = "share > 1500 ppm";
            refId = "A";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 32..40: scrape health for the exporter itself
      # ---------------------------------------------------------------
      {
        id = 30;
        type = "timeseries";
        title = "Exporter scrape up";
        description = ''
          `up` for `job="co2"` — 1 means vmagent successfully scraped
          the local exporter, 0 means the exporter is unreachable
          (USB unplugged, libusb error, service crashed, …).
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 24;
          x = 0;
          y = 32;
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          max = 1;
          decimals = 0;
          custom = {
            drawStyle = "line";
            lineInterpolation = "stepAfter";
            lineWidth = 2;
            fillOpacity = 20;
          };
        };
        options.legend = {
          displayMode = "list";
          placement = "bottom";
        };
        targets = [
          {
            expr = ''up{job="co2", host=~"$host"}'';
            legendFormat = "{{host}}";
            refId = "A";
          }
        ];
      }
    ];
  };

  co2DashboardFile = pkgs.writeText "co2-dashboard.json" (builtins.toJSON co2Dashboard);
in
{
  options.myconfig.observability.host.co2 = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard for the TFA Dostmann AirCO2NTROL
        CO2 / temperature metrics (job=`co2`), scraped by vmagent on
        every host that runs `myconfig.observability.client.co2Exporter`.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && co2Cfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-co2";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "co2-dashboards" { } ''
            mkdir -p $out
            cp ${co2DashboardFile} $out/co2.json
          '';
        }
      ];
    };
  };
}
