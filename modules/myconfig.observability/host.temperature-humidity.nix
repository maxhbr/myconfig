# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "Temperature & humidity": a single pane of glass
# that aggregates every temperature- and humidity-bearing metric that
# the observability stack already ingests, so you can compare indoor
# vs outdoor conditions, individual rooms, and the AirCO2NTROL Mini
# all in one place without flipping between dashboards.
#
# Sources merged into this dashboard (all already provisioned
# elsewhere — this file only *reads* them, it does not configure any
# exporter):
#
#   1. Home Assistant Prometheus exporter
#      (see modules/myconfig.observability/host.power.nix and
#       hosts/host.nuc/smart-home/services.home-assistant.nix).
#      Metric naming follows the HA `prometheus` integration:
#        - `hass_sensor_temperature_celsius{entity,friendly_name,domain}`
#          for any sensor with `device_class: temperature`.
#        - `hass_sensor_humidity_percent{entity,friendly_name,domain}`
#          for any sensor with `device_class: humidity`.
#        - Fallback `hass_sensor_unit_celsius` / `hass_sensor_unit_percent`
#          for sensors without a device_class but a matching unit —
#          we deliberately do NOT include these (too noisy: battery
#          percentages, signal strength, etc.) and stick to the
#          well-typed device_class metrics.
#        - `hass_climate_current_temperature_celsius` for HVAC
#          entities (heating/cooling reporting an internal probe).
#
#   2. AirCO2NTROL Mini USB sensor via `air_co2_exporter`
#      (see host.co2.nix).
#        - `air_temp{host,tag}`   — ambient temperature next to the
#          CO2 sensor in °C.
#
#   3. Open-Meteo weather scraper
#      (see host.weather.nix and client.weather-exporter.nix).
#        - `weather_temperature_celsius{host,location}` — outdoor
#          temperature at 2 m.
#        - `weather_apparent_temperature_celsius{host,location}` —
#          feels-like temperature.
#        - `weather_humidity_percent{host,location}` — outdoor RH.
#
# Existing dashboards (CO2, weather, power) are intentionally left
# untouched; this dashboard is a *read-only* aggregation layer over
# the same underlying metrics so users with the old dashboards still
# get exactly the same experience there.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  thCfg = hostCfg.temperatureHumidity;

  # Indoor comfort thresholds (°C). Matches the CO2 dashboard's
  # temperature stat tile so the colour coding feels consistent when
  # you flip between dashboards.
  tempThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "purple";
        value = null;
      }
      {
        color = "blue";
        value = 0;
      }
      {
        color = "light-blue";
        value = 10;
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
        color = "orange";
        value = 28;
      }
      {
        color = "red";
        value = 32;
      }
    ];
  };

  # Indoor relative-humidity comfort band (%). Same shape as the
  # weather dashboard for cross-dashboard consistency: <40 % is dry,
  # 40-70 % is comfortable, >70 % is muggy / mould risk territory.
  humidityThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "orange";
        value = null;
      }
      {
        color = "green";
        value = 40;
      }
      {
        color = "yellow";
        value = 60;
      }
      {
        color = "blue";
        value = 70;
      }
      {
        color = "red";
        value = 85;
      }
    ];
  };

  # ---------------------------------------------------------------------
  # Sub-query helpers. We use template variables to filter Home
  # Assistant entities and weather locations independently — different
  # data sources expose different labels so they can't share one
  # variable.
  # ---------------------------------------------------------------------

  # HA-side label filter. `entity=~"$ha_entity"` lets the user narrow
  # down to a single sensor.
  haFilt = ''entity=~"$ha_entity"'';

  # Air-CO2 exporter filter — `host` + `tag` mirror host.co2.nix's
  # variables but we re-declare them here so this dashboard remains
  # self-contained.
  co2Filt = ''host=~"$co2_host", tag=~"$co2_tag"'';

  # Weather exporter filter.
  weatherFilt = ''host=~"$weather_host", location=~"$weather_location"'';

  # All three temperature series unioned into a single query string —
  # used by the combined timeseries panel. Each series carries a
  # synthetic `source` label so legends stay readable.
  combinedTempExpr = ''
    label_replace(hass_sensor_temperature_celsius{${haFilt}}, "source", "indoor (HA)", "", "")
    or
    label_replace(hass_climate_current_temperature_celsius{${haFilt}}, "source", "climate (HA)", "", "")
    or
    label_replace(air_temp{${co2Filt}}, "source", "AirCO2NTROL", "", "")
    or
    label_replace(weather_temperature_celsius{${weatherFilt}}, "source", "outdoor (Open-Meteo)", "", "")
    or
    label_replace(weather_apparent_temperature_celsius{${weatherFilt}}, "source", "feels-like (Open-Meteo)", "", "")
  '';

  combinedHumidityExpr = ''
    label_replace(hass_sensor_humidity_percent{${haFilt}}, "source", "indoor (HA)", "", "")
    or
    label_replace(weather_humidity_percent{${weatherFilt}}, "source", "outdoor (Open-Meteo)", "", "")
  '';

  thDashboard = {
    uid = "myconfig-temperature-humidity";
    title = "Temperature & humidity";
    tags = [
      "myconfig"
      "environment"
      "temperature-humidity"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "1m";
    time = {
      from = "now-24h";
      to = "now";
    };
    annotations.list = [ ];
    templating.list = [
      # Home Assistant sensor selector. Pulls the union of any entity
      # that emits a temperature or humidity sample so a single
      # dropdown drives every HA-sourced panel.
      {
        name = "ha_entity";
        label = "HA entity";
        type = "query";
        datasource = "VictoriaMetrics";
        query = ''label_values({__name__=~"hass_sensor_(temperature_celsius|humidity_percent)|hass_climate_current_temperature_celsius"}, entity)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
        allValue = ".*";
        current = {
          selected = true;
          text = [ "All" ];
          value = [ "$__all" ];
        };
      }
      # AirCO2NTROL host/tag filters — re-declared from host.co2.nix
      # so this dashboard stays standalone.
      {
        name = "co2_host";
        label = "CO2 host";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(air_temp, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "co2_tag";
        label = "CO2 tag";
        type = "query";
        datasource = "VictoriaMetrics";
        query = ''label_values(air_temp{host=~"$co2_host"}, tag)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      # Outdoor weather filters.
      {
        name = "weather_host";
        label = "weather host";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(weather_temperature_celsius, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "weather_location";
        label = "weather location";
        type = "query";
        datasource = "VictoriaMetrics";
        query = ''label_values(weather_temperature_celsius{host=~"$weather_host"}, location)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];
    panels = [
      # -----------------------------------------------------------------
      # Row 0..6: at-a-glance current values across all sources.
      # Each tile reduces a different metric family so the user sees
      # at a glance how indoor / outdoor / sensor probe compare.
      # -----------------------------------------------------------------
      {
        id = 1;
        type = "stat";
        title = "Indoor temperatures (HA sensors)";
        description = ''
          Latest `hass_sensor_temperature_celsius` per entity. One
          tile per sensor — coloured against the indoor-comfort band.
          Drives off the same template var as the CO2/weather panels
          for consistent filtering.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 12;
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
        fieldConfig.defaults = {
          unit = "celsius";
          decimals = 1;
          thresholds = tempThresholds;
        };
        targets = [
          {
            expr = "hass_sensor_temperature_celsius{${haFilt}}";
            legendFormat = "{{friendly_name}}";
            refId = "A";
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Indoor humidity (HA sensors)";
        description = ''
          Latest `hass_sensor_humidity_percent` per entity. Comfort
          band: 40–70 % is green, drier is orange (skin/wood issues),
          wetter is blue (condensation / mould risk).
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 12;
          x = 12;
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
        fieldConfig.defaults = {
          unit = "percent";
          decimals = 0;
          min = 0;
          max = 100;
          thresholds = humidityThresholds;
        };
        targets = [
          {
            expr = "hass_sensor_humidity_percent{${haFilt}}";
            legendFormat = "{{friendly_name}}";
            refId = "A";
          }
        ];
      }

      # -----------------------------------------------------------------
      # Row 6..12: secondary current-value tiles for the non-HA
      # sources (AirCO2NTROL + outdoor weather).
      # -----------------------------------------------------------------
      {
        id = 3;
        type = "stat";
        title = "AirCO2NTROL probe (°C)";
        description = ''
          `air_temp` from the TFA Dostmann AirCO2NTROL Mini, scraped
          by `air_co2_exporter`. Mirrors the temperature tile on the
          CO2 dashboard but shown here next to the HA sensors so you
          can sanity-check both against each other.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 8;
          x = 0;
          y = 6;
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
        };
        fieldConfig.defaults = {
          unit = "celsius";
          decimals = 1;
          thresholds = tempThresholds;
        };
        targets = [
          {
            expr = "air_temp{${co2Filt}}";
            legendFormat = "{{host}} / {{tag}}";
            refId = "A";
          }
        ];
      }
      {
        id = 4;
        type = "stat";
        title = "Outdoor temperature";
        description = ''
          `weather_temperature_celsius` from the Open-Meteo weather
          exporter. Same metric the weather dashboard's
          "Temperature" tile reads.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 8;
          x = 8;
          y = 6;
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
        };
        fieldConfig.defaults = {
          unit = "celsius";
          decimals = 1;
          thresholds = tempThresholds;
        };
        targets = [
          {
            expr = "avg by (location) (weather_temperature_celsius{${weatherFilt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
          {
            expr = "avg by (location) (weather_apparent_temperature_celsius{${weatherFilt}})";
            legendFormat = "{{location}} (feels-like)";
            refId = "B";
          }
        ];
      }
      {
        id = 5;
        type = "stat";
        title = "Outdoor humidity";
        description = "`weather_humidity_percent` from Open-Meteo.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 8;
          x = 16;
          y = 6;
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
        };
        fieldConfig.defaults = {
          unit = "percent";
          decimals = 0;
          min = 0;
          max = 100;
          thresholds = humidityThresholds;
        };
        targets = [
          {
            expr = "avg by (location) (weather_humidity_percent{${weatherFilt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }

      # -----------------------------------------------------------------
      # Row 12..22: combined timeseries — indoor vs outdoor vs probe.
      # This is the headline panel of the dashboard.
      # -----------------------------------------------------------------
      {
        id = 10;
        type = "timeseries";
        title = "Temperature — all sources (°C)";
        description = ''
          Union of every temperature-bearing series this stack
          collects: HA `hass_sensor_temperature_celsius`, HA climate
          probes, the AirCO2NTROL `air_temp`, and Open-Meteo
          `weather_temperature_celsius` / apparent-temperature. A
          synthetic `source` label is added via `label_replace` so
          the legend stays readable.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 12;
        };
        fieldConfig.defaults = {
          unit = "celsius";
          decimals = 1;
          custom = {
            drawStyle = "line";
            lineWidth = 2;
            fillOpacity = 8;
            spanNulls = false;
          };
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
            expr = combinedTempExpr;
            # `source` is the synthetic label injected above;
            # `friendly_name` / `tag` / `location` differentiate
            # individual series within each source.
            legendFormat = "{{source}}: {{friendly_name}}{{tag}}{{location}}";
            refId = "A";
          }
        ];
      }

      # -----------------------------------------------------------------
      # Row 22..32: combined humidity timeseries (indoor + outdoor).
      # -----------------------------------------------------------------
      {
        id = 11;
        type = "timeseries";
        title = "Humidity — all sources (%)";
        description = ''
          HA `hass_sensor_humidity_percent` + Open-Meteo
          `weather_humidity_percent` overlaid. Useful for spotting
          when indoor RH tracks the outdoor curve (poorly insulated
          rooms) vs. stays stable (good envelope).
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 22;
        };
        fieldConfig.defaults = {
          unit = "percent";
          decimals = 0;
          min = 0;
          max = 100;
          custom = {
            drawStyle = "line";
            lineWidth = 2;
            fillOpacity = 8;
            spanNulls = false;
          };
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
            expr = combinedHumidityExpr;
            legendFormat = "{{source}}: {{friendly_name}}{{location}}";
            refId = "A";
          }
        ];
      }

      # -----------------------------------------------------------------
      # Row 32..40: HA-only per-sensor temperature timeseries.
      # Same data as the combined panel above, but split out so the
      # user can drill into a single source without legend clutter
      # from the outdoor / probe series.
      # -----------------------------------------------------------------
      {
        id = 20;
        type = "timeseries";
        title = "HA temperature sensors over time (°C)";
        description = ''
          `hass_sensor_temperature_celsius` per entity. One line per
          HA temperature sensor; filter via `$ha_entity`.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 32;
        };
        fieldConfig.defaults = {
          unit = "celsius";
          decimals = 1;
          custom = {
            drawStyle = "line";
            lineWidth = 2;
            fillOpacity = 10;
            spanNulls = false;
          };
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
            expr = "hass_sensor_temperature_celsius{${haFilt}}";
            legendFormat = "{{friendly_name}}";
            refId = "A";
          }
          {
            expr = "hass_climate_current_temperature_celsius{${haFilt}}";
            legendFormat = "{{friendly_name}} (climate)";
            refId = "B";
          }
        ];
      }
      {
        id = 21;
        type = "timeseries";
        title = "HA humidity sensors over time (%)";
        description = "`hass_sensor_humidity_percent` per entity.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 32;
        };
        fieldConfig.defaults = {
          unit = "percent";
          decimals = 0;
          min = 0;
          max = 100;
          custom = {
            drawStyle = "line";
            lineWidth = 2;
            fillOpacity = 10;
            spanNulls = false;
          };
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
            expr = "hass_sensor_humidity_percent{${haFilt}}";
            legendFormat = "{{friendly_name}}";
            refId = "A";
          }
        ];
      }

      # -----------------------------------------------------------------
      # Row 40..48: per-sensor min / max / mean snapshot table.
      # Lets you scan at a glance which room is consistently hottest
      # / driest / most variable over the dashboard time window.
      # -----------------------------------------------------------------
      {
        id = 30;
        type = "table";
        title = "Per-sensor stats over time window";
        description = ''
          Reduces every HA temperature and humidity series across the
          dashboard's time range to min / mean / max / last. Sorted
          by friendly_name so the same sensor sits at the same row
          when you flip between time ranges.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 40;
        };
        options = {
          showHeader = true;
          footer = {
            show = false;
          };
        };
        fieldConfig = {
          defaults = {
            custom = {
              align = "left";
            };
          };
          overrides = [
            {
              matcher = {
                id = "byRegexp";
                options = ".*temperature.*";
              };
              properties = [
                {
                  id = "unit";
                  value = "celsius";
                }
                {
                  id = "decimals";
                  value = 1;
                }
              ];
            }
            {
              matcher = {
                id = "byRegexp";
                options = ".*humidity.*";
              };
              properties = [
                {
                  id = "unit";
                  value = "percent";
                }
                {
                  id = "decimals";
                  value = 0;
                }
              ];
            }
          ];
        };
        targets = [
          {
            expr = "hass_sensor_temperature_celsius{${haFilt}}";
            legendFormat = "{{friendly_name}} temperature";
            refId = "A";
            instant = true;
            format = "table";
          }
          {
            expr = "hass_sensor_humidity_percent{${haFilt}}";
            legendFormat = "{{friendly_name}} humidity";
            refId = "B";
            instant = true;
            format = "table";
          }
        ];
      }
    ];
  };

  thDashboardFile = pkgs.writeText "temperature-humidity-dashboard.json" (
    builtins.toJSON thDashboard
  );
in
{
  options.myconfig.observability.host.temperatureHumidity = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision the combined "Temperature & humidity" Grafana
        dashboard. Aggregates the temperature/humidity-bearing
        metrics from Home Assistant (`hass_sensor_temperature_celsius`,
        `hass_sensor_humidity_percent`,
        `hass_climate_current_temperature_celsius`), the
        AirCO2NTROL Mini USB sensor (`air_temp`), and the Open-Meteo
        weather scraper (`weather_temperature_celsius`,
        `weather_apparent_temperature_celsius`,
        `weather_humidity_percent`) into a single dashboard.

        This dashboard only reads metrics that other modules already
        ingest — it does not configure any exporter — and does not
        modify the per-source dashboards (CO2, weather, power).
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && thCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-temperature-humidity";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "temperature-humidity-dashboards" { } ''
            mkdir -p $out
            cp ${thDashboardFile} $out/temperature-humidity.json
          '';
        }
      ];
    };
  };
}
