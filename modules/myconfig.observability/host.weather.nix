# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "Outdoor weather" for the metrics produced by
# `myconfig.observability.client.weatherExporter` (Open-Meteo →
# node_exporter textfile collector → vmagent → VictoriaMetrics).
#
# The dashboard is intentionally comprehensive: current conditions
# at a glance (temp / feels-like / humidity / wind / pressure /
# precipitation / UV / conditions / day-or-night), forecast min/max
# for today, 24 h timeseries for temperature, humidity, wind,
# pressure, cloud cover and precipitation, plus a scrape-health
# panel so you can spot Open-Meteo outages without trawling logs.
#
# Datasource: VictoriaMetrics. Template variables:
#   - $host      multi/all, filters by the originating client
#   - $location  multi/all, filters by the location label
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  weatherCfg = hostCfg.weather;

  # Comfort thresholds (°C). Tuned for Central European indoor/outdoor
  # perception — these are colour bands for the stat tile, NOT alert
  # thresholds.
  tempThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "purple";
        value = null;
      }
      {
        color = "blue";
        value = -5;
      }
      {
        color = "light-blue";
        value = 5;
      }
      {
        color = "green";
        value = 15;
      }
      {
        color = "yellow";
        value = 25;
      }
      {
        color = "orange";
        value = 30;
      }
      {
        color = "red";
        value = 35;
      }
    ];
  };

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
        value = 70;
      }
      {
        color = "blue";
        value = 85;
      }
    ];
  };

  windThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "green";
        value = null;
      }
      {
        color = "yellow";
        value = 20;
      }
      {
        color = "orange";
        value = 40;
      }
      {
        color = "red";
        value = 60;
      }
    ];
  };

  uvThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "green";
        value = null;
      }
      {
        color = "yellow";
        value = 3;
      }
      {
        color = "orange";
        value = 6;
      }
      {
        color = "red";
        value = 8;
      }
      {
        color = "purple";
        value = 11;
      }
    ];
  };

  precipThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "green";
        value = null;
      }
      {
        color = "blue";
        value = 0.1;
      }
      {
        color = "yellow";
        value = 2;
      }
      {
        color = "orange";
        value = 10;
      }
      {
        color = "red";
        value = 25;
      }
    ];
  };

  # Standard label-filter fragment baked into every PromQL expression
  # so the dashboard's template variables apply uniformly.
  filt = ''host=~"$host", location=~"$location"'';

  weatherDashboard = {
    uid = "myconfig-weather";
    title = "Outdoor weather";
    tags = [
      "myconfig"
      "environment"
      "weather"
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
      {
        name = "host";
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(weather_temperature_celsius, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "location";
        label = "location";
        type = "query";
        datasource = "VictoriaMetrics";
        query = ''label_values(weather_temperature_celsius{host=~"$host"}, location)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];
    panels = [
      # ---------------------------------------------------------------
      # Row 0..6: at-a-glance current conditions (8 stat tiles, 2 rows)
      # ---------------------------------------------------------------
      {
        id = 1;
        type = "stat";
        title = "Temperature";
        description = "Current air temperature at 2 m above ground.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
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
            expr = "avg by (location) (weather_temperature_celsius{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Feels like";
        description = "Apparent (feels-like) temperature, factoring in humidity and wind.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
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
            expr = "avg by (location) (weather_apparent_temperature_celsius{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 3;
        type = "stat";
        title = "Humidity";
        description = "Relative humidity at 2 m above ground.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
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
            expr = "avg by (location) (weather_humidity_percent{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 4;
        type = "stat";
        title = "Wind";
        description = "Wind speed at 10 m above ground.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
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
          colorMode = "background";
          graphMode = "area";
          textMode = "value_and_name";
        };
        fieldConfig.defaults = {
          unit = "velocitykmh";
          decimals = 1;
          thresholds = windThresholds;
        };
        targets = [
          {
            expr = "avg by (location) (weather_wind_speed_kmh{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 5;
        type = "stat";
        title = "Pressure (MSL)";
        description = "Atmospheric pressure reduced to mean sea level.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 6;
          x = 0;
          y = 6;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "area";
          textMode = "value_and_name";
        };
        fieldConfig.defaults = {
          unit = "pressurehpa";
          decimals = 1;
        };
        targets = [
          {
            expr = "avg by (location) (weather_pressure_msl_hpa{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 6;
        type = "stat";
        title = "Cloud cover";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 6;
          x = 6;
          y = 6;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "area";
          textMode = "value_and_name";
        };
        fieldConfig.defaults = {
          unit = "percent";
          decimals = 0;
          min = 0;
          max = 100;
        };
        targets = [
          {
            expr = "avg by (location) (weather_cloud_cover_percent{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 7;
        type = "stat";
        title = "UV index (max today)";
        description = "Forecast peak UV index for the day. ≥6 → sunscreen; ≥8 → seek shade; ≥11 → extreme.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 6;
          x = 12;
          y = 6;
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
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 1;
          thresholds = uvThresholds;
        };
        targets = [
          {
            expr = "avg by (location) (weather_uv_index_max{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 8;
        type = "stat";
        title = "Conditions";
        description = "Current WMO weather code with a human-readable description.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 6;
          x = 18;
          y = 6;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "/^description$/";
            values = false;
          };
          colorMode = "value";
          graphMode = "none";
          textMode = "value_and_name";
        };
        fieldConfig.defaults = {
          color.mode = "fixed";
          color.fixedColor = "blue";
        };
        targets = [
          {
            # Carry the `description` label out of weather_info into
            # the field name so Grafana's stat renderer can display
            # it as text.
            expr = "weather_info{${filt}}";
            legendFormat = "{{description}}";
            refId = "A";
            instant = true;
            format = "table";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 12..16: today's range + sun times
      # ---------------------------------------------------------------
      {
        id = 10;
        type = "stat";
        title = "Today min / max";
        description = "Forecast minimum and maximum air temperature for the current day.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 4;
          w = 12;
          x = 0;
          y = 12;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "none";
          textMode = "value_and_name";
          orientation = "horizontal";
        };
        fieldConfig.defaults = {
          unit = "celsius";
          decimals = 1;
          thresholds = tempThresholds;
        };
        targets = [
          {
            expr = "avg by (location) (weather_temperature_min_celsius{${filt}})";
            legendFormat = "min ({{location}})";
            refId = "A";
          }
          {
            expr = "avg by (location) (weather_temperature_max_celsius{${filt}})";
            legendFormat = "max ({{location}})";
            refId = "B";
          }
        ];
      }
      {
        id = 11;
        type = "stat";
        title = "Sunrise / sunset";
        description = "Today's sunrise and sunset (local time).";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 4;
          w = 12;
          x = 12;
          y = 12;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "none";
          textMode = "value_and_name";
          orientation = "horizontal";
        };
        fieldConfig.defaults = {
          unit = "dateTimeAsLocalNoDateIfToday";
          color.mode = "fixed";
          color.fixedColor = "orange";
        };
        # `dateTimeAsLocalNoDateIfToday` interprets the gauge value
        # as milliseconds since epoch, but our metrics are in
        # seconds; multiply by 1000 in PromQL.
        targets = [
          {
            expr = "weather_sunrise_timestamp_seconds{${filt}} * 1000";
            legendFormat = "sunrise ({{location}})";
            refId = "A";
          }
          {
            expr = "weather_sunset_timestamp_seconds{${filt}} * 1000";
            legendFormat = "sunset ({{location}})";
            refId = "B";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 16..24: temperature + apparent temperature timeseries
      # ---------------------------------------------------------------
      {
        id = 20;
        type = "timeseries";
        title = "Temperature & feels-like (°C)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 24;
          x = 0;
          y = 16;
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
            expr = "weather_temperature_celsius{${filt}}";
            legendFormat = "{{location}} temp";
            refId = "A";
          }
          {
            expr = "weather_apparent_temperature_celsius{${filt}}";
            legendFormat = "{{location}} feels-like";
            refId = "B";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 24..32: humidity + cloud cover (right) timeseries
      # ---------------------------------------------------------------
      {
        id = 21;
        type = "timeseries";
        title = "Humidity & cloud cover (%)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 24;
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
            expr = "weather_humidity_percent{${filt}}";
            legendFormat = "{{location}} humidity";
            refId = "A";
          }
          {
            expr = "weather_cloud_cover_percent{${filt}}";
            legendFormat = "{{location}} clouds";
            refId = "B";
          }
        ];
      }
      {
        id = 22;
        type = "timeseries";
        title = "Pressure (hPa)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 24;
        };
        fieldConfig.defaults = {
          unit = "pressurehpa";
          decimals = 1;
          custom = {
            drawStyle = "line";
            lineWidth = 2;
            fillOpacity = 5;
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
            expr = "weather_pressure_msl_hpa{${filt}}";
            legendFormat = "{{location}} MSL";
            refId = "A";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 32..40: wind speed/gusts and wind direction
      # ---------------------------------------------------------------
      {
        id = 30;
        type = "timeseries";
        title = "Wind speed & gusts (km/h)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 16;
          x = 0;
          y = 32;
        };
        fieldConfig.defaults = {
          unit = "velocitykmh";
          decimals = 1;
          custom = {
            drawStyle = "line";
            lineWidth = 2;
            fillOpacity = 10;
          };
        };
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
            expr = "weather_wind_speed_kmh{${filt}}";
            legendFormat = "{{location}} speed";
            refId = "A";
          }
          {
            expr = "weather_wind_gusts_kmh{${filt}}";
            legendFormat = "{{location}} gusts";
            refId = "B";
          }
        ];
      }
      {
        id = 31;
        type = "gauge";
        title = "Wind direction (°, meteorological)";
        description = ''
          Direction the wind is blowing FROM, in degrees clockwise
          from North (meteorological convention: 0° = N, 90° = E,
          180° = S, 270° = W).
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 8;
          x = 16;
          y = 32;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          showThresholdLabels = false;
          showThresholdMarkers = true;
          orientation = "auto";
        };
        fieldConfig.defaults = {
          unit = "degree";
          min = 0;
          max = 360;
          decimals = 0;
          color.mode = "fixed";
          color.fixedColor = "blue";
        };
        targets = [
          {
            expr = "avg by (location) (weather_wind_direction_degrees{${filt}})";
            legendFormat = "{{location}}";
            refId = "A";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 40..48: precipitation (bar chart)
      # ---------------------------------------------------------------
      {
        id = 40;
        type = "timeseries";
        title = "Precipitation (mm per interval)";
        description = ''
          Precipitation amount reported by Open-Meteo for each scrape
          interval (rain + showers + snow water equivalent).
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 24;
          x = 0;
          y = 40;
        };
        fieldConfig.defaults = {
          unit = "lengthmm";
          decimals = 1;
          min = 0;
          thresholds = precipThresholds;
          custom = {
            drawStyle = "bars";
            lineWidth = 1;
            fillOpacity = 80;
            barAlignment = 0;
            gradientMode = "opacity";
          };
          color.mode = "thresholds";
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "sum"
            "max"
          ];
        };
        targets = [
          {
            expr = "weather_precipitation_mm{${filt}}";
            legendFormat = "{{location}} total";
            refId = "A";
          }
          {
            expr = "weather_rain_mm{${filt}}";
            legendFormat = "{{location}} rain";
            refId = "B";
          }
          {
            expr = "weather_showers_mm{${filt}}";
            legendFormat = "{{location}} showers";
            refId = "C";
          }
          {
            expr = "weather_snowfall_cm{${filt}} * 10"; # cm → mm-equivalent ≈ *10 (rough)
            legendFormat = "{{location}} snow (≈mm)";
            refId = "D";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 48..56: scrape health
      # ---------------------------------------------------------------
      {
        id = 90;
        type = "timeseries";
        title = "Scrape success";
        description = ''
          1 = the most recent Open-Meteo fetch on this host succeeded
          (HTTP 200 + valid JSON). 0 = failure; previously-emitted
          gauges remain unchanged until the next successful refresh.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 12;
          x = 0;
          y = 48;
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
            expr = "weather_scrape_success{${filt}}";
            legendFormat = "{{host}} / {{location}}";
            refId = "A";
          }
        ];
      }
      {
        id = 91;
        type = "stat";
        title = "Data freshness";
        description = ''
          Age of the last successful Open-Meteo response. Anything
          beyond ~15 min for a 10 min refresh interval points at
          network or API trouble.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 12;
          x = 12;
          y = 48;
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
        };
        fieldConfig.defaults = {
          unit = "s";
          decimals = 0;
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
              {
                color = "yellow";
                value = 900;
              }
              {
                color = "orange";
                value = 1800;
              }
              {
                color = "red";
                value = 3600;
              }
            ];
          };
        };
        targets = [
          {
            expr = "time() - max by (host, location) (weather_scrape_timestamp_seconds{${filt}})";
            legendFormat = "{{host}} / {{location}}";
            refId = "A";
          }
        ];
      }
    ];
  };

  weatherDashboardFile = pkgs.writeText "weather-dashboard.json" (builtins.toJSON weatherDashboard);
in
{
  options.myconfig.observability.host.weather = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard for the outdoor weather metrics
        produced by `myconfig.observability.client.weatherExporter`.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && weatherCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-weather";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "weather-dashboards" { } ''
            mkdir -p $out
            cp ${weatherDashboardFile} $out/weather.json
          '';
        }
      ];
    };
  };
}
