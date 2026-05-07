# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "Power usage": consumption + energy view sourced
# from the Home Assistant Prometheus exporter
# (https://www.home-assistant.io/integrations/prometheus/).
#
# Relevant metrics emitted by HA for any sensor with the appropriate
# `device_class` / unit:
#   - `hass_sensor_power_w`     (gauge, instantaneous watts)
#   - `hass_sensor_energy_kwh`  (gauge that monotonically increases for
#                                `total_increasing` energy sensors;
#                                we use `increase()` over it to derive
#                                consumption per period)
#
# Both metrics carry the labels HA's exporter sets — at minimum
# `entity` and `friendly_name`; this dashboard groups by
# `friendly_name` (falling back to `entity` where it isn't set) so the
# legend is human-readable.
#
# This module only runs on the observability *host* (where Grafana
# is) and provisions a dashboard so the metrics are immediately
# visualised. Scraping HA itself is set up wherever the HA exporter
# lives; this module makes no assumption about the scrape job name.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  powerCfg = hostCfg.power;

  # Common label-filter fragment, applied to every query so the
  # template variables are honoured. `entity=~"$entity"` lets the user
  # narrow down to a single device when needed.
  filt = ''entity=~"$entity"'';

  # ---------------------------------------------------------------------
  # Top-line stats
  # ---------------------------------------------------------------------
  overviewPanels = [
    {
      id = 100;
      type = "stat";
      title = "Current total power (W)";
      description = ''
        Sum of `hass_sensor_power_w` across all sensors matching the
        `entity` filter — instantaneous draw of the whole house (or
        whichever subset is selected).
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
        textMode = "auto";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "watt";
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
              value = 1000;
            }
            {
              color = "orange";
              value = 3000;
            }
            {
              color = "red";
              value = 6000;
            }
          ];
        };
      };
      targets = [
        {
          expr = "sum(hass_sensor_power_w{${filt}})";
          legendFormat = "total";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 101;
      type = "stat";
      title = "Energy used (last 24h, kWh)";
      description = ''
        `increase(hass_sensor_energy_kwh[24h])` summed over all
        matching sensors. Uses `increase()` to handle counter resets
        in HA's `total_increasing` sensors.
      '';
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
        colorMode = "value";
        graphMode = "area";
        textMode = "auto";
      };
      fieldConfig.defaults = {
        unit = "kwatth";
        decimals = 2;
      };
      targets = [
        {
          expr = "sum(increase(hass_sensor_energy_kwh{${filt}}[24h]))";
          legendFormat = "24h";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 102;
      type = "stat";
      title = "Energy used (last 7d, kWh)";
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
        colorMode = "value";
        graphMode = "area";
        textMode = "auto";
      };
      fieldConfig.defaults = {
        unit = "kwatth";
        decimals = 2;
      };
      targets = [
        {
          expr = "sum(increase(hass_sensor_energy_kwh{${filt}}[7d]))";
          legendFormat = "7d";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 103;
      type = "stat";
      title = "Energy used (last 30d, kWh)";
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
        colorMode = "value";
        graphMode = "area";
        textMode = "auto";
      };
      fieldConfig.defaults = {
        unit = "kwatth";
        decimals = 2;
      };
      targets = [
        {
          expr = "sum(increase(hass_sensor_energy_kwh{${filt}}[30d]))";
          legendFormat = "30d";
          refId = "A";
          instant = true;
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Power (W) row — instantaneous draw
  # ---------------------------------------------------------------------
  powerPanels = [
    {
      id = 200;
      type = "timeseries";
      title = "Power per sensor (W)";
      description = ''
        `hass_sensor_power_w` per sensor — top 15 by latest value, so
        a noisy sensor list stays readable. Stacked so the total
        equals the household draw when all sensors are selected.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 10;
        w = 24;
        x = 0;
        y = 8;
      };
      fieldConfig.defaults = {
        unit = "watt";
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
              max by (friendly_name, entity) (
                hass_sensor_power_w{${filt}}
              )
            )
          '';
          legendFormat = "{{friendly_name}}";
          refId = "A";
        }
      ];
    }
    {
      id = 201;
      type = "timeseries";
      title = "Total power (W)";
      description = ''
        `sum(hass_sensor_power_w)` — single line tracking the
        aggregate instantaneous draw over time.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 18;
      };
      fieldConfig.defaults = {
        unit = "watt";
        custom = {
          drawStyle = "line";
          fillOpacity = 20;
        };
      };
      options.legend = {
        displayMode = "list";
        placement = "bottom";
        calcs = [
          "lastNotNull"
          "max"
          "mean"
        ];
      };
      targets = [
        {
          expr = "sum(hass_sensor_power_w{${filt}})";
          legendFormat = "total";
          refId = "A";
        }
      ];
    }
    {
      id = 202;
      type = "table";
      title = "Top consumers right now";
      description = ''
        Snapshot of `hass_sensor_power_w` ordered by current draw —
        quick way to see which device is using the most power.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 18;
      };
      transformations = [
        {
          id = "organize";
          options = {
            excludeByName = {
              Time = true;
              __name__ = true;
              job = true;
              instance = true;
            };
            indexByName = {
              friendly_name = 0;
              entity = 1;
              Value = 2;
            };
            renameByName = {
              Value = "watts";
            };
          };
        }
      ];
      fieldConfig = {
        defaults = { };
        overrides = [
          {
            matcher = {
              id = "byName";
              options = "watts";
            };
            properties = [
              {
                id = "unit";
                value = "watt";
              }
              {
                id = "decimals";
                value = 1;
              }
              {
                id = "custom.cellOptions";
                value = {
                  type = "color-background";
                  mode = "gradient";
                };
              }
            ];
          }
        ];
      };
      targets = [
        {
          expr = "topk(20, hass_sensor_power_w{${filt}})";
          legendFormat = "{{friendly_name}}";
          refId = "A";
          format = "table";
          instant = true;
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Energy (kWh) row — cumulative + period-over-period
  # ---------------------------------------------------------------------
  energyPanels = [
    {
      id = 300;
      type = "timeseries";
      title = "Energy use rate (kWh / hour, per sensor)";
      description = ''
        `increase(hass_sensor_energy_kwh[1h])` — kWh consumed in the
        trailing hour, per sensor (top 15). `increase()` handles
        counter resets so this works even after HA restarts.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 10;
        w = 24;
        x = 0;
        y = 27;
      };
      fieldConfig.defaults = {
        unit = "kwatth";
        custom = {
          drawStyle = "bars";
          fillOpacity = 60;
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
          "sum"
          "max"
        ];
      };
      targets = [
        {
          expr = ''
            topk(15,
              sum by (friendly_name, entity) (
                increase(hass_sensor_energy_kwh{${filt}}[1h])
              )
            )
          '';
          legendFormat = "{{friendly_name}}";
          refId = "A";
        }
      ];
    }
    {
      id = 301;
      type = "timeseries";
      title = "Daily energy use (kWh, total)";
      description = ''
        `increase(hass_sensor_energy_kwh[24h])` summed across all
        selected sensors — one bar per day to compare days at a
        glance.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 37;
      };
      fieldConfig.defaults = {
        unit = "kwatth";
        custom = {
          drawStyle = "bars";
          fillOpacity = 80;
        };
      };
      options.legend = {
        displayMode = "list";
        placement = "bottom";
        calcs = [
          "lastNotNull"
          "max"
          "mean"
        ];
      };
      targets = [
        {
          expr = "sum(increase(hass_sensor_energy_kwh{${filt}}[24h]))";
          legendFormat = "kWh / 24h";
          refId = "A";
          interval = "1d";
        }
      ];
    }
    {
      id = 302;
      type = "timeseries";
      title = "Cumulative energy (kWh, raw counters)";
      description = ''
        Raw `hass_sensor_energy_kwh` values — useful to verify
        sensors are reporting and to spot counter resets.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 37;
      };
      fieldConfig.defaults = {
        unit = "kwatth";
        custom = {
          drawStyle = "line";
          lineInterpolation = "linear";
          fillOpacity = 5;
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [ "lastNotNull" ];
      };
      targets = [
        {
          expr = "hass_sensor_energy_kwh{${filt}}";
          legendFormat = "{{friendly_name}}";
          refId = "A";
        }
      ];
    }
    {
      id = 303;
      type = "table";
      title = "Energy by sensor (selected range)";
      description = ''
        `increase(hass_sensor_energy_kwh[$__range])` — total kWh
        consumed by each sensor across the dashboard's currently
        selected time range. Recompute by changing the time picker.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 10;
        w = 24;
        x = 0;
        y = 45;
      };
      transformations = [
        {
          id = "organize";
          options = {
            excludeByName = {
              Time = true;
              __name__ = true;
              job = true;
              instance = true;
            };
            indexByName = {
              friendly_name = 0;
              entity = 1;
              Value = 2;
            };
            renameByName = {
              Value = "kWh";
            };
          };
        }
        {
          id = "sortBy";
          options = {
            fields = { };
            sort = [
              {
                field = "kWh";
                desc = true;
              }
            ];
          };
        }
      ];
      fieldConfig = {
        defaults = { };
        overrides = [
          {
            matcher = {
              id = "byName";
              options = "kWh";
            };
            properties = [
              {
                id = "unit";
                value = "kwatth";
              }
              {
                id = "decimals";
                value = 3;
              }
              {
                id = "custom.cellOptions";
                value = {
                  type = "color-background";
                  mode = "gradient";
                };
              }
            ];
          }
        ];
      };
      targets = [
        {
          expr = ''
            sum by (friendly_name, entity) (
              increase(hass_sensor_energy_kwh{${filt}}[$__range])
            )
          '';
          legendFormat = "{{friendly_name}}";
          refId = "A";
          format = "table";
          instant = true;
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

  powerRow = {
    id = 2;
    type = "row";
    title = "Power (W)";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 7;
    };
    panels = [ ];
  };

  energyRow = {
    id = 3;
    type = "row";
    title = "Energy (kWh)";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 26;
    };
    panels = [ ];
  };

  powerDashboard = {
    uid = "myconfig-power";
    title = "Power usage";
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
        name = "entity";
        label = "entity";
        type = "query";
        datasource = "VictoriaMetrics";
        # Source the entity list from the power metric so the
        # variable is populated as soon as any sensor is exposed —
        # energy-only sensors will still match because most power
        # plugs in HA emit *both* metrics.
        query = "label_values(hass_sensor_power_w, entity)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];
    annotations.list = [ ];
    panels = [
      overviewRow
    ]
    ++ overviewPanels
    ++ [ powerRow ]
    ++ powerPanels
    ++ [ energyRow ]
    ++ energyPanels;
  };

  powerDashboardFile = pkgs.writeText "power-dashboard.json" (builtins.toJSON powerDashboard);
in
{
  options.myconfig.observability.host.power = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision the "Power usage" Grafana dashboard, which uses the
        Home Assistant Prometheus exporter metrics
        `hass_sensor_power_w` (instantaneous watts) and
        `hass_sensor_energy_kwh` (cumulative energy) to show
        per-device power draw, current top consumers, hourly /
        daily / period-over-period energy use, and raw counters.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && powerCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = 1;
      providers = [
        {
          name = "myconfig-power";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "power-dashboards" { } ''
            mkdir -p $out
            cp ${powerDashboardFile} $out/power.json
          '';
        }
      ];
    };
  };
}
