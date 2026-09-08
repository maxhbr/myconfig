# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "ccusage — coding-agent usage" for the metrics
# produced by `myconfig.observability.client.ccusageExporter`
# (ccusage --json → node_exporter textfile collector → vmagent →
# VictoriaMetrics).
#
# The exporter writes *daily cumulative* gauges: each series is
# `ccusage_daily_*{user,period="YYYY-MM-DD"}` where the value is the
# total for that whole day, re-written every refresh interval. A day's
# value therefore appears as a step (0 before any usage, then jumps as
# the day accumulates), and each period only exists for
# `lookbackDays` days before the exporter stops emitting it.
#
# The dashboard follows the weather dashboard's layout conventions:
#   * top row of at-a-glance stat tiles (tokens today, cost today,
#     share of input vs output, scrape health),
#   * timeseries panels for daily tokens and cost (steps),
#   * per-model breakdown (stacked bars per day),
#   * scrape-health panels so outages are visible without logs.
#
# Datasource: VictoriaMetrics (`uid = "victoriametrics"`, referenced
# by name like the other dashboards in this module family).
# Template variables:
#   - $host  multi/all, filters by the originating client
#   - $user  multi/all, filters by the monitored user label
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  ccusageHostCfg = hostCfg.ccusage;

  # Standard label-filter fragment baked into every PromQL expression
  # so the dashboard's template variables apply uniformly. The `user`
  # label is emitted by the exporter; `host` is the vmagent external
  # label added on scrape.
  filt = ''host=~"$host", user=~"$user"'';

  # "Selected day's" total: restrict to the series whose period
  # label matches the $period variable, then take the maximum over
  # the query range. Each series is a step function that accumulates
  # within its day, so `max(...)` yields the day's final cumulative
  # value; `sum` afterwards aggregates over hosts/users.
  dayValue = metric: "max(${metric}{${filt}, period=~\"$period\"})";

  # Cost thresholds (USD) for the stat tiles — perception bands, not
  # alert thresholds.
  costThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "green";
        value = null;
      }
      {
        color = "yellow";
        value = 1;
      }
      {
        color = "orange";
        value = 5;
      }
      {
        color = "red";
        value = 20;
      }
    ];
  };

  freshnessThresholds = {
    mode = "absolute";
    steps = [
      {
        color = "green";
        value = null;
      }
      {
        color = "yellow";
        value = 1800;
      }
      {
        color = "orange";
        value = 3600;
      }
      {
        color = "red";
        value = 86400;
      }
    ];
  };

  # Legend-calculation block shared by the timeseries panels.
  legendTable = {
    displayMode = "table";
    placement = "bottom";
    calcs = [
      "lastNotNull"
      "max"
      "sum"
    ];
  };

  ccusageDashboard = {
    uid = "myconfig-ccusage";
    title = "ccusage — coding-agent usage";
    tags = [
      "myconfig"
      "ai"
      "usage"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    # The exporter refreshes every 10 minutes; a 5 minute dashboard
    # refresh is enough for a human-facing view.
    refresh = "5m";
    # Default view: the lookback window of the exporter (30 days).
    time = {
      from = "now-30d";
      to = "now";
    };
    annotations.list = [ ];
    templating.list = [
      {
        name = "host";
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(ccusage_scrape_success, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "user";
        label = "user";
        type = "query";
        datasource = "VictoriaMetrics";
        query = ''label_values(ccusage_scrape_success{host=~"$host"}, user)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        # Period selector for the "at a glance" stat tiles: defaults
        # to the newest day with data, but any past day within the
        # lookback window can be inspected. `refresh = 2` re-queries
        # on every dashboard load so "today" stays current.
        name = "period";
        label = "period";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "sort_desc(label_values(ccusage_daily_total_tokens{${filt}}, period))";
        refresh = 2;
        includeAll = false;
        multi = false;
        # `current` deliberately omitted: Grafana picks the first
        # (newest, thanks to sort_desc) entry on first load, which is
        # "today" or the most recent day with usage.
      }
    ];
    panels = [
      # ---------------------------------------------------------------
      # Row 0..6: at-a-glance — tokens & cost for $period
      # ---------------------------------------------------------------
      {
        id = 1;
        type = "stat";
        title = "Tokens (total)";
        description = "Total tokens consumed on the selected day, summed over all agents and models.";
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
          colorMode = "value";
          graphMode = "none";
          textMode = "value_and_name";
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 2;
        };
        targets = [
          {
            expr = "sum(${dayValue "ccusage_daily_total_tokens"})";
            legendFormat = "tokens";
            refId = "A";
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Cost (USD)";
        description = "LiteLLM-estimated USD cost for the selected day, summed over all agents and models.";
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
          graphMode = "none";
          textMode = "value_and_name";
        };
        fieldConfig.defaults = {
          unit = "currencyUSD";
          decimals = 2;
          thresholds = costThresholds;
        };
        targets = [
          {
            expr = "sum(${dayValue "ccusage_daily_cost_usd"})";
            legendFormat = "cost";
            refId = "A";
          }
        ];
      }
      {
        id = 3;
        type = "stat";
        title = "Input / output / cache";
        description = "Token split for the selected day: input, output, cache-creation and cache-read.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 8;
          x = 12;
          y = 0;
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
          unit = "short";
          decimals = 2;
        };
        targets = [
          {
            expr = "sum(${dayValue "ccusage_daily_input_tokens"})";
            legendFormat = "input";
            refId = "A";
          }
          {
            expr = "sum(${dayValue "ccusage_daily_output_tokens"})";
            legendFormat = "output";
            refId = "B";
          }
          {
            expr = "sum(${dayValue "ccusage_daily_cache_creation_tokens"})";
            legendFormat = "cache creation";
            refId = "C";
          }
          {
            expr = "sum(${dayValue "ccusage_daily_cache_read_tokens"})";
            legendFormat = "cache read";
            refId = "D";
          }
        ];
      }
      {
        id = 4;
        type = "stat";
        title = "Data freshness";
        description = ''
          Age of the last successful ccusage run. Beyond ~20 min
          (for the 10 min refresh interval) the exporter is stale —
          check `systemctl status myconfig-ccusage`.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 6;
          w = 4;
          x = 20;
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
          textMode = "value_and_name";
        };
        fieldConfig.defaults = {
          unit = "s";
          decimals = 0;
          thresholds = freshnessThresholds;
        };
        targets = [
          {
            expr = "time() - max by (host, user) (ccusage_scrape_timestamp_seconds{${filt}})";
            legendFormat = "{{host}}";
            refId = "A";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 6..14: daily tokens + cost (step timeseries)
      # ---------------------------------------------------------------
      {
        id = 10;
        type = "timeseries";
        title = "Tokens per day";
        description = ''
          Daily token consumption (input / output / cache), drawn as
          steps: each day's series carries the cumulative total for
          that day, refreshed every export interval. Use the legend
          table's `max` column for a day's final value.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 16;
          x = 0;
          y = 6;
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 1;
          custom = {
            drawStyle = "line";
            lineInterpolation = "stepAfter";
            lineWidth = 2;
            fillOpacity = 10;
            stacking = {
              group = "A";
              mode = "normal";
            };
          };
        };
        options.legend = legendTable;
        targets = [
          {
            expr = "ccusage_daily_input_tokens{${filt}}";
            legendFormat = "{{host}}/{{user}} input";
            refId = "A";
          }
          {
            expr = "ccusage_daily_output_tokens{${filt}}";
            legendFormat = "{{host}}/{{user}} output";
            refId = "B";
          }
          {
            expr = "ccusage_daily_cache_creation_tokens{${filt}}";
            legendFormat = "{{host}}/{{user}} cache create";
            refId = "C";
          }
          {
            expr = "ccusage_daily_cache_read_tokens{${filt}}";
            legendFormat = "{{host}}/{{user}} cache read";
            refId = "D";
          }
        ];
      }
      {
        id = 11;
        type = "timeseries";
        title = "Cost per day (USD)";
        description = "LiteLLM-estimated USD cost per day.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 8;
          x = 16;
          y = 6;
        };
        fieldConfig.defaults = {
          unit = "currencyUSD";
          decimals = 2;
          custom = {
            drawStyle = "line";
            lineInterpolation = "stepAfter";
            lineWidth = 2;
            fillOpacity = 30;
            gradientMode = "opacity";
          };
        };
        options.legend = legendTable;
        targets = [
          {
            expr = "ccusage_daily_cost_usd{${filt}}";
            legendFormat = "{{host}}/{{user}}";
            refId = "A";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 14..22: per-model breakdown (stacked bars)
      # ---------------------------------------------------------------
      {
        id = 20;
        type = "timeseries";
        title = "Input tokens per model per day";
        description = "Per-model input-token consumption, stacked per day.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 14;
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 1;
          custom = {
            drawStyle = "bars";
            lineWidth = 1;
            fillOpacity = 80;
            barAlignment = 0;
            stacking = {
              group = "A";
              mode = "normal";
            };
          };
        };
        options.legend = legendTable;
        targets = [
          {
            expr = "ccusage_model_input_tokens{${filt}}";
            legendFormat = "{{host}}/{{user}} {{model}}";
            refId = "A";
          }
        ];
      }
      {
        id = 21;
        type = "timeseries";
        title = "Output tokens per model per day";
        description = "Per-model output-token consumption, stacked per day.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 14;
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 1;
          custom = {
            drawStyle = "bars";
            lineWidth = 1;
            fillOpacity = 80;
            barAlignment = 0;
            stacking = {
              group = "A";
              mode = "normal";
            };
          };
        };
        options.legend = legendTable;
        targets = [
          {
            expr = "ccusage_model_output_tokens{${filt}}";
            legendFormat = "{{host}}/{{user}} {{model}}";
            refId = "A";
          }
        ];
      }

      # ---------------------------------------------------------------
      # Row 22..28: per-model cost + scrape health
      # ---------------------------------------------------------------
      {
        id = 30;
        type = "timeseries";
        title = "Cost per model per day (USD)";
        description = "Per-model LiteLLM-estimated USD cost, stacked per day.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 16;
          x = 0;
          y = 22;
        };
        fieldConfig.defaults = {
          unit = "currencyUSD";
          decimals = 2;
          custom = {
            drawStyle = "bars";
            lineWidth = 1;
            fillOpacity = 80;
            barAlignment = 0;
            stacking = {
              group = "A";
              mode = "normal";
            };
          };
        };
        options.legend = legendTable;
        targets = [
          {
            expr = "ccusage_model_cost_usd{${filt}}";
            legendFormat = "{{host}}/{{user}} {{model}}";
            refId = "A";
          }
        ];
      }
      {
        id = 31;
        type = "timeseries";
        title = "Scrape success";
        description = ''
          1 = the most recent ccusage run on this host succeeded.
          0 = failure (ccusage invocation failed or produced invalid
          JSON); the gauges stay at their last good values until the
          next successful refresh.
        '';
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 8;
          x = 16;
          y = 22;
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
            expr = "ccusage_scrape_success{${filt}}";
            legendFormat = "{{host}}/{{user}}";
            refId = "A";
          }
        ];
      }
    ];
  };

  ccusageDashboardFile = pkgs.writeText "ccusage-dashboard.json" (builtins.toJSON ccusageDashboard);
in
{
  options.myconfig.observability.host.ccusage = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard for the coding-agent usage
        metrics produced by
        `myconfig.observability.client.ccusageExporter`.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && ccusageHostCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-ccusage";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          # Group with the other AI-related dashboards (LiteLLM,
          # llama-server, llama-swap) under the "AI" folder.
          folder = "AI";
          options.path = pkgs.runCommand "ccusage-dashboards" { } ''
            mkdir -p $out
            cp ${ccusageDashboardFile} $out/ccusage.json
          '';
        }
      ];
    };
  };
}
