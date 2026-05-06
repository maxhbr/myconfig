# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard for LiteLLM proxy metrics.
#
# The metrics themselves are produced by the LiteLLM service running on
# any client where `services.litellm.enable = true` and
# `myconfig.observability.client.enable = true` — the client module
# wraps the litellm package with `prometheus_client` and registers the
# `prometheus` callback (see modules/myconfig.ai/services.litellm.nix).
# The local vmagent on the client scrapes
# `http://<litellm host>:<port>/metrics` (job=`litellm`) and pushes into
# the central VictoriaMetrics instance via remote_write.
#
# This module only runs on the observability *host* (where Grafana is)
# and provisions a dashboard so the metrics are immediately visualised.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  litellmCfg = hostCfg.litellm;

  # Grafana dashboard for LiteLLM proxy metrics.
  litellmDashboard = {
    uid = "myconfig-litellm";
    title = "LiteLLM";
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
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(litellm_proxy_total_requests_metric_total, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
      {
        name = "model";
        label = "model";
        type = "query";
        datasource = "VictoriaMetrics";
        query = ''label_values(litellm_proxy_total_requests_metric_total{host=~"$host"}, model)'';
        refresh = 2;
        includeAll = true;
        multi = true;
        sort = 1;
      }
    ];
    panels = [
      # ------------------------------------------------------------------
      # Row 0..6: top-line stats
      # ------------------------------------------------------------------
      {
        id = 1;
        type = "stat";
        title = "Requests / s (proxy)";
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
          graphMode = "area";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "reqps";
          decimals = 2;
        };
        targets = [
          {
            expr = ''sum(rate(litellm_proxy_total_requests_metric_total{host=~"$host"}[5m]))'';
            legendFormat = "requests/s";
            refId = "A";
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Failed requests / s";
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
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "reqps";
          decimals = 2;
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
              {
                color = "orange";
                value = 0.001;
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
            expr = ''sum(rate(litellm_proxy_failed_requests_metric_total{host=~"$host"}[5m]))'';
            legendFormat = "failed/s";
            refId = "A";
          }
        ];
      }
      {
        id = 3;
        type = "stat";
        title = "Tokens / s (total)";
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
          colorMode = "value";
          graphMode = "area";
          textMode = "auto";
        };
        fieldConfig.defaults.unit = "short";
        targets = [
          {
            expr = ''sum(rate(litellm_total_tokens_metric_total{host=~"$host"}[5m]))'';
            legendFormat = "tokens/s";
            refId = "A";
          }
        ];
      }
      {
        id = 4;
        type = "stat";
        title = "Total spend ($)";
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
          colorMode = "value";
          graphMode = "area";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "currencyUSD";
          decimals = 4;
        };
        targets = [
          {
            expr = ''sum(litellm_spend_metric_total{host=~"$host"})'';
            legendFormat = "spend";
            refId = "A";
          }
        ];
      }
      # ------------------------------------------------------------------
      # Row 6..14: request rates per model
      # ------------------------------------------------------------------
      {
        id = 10;
        type = "timeseries";
        title = "Requests / s by model";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 6;
        };
        fieldConfig.defaults.unit = "reqps";
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
            expr = ''sum by (model) (rate(litellm_proxy_total_requests_metric_total{host=~"$host", model=~"$model"}[5m]))'';
            legendFormat = "{{model}}";
            refId = "A";
          }
        ];
      }
      {
        id = 11;
        type = "timeseries";
        title = "Failed requests / s by model";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 6;
        };
        fieldConfig.defaults.unit = "reqps";
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
            expr = ''sum by (model, exception_class) (rate(litellm_proxy_failed_requests_metric_total{host=~"$host", model=~"$model"}[5m]))'';
            legendFormat = "{{model}} / {{exception_class}}";
            refId = "A";
          }
        ];
      }
      # ------------------------------------------------------------------
      # Row 14..22: latency
      # ------------------------------------------------------------------
      {
        id = 20;
        type = "timeseries";
        title = "Total request latency p50 / p95 (s)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 14;
        };
        fieldConfig.defaults.unit = "s";
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = ''histogram_quantile(0.50, sum by (le, model) (rate(litellm_request_total_latency_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p50 {{model}}";
            refId = "A";
          }
          {
            expr = ''histogram_quantile(0.95, sum by (le, model) (rate(litellm_request_total_latency_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p95 {{model}}";
            refId = "B";
          }
        ];
      }
      {
        id = 21;
        type = "timeseries";
        title = "LLM API latency p50 / p95 (s)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 14;
        };
        fieldConfig.defaults.unit = "s";
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = ''histogram_quantile(0.50, sum by (le, model) (rate(litellm_llm_api_latency_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p50 {{model}}";
            refId = "A";
          }
          {
            expr = ''histogram_quantile(0.95, sum by (le, model) (rate(litellm_llm_api_latency_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p95 {{model}}";
            refId = "B";
          }
        ];
      }
      {
        id = 22;
        type = "timeseries";
        title = "Time to first token p50 / p95 (s)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 22;
        };
        fieldConfig.defaults.unit = "s";
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = ''histogram_quantile(0.50, sum by (le, model) (rate(litellm_llm_api_time_to_first_token_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p50 {{model}}";
            refId = "A";
          }
          {
            expr = ''histogram_quantile(0.95, sum by (le, model) (rate(litellm_llm_api_time_to_first_token_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p95 {{model}}";
            refId = "B";
          }
        ];
      }
      {
        id = 23;
        type = "timeseries";
        title = "LiteLLM overhead latency (ms)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 22;
        };
        fieldConfig.defaults.unit = "ms";
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = ''histogram_quantile(0.50, sum by (le, model) (rate(litellm_overhead_latency_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p50 {{model}}";
            refId = "A";
          }
          {
            expr = ''histogram_quantile(0.95, sum by (le, model) (rate(litellm_overhead_latency_metric_bucket{host=~"$host", model=~"$model"}[5m])))'';
            legendFormat = "p95 {{model}}";
            refId = "B";
          }
        ];
      }
      # ------------------------------------------------------------------
      # Row 30..38: token throughput
      # ------------------------------------------------------------------
      {
        id = 30;
        type = "timeseries";
        title = "Input tokens / s by model";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 30;
        };
        fieldConfig.defaults.unit = "short";
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
          ];
        };
        targets = [
          {
            expr = ''sum by (model) (rate(litellm_input_tokens_metric_total{host=~"$host", model=~"$model"}[5m]))'';
            legendFormat = "{{model}}";
            refId = "A";
          }
        ];
      }
      {
        id = 31;
        type = "timeseries";
        title = "Output tokens / s by model";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 30;
        };
        fieldConfig.defaults.unit = "short";
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
          ];
        };
        targets = [
          {
            expr = ''sum by (model) (rate(litellm_output_tokens_metric_total{host=~"$host", model=~"$model"}[5m]))'';
            legendFormat = "{{model}}";
            refId = "A";
          }
        ];
      }
      # ------------------------------------------------------------------
      # Row 38..46: deployment health
      # ------------------------------------------------------------------
      {
        id = 40;
        type = "timeseries";
        title = "Deployment state (0 healthy / 1 partial / 2 outage)";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 38;
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          max = 2;
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = ''litellm_deployment_state{host=~"$host"}'';
            legendFormat = "{{litellm_model_name}} ({{api_provider}})";
            refId = "A";
          }
        ];
      }
      {
        id = 41;
        type = "timeseries";
        title = "Deployment success vs failure / s";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 38;
        };
        fieldConfig.defaults.unit = "reqps";
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
            expr = ''sum by (litellm_model_name) (rate(litellm_deployment_success_responses_total{host=~"$host"}[5m]))'';
            legendFormat = "ok {{litellm_model_name}}";
            refId = "A";
          }
          {
            expr = ''sum by (litellm_model_name) (rate(litellm_deployment_failure_responses_total{host=~"$host"}[5m]))'';
            legendFormat = "fail {{litellm_model_name}}";
            refId = "B";
          }
        ];
      }
    ];
  };

  litellmDashboardFile = pkgs.writeText "litellm-dashboard.json" (builtins.toJSON litellmDashboard);
in
{
  options.myconfig.observability.host.litellm = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard for the LiteLLM proxy metrics
        (job=`litellm`, scraped by vmagent on every host that runs
        `services.litellm`).
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && litellmCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = 1;
      providers = [
        {
          name = "myconfig-litellm";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "litellm-dashboards" { } ''
            mkdir -p $out
            cp ${litellmDashboardFile} $out/litellm.json
          '';
        }
      ];
    };
  };
}
