# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "Llama-server models": visualises llama-server /
# llama-swap model state, metadata and timing performance sourced from
# the Prometheus gauges produced by ``client.llama-server.nix``.
#
# Two exporters feed metrics:
#
# 1. ``llama-server-model-exporter.py`` – scrapes ``GET /v1/models`` and
#    exposes gauges (``llama_server_model_status``, ``llama_server_model_info``,
#    ``llama_server_model_ctx_size``, etc.).  Visualised in the model-roster
#    and capacity sections.
#
# 2. ``llama-server-timing-exporter.py`` – tails the systemd journal and
#    parses ``slot print_timing`` lines from the llama-cpp service.  Emits
#    ``llama_server_tg_tokens_per_second`` (rolling live gauge) and per-task
#    metrics (``llama_server_task_tg_tokens_per_second``,
#    ``llama_server_task_prompt_eval_tokens_per_second``,
#    ``llama_server_task_total_time_ms``, ``llama_server_task_n_decoded_tokens``,
#    ``llama_server_task_n_prompt_tokens``).  Visualised in the "Timing"
#    section.
#
# The local vmagent scrapes these gauges and remote-writes them into
# the central VictoriaMetrics instance.
#
# This module runs only on the observability *host* (where Grafana is)
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
  llamaCfg = hostCfg.llamaServer;

  # ------------------------------------------------------------------
  # Dashboard JSON (rendered to a file for Grafana file provisioning)
  # ------------------------------------------------------------------
  llamaDashboard = {
    uid = "myconfig-llama-server";
    title = "Llama-server models";
    schemaVersion = 39;
    version = 2;
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
        query = "label_values(llama_server_model_info, host)";
        refresh = 2;
        includeAll = false;
        multi = false;
        sort = 1;
      }
      {
        name = "model";
        label = "model";
        type = "query";
        datasource = "VictoriaMetrics";
        query = "label_values(llama_server_model_info{host=~\"$host\"}, model)";
        refresh = 2;
        includeAll = true;
        multi = true;
        allValue = ".*";
        sort = 1;
      }
    ];
    panels = [
      # ================================================================
      # Row 0: Overview — stat panels
      # ================================================================
      {
        id = 1;
        type = "stat";
        title = "Models loaded";
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
          unit = "short";
          decimals = 0;
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "red";
                value = null;
              }
              {
                color = "yellow";
                value = 1;
              }
              {
                color = "green";
                value = 2;
              }
            ];
          };
        };
        targets = [
          {
            expr = "sum(llama_server_model_count{host=~\"$host\", value=\"loaded\"})";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Models unloaded";
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
          colorMode = "value";
          graphMode = "area";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 0;
        };
        targets = [
          {
            expr = "sum(llama_server_model_count{host=~\"$host\", value=\"unloaded\"})";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 3;
        type = "stat";
        title = "Total models registered";
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
          graphMode = "none";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 0;
        };
        targets = [
          {
            expr = "count(llama_server_model_info{host=~\"$host\"})";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 4;
        type = "stat";
        title = "Last scrape";
        description = "Age of the most recent /v1/models fetch.";
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
          graphMode = "none";
          textMode = "auto";
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
                value = 30;
              }
              {
                color = "red";
                value = 60;
              }
            ];
          };
        };
        targets = [
          {
            expr = "time() - max(llama_server_model_scrape_timestamp_seconds{host=~\"$host\"})";
            refId = "A";
            instant = true;
          }
        ];
      }

      # ================================================================
      # Row 6: Model roster — table panel
      # ================================================================
      {
        id = 10;
        type = "table";
        title = "Model roster";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 14;
          w = 24;
          x = 0;
          y = 6;
        };
        options = {
          showHeader = true;
          footer = {
            show = false;
            reduceBy = "lastNotNull";
            fields = "";
          };
          sortBy = [
            {
              displayName = "Model";
              desc = false;
            }
          ];
        };
        transformations = [
          {
            id = "merge";
          }
          {
            id = "organize";
            options = {
              excludeByName = {
                Time = true;
                __name__ = true;
                host = true;
                job = true;
              };
              renameByName = {
                model = "Model";
                owned_by = "Owner";
                status = "Status";
                ctx_size = "Context";
                n_params = "Params (B)";
                n_embd = "Embed dim";
                n_vocab = "Vocab";
                n_gpu_layers = "GPU layers";
                alias = "Alias";
                modality = "Modality";
              };
            };
          }
        ];
        fieldConfig.defaults = {
          custom = {
            cellOptions = {
              type = "auto";
            };
            inspect = false;
          };
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
            ];
          };
        };
        fieldConfig.overrides = [
          {
            matcher = {
              id = "byName";
              options = "Status";
            };
            properties = [
              {
                id = "mappings";
                value = [
                  {
                    options = {
                      "1" = {
                        color = "green";
                        index = 0;
                        text = "loaded";
                      };
                      "0" = {
                        color = "red";
                        index = 1;
                        text = "unloaded";
                      };
                    };
                    type = "value";
                  }
                ];
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "Params (B)";
            };
            properties = [
              {
                id = "unit";
                value = "short";
              }
              {
                id = "custom.displayMode";
                value = "basic";
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "Context";
            };
            properties = [
              {
                id = "unit";
                value = "short";
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "GPU layers";
            };
            properties = [
              {
                id = "mappings";
                value = [
                  {
                    options = {
                      "-1" = {
                        color = "blue";
                        index = 0;
                        text = "all";
                      };
                    };
                    type = "value";
                  }
                ];
              }
            ];
          }
        ];
        targets = [
          # Core model status
          {
            expr = "llama_server_model_status{host=~\"$host\", model=~\"$model\"}";
            refId = "status";
            format = "table";
            instant = true;
          }
          # Model info (owned_by)
          {
            expr = "llama_server_model_info{host=~\"$host\", model=~\"$model\"}";
            refId = "info";
            format = "table";
            instant = true;
          }
          # Context size
          {
            expr = "llama_server_model_ctx_size{host=~\"$host\", model=~\"$model\"}";
            refId = "ctx";
            format = "table";
            instant = true;
          }
          # Parameter count
          {
            expr = "llama_server_model_n_params{host=~\"$host\", model=~\"$model\"}";
            refId = "params";
            format = "table";
            instant = true;
          }
          # Embedding dimension
          {
            expr = "llama_server_model_n_embd{host=~\"$host\", model=~\"$model\"}";
            refId = "embd";
            format = "table";
            instant = true;
          }
          # Vocabulary size
          {
            expr = "llama_server_model_n_vocab{host=~\"$host\", model=~\"$model\"}";
            refId = "vocab";
            format = "table";
            instant = true;
          }
          # GPU layers
          {
            expr = "llama_server_model_n_gpu_layers{host=~\"$host\", model=~\"$model\"}";
            refId = "gpu";
            format = "table";
            instant = true;
          }
          # Aliases (multi-valued — each alias becomes a row)
          {
            expr = "llama_server_model_alias{host=~\"$host\", model=~\"$model\"}";
            refId = "alias";
            format = "table";
            instant = true;
          }
          # Input modalities
          {
            expr = "llama_server_model_input_modality{host=~\"$host\", model=~\"$model\"}";
            refId = "input_mod";
            format = "table";
            instant = true;
          }
          # Output modalities
          {
            expr = "llama_server_model_output_modality{host=~\"$host\", model=~\"$model\"}";
            refId = "output_mod";
            format = "table";
            instant = true;
          }
        ];
      }

      # ================================================================
      # Row 20: Load state over time
      # ================================================================
      {
        id = 20;
        type = "timeseries";
        title = "Models loaded over time";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 20;
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          custom = {
            drawStyle = "line";
            lineInterpolation = "stepAfter";
            fillOpacity = 20;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [ "lastNotNull" ];
        };
        targets = [
          {
            expr = "sum by (host) (llama_server_model_count{host=~\"$host\", value=\"loaded\"})";
            legendFormat = "{{host}} loaded";
            refId = "A";
          }
          {
            expr = "sum by (host) (llama_server_model_count{host=~\"$host\", value=\"unloaded\"})";
            legendFormat = "{{host}} unloaded";
            refId = "B";
          }
        ];
      }
      {
        id = 21;
        type = "timeseries";
        title = "Per-model load state";
        description = "1 = loaded, 0 = unloaded (step lines).";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 20;
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          max = 1;
          custom = {
            drawStyle = "line";
            lineInterpolation = "stepAfter";
            fillOpacity = 5;
          };
        };
        options.legend = {
          displayMode = "list";
          placement = "right";
        };
        targets = [
          {
            expr = "llama_server_model_status{host=~\"$host\", model=~\"$model\"}";
            legendFormat = "{{model}}";
            refId = "A";
          }
        ];
      }

      # ================================================================
      # Row 28: Model capacity
      # ================================================================
      {
        id = 30;
        type = "bargauge";
        title = "Parameter count (B) by model";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 28;
        };
        options = {
          displayMode = "gradient";
          showUnfilled = true;
          orientation = "horizontal";
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          color = {
            mode = "continuous-GrYlRd";
          };
        };
        targets = [
          {
            expr = "orderDesc(llama_server_model_n_params{host=~\"$host\", model=~\"$model\"})";
            legendFormat = "{{model}}";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 31;
        type = "bargauge";
        title = "Context size (tokens) by model";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 38;
        };
        options = {
          displayMode = "gradient";
          showUnfilled = true;
          orientation = "horizontal";
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          color = {
            mode = "continuous-BlYlRd";
          };
        };
        targets = [
          {
            expr = "orderDesc(llama_server_model_ctx_size{host=~\"$host\", model=~\"$model\"})";
            legendFormat = "{{model}}";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 32;
        type = "stat";
        title = "GPU layers allocation";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 38;
        };
        options = {
          reduceOptions = {
            calcs = [ "lastNotNull" ];
            fields = "";
            values = false;
          };
          colorMode = "value";
          graphMode = "none";
          textMode = "auto";
          orientation = "auto";
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 0;
        };
        targets = [
          {
            expr = "llama_server_model_n_gpu_layers{host=~\"$host\", model=~\"$model\"}";
            legendFormat = "{{model}}";
            refId = "A";
            instant = true;
          }
        ];
      }

      # ================================================================
      # Row 46: Timing metrics (from llama-server-timing-exporter)
      # ================================================================
      #
      # These panels visualise the metrics written by
      # ``llama-server-timing-exporter.py`` which tails the systemd journal
      # and parses ``slot print_timing`` lines from the llama-cpp service.
      #
      # Metrics:
      #   llama_server_tg_tokens_per_second          – rolling live gauge
      #   llama_server_task_tg_tokens_per_second     – per-task TG rate
      #   llama_server_task_prompt_eval_tokens_per_second – per-task prefill
      #   llama_server_task_total_time_ms            – per-task wall time
      #   llama_server_task_n_decoded_tokens         – decoded tokens/task
      #   llama_server_task_n_prompt_tokens          – prompt tokens/task
      #   llama_server_timing_journal_lines_total    – exporter health counter

      # --- Row header --------------------------------------------------
      {
        id = 50;
        type = "row";
        title = "Timing (journal exporter)";
        collapsed = false;
        gridPos = {
          h = 1;
          w = 24;
          x = 0;
          y = 46;
        };
        panels = [ ];
      }

      # --- Live TG rate stat -------------------------------------------
      {
        id = 51;
        type = "stat";
        title = "Current TG rate";
        description = "Most recent rolling token-generation rate from the periodic print_timing log line.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
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
          textMode = "auto";
          orientation = "auto";
        };
        fieldConfig.defaults = {
          unit = "reqps";
          decimals = 1;
          displayName = "t/s";
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "red";
                value = null;
              }
              {
                color = "yellow";
                value = 5;
              }
              {
                color = "green";
                value = 20;
              }
            ];
          };
        };
        targets = [
          {
            expr = "llama_server_tg_tokens_per_second{host=~\"$host\"}";
            legendFormat = "{{host}}";
            refId = "A";
            instant = true;
          }
        ];
      }

      # --- Timing exporter health stat ---------------------------------
      {
        id = 52;
        type = "stat";
        title = "Journal lines processed";
        description = "Total journal lines processed by the timing exporter since last restart.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
          x = 6;
          y = 47;
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
          orientation = "auto";
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 0;
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "blue";
                value = null;
              }
            ];
          };
        };
        targets = [
          {
            expr = "llama_server_timing_journal_lines_total{host=~\"$host\"}";
            legendFormat = "{{host}}";
            refId = "A";
            instant = true;
          }
        ];
      }

      # --- Live TG rate over time -------------------------------------
      {
        id = 53;
        type = "timeseries";
        title = "Live TG rate over time";
        description = "Rolling token-generation rate (t/s) from the most recent periodic print_timing line.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 47;
        };
        fieldConfig.defaults = {
          unit = "reqps";
          min = 0;
          displayName = "t/s";
          custom = {
            drawStyle = "line";
            lineInterpolation = "linear";
            fillOpacity = 10;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = "llama_server_tg_tokens_per_second{host=~\"$host\"}";
            legendFormat = "{{host}}";
            refId = "A";
          }
        ];
      }

      # --- Per-task TG rate over time ----------------------------------
      {
        id = 54;
        type = "timeseries";
        title = "Per-task TG rate";
        description = "Token-generation rate (t/s) at the end of each completed task (from the final print_timing summary).";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 55;
        };
        fieldConfig.defaults = {
          unit = "reqps";
          min = 0;
          displayName = "t/s";
          custom = {
            drawStyle = "points";
            pointSize = 4;
            fillOpacity = 0;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = "llama_server_task_tg_tokens_per_second{host=~\"$host\"}";
            legendFormat = "{{host}} task {{task}}";
            refId = "A";
          }
        ];
      }

      # --- Per-task prompt-eval rate -----------------------------------
      {
        id = 55;
        type = "timeseries";
        title = "Per-task prefill (prompt eval) rate";
        description = "Prompt-evaluation rate (t/s) for each completed task.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 55;
        };
        fieldConfig.defaults = {
          unit = "reqps";
          min = 0;
          displayName = "t/s";
          custom = {
            drawStyle = "points";
            pointSize = 4;
            fillOpacity = 0;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = "llama_server_task_prompt_eval_tokens_per_second{host=~\"$host\"}";
            legendFormat = "{{host}} task {{task}}";
            refId = "A";
          }
        ];
      }

      # --- Per-task total wall time ------------------------------------
      {
        id = 56;
        type = "timeseries";
        title = "Per-task total time";
        description = "Total wall time (ms) for each completed task (prompt + eval combined).";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 0;
          y = 63;
        };
        fieldConfig.defaults = {
          unit = "ms";
          min = 0;
          custom = {
            drawStyle = "points";
            pointSize = 4;
            fillOpacity = 0;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = "llama_server_task_total_time_ms{host=~\"$host\"}";
            legendFormat = "{{host}} task {{task}}";
            refId = "A";
          }
        ];
      }

      # --- Per-task token counts ----------------------------------------
      {
        id = 57;
        type = "timeseries";
        title = "Per-task token counts";
        description = "Number of prompt tokens processed and tokens generated per completed task.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 8;
          w = 12;
          x = 12;
          y = 63;
        };
        fieldConfig.defaults = {
          unit = "short";
          min = 0;
          custom = {
            drawStyle = "points";
            pointSize = 4;
            fillOpacity = 0;
          };
        };
        options.legend = {
          displayMode = "table";
          placement = "bottom";
          calcs = [
            "lastNotNull"
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = "llama_server_task_n_prompt_tokens{host=~\"$host\"}";
            legendFormat = "{{host}} prompt tokens (task {{task}})";
            refId = "A";
          }
          {
            expr = "llama_server_task_n_decoded_tokens{host=~\"$host\"}";
            legendFormat = "{{host}} decoded tokens (task {{task}})";
            refId = "B";
          }
        ];
      }

      # ================================================================
      # Row 71: Scrape health
      # ================================================================
      {
        id = 40;
        type = "stat";
        title = "Exporter up";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
          x = 0;
          y = 71;
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
          mappings = [
            {
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
              type = "value";
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
        targets = [
          {
            expr = "up{host=~\"$host\", job=\"llama-server\"}";
            legendFormat = "{{host}} / {{instance}}";
            refId = "A";
            instant = true;
          }
        ];
      }
      {
        id = 41;
        type = "stat";
        title = "API scrape success";
        description = "1 if the most recent /v1/models fetch from llama-server succeeded.";
        datasource = "VictoriaMetrics";
        gridPos = {
          h = 5;
          w = 6;
          x = 6;
          y = 71;
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
          mappings = [
            {
              options = {
                "0" = {
                  color = "red";
                  text = "FAIL";
                };
                "1" = {
                  color = "green";
                  text = "OK";
                };
              };
              type = "value";
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
        targets = [
          {
            expr = "llama_server_scrape_success{host=~\"$host\"}";
            legendFormat = "{{host}}";
            refId = "A";
            instant = true;
          }
        ];
      }
    ];
  };

  llamaDashboardFile = pkgs.writeText "llama-server-dashboard.json" (builtins.toJSON llamaDashboard);
in
{
  options.myconfig.observability.host.llamaServer = with lib; {
    provisionDashboard = mkEnableOption {
      text = ''
        Provision the "Llama-server models" Grafana dashboard, which
        visualises model state, metadata and scrape-health from the
        ``llama_server_*`` Prometheus gauges produced by
        ``client.llama-server.nix``. Auto-enabled when the
        observability host is active.
      '';
      default = false;
    };
  };

  config = lib.mkMerge [
    # Auto-provision when the observability host is active
    (lib.mkIf hostCfg.enable {
      myconfig.observability.host.llamaServer.provisionDashboard = lib.mkDefault true;
    })
    # Dashboard provisioning
    (lib.mkIf llamaCfg.provisionDashboard {
      services.grafana.provision.dashboards.settings = {
        apiVersion = 1;
        providers = [
          {
            name = "myconfig-llama-server";
            type = "file";
            disableDeletion = true;
            updateIntervalSeconds = 60;
            options.path = pkgs.runCommand "llama-server-dashboards" { } ''
              mkdir -p $out
              cp ${llamaDashboardFile} $out/llama-server.json
            '';
          }
        ];
      };
    })
  ];
}
