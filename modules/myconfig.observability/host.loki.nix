# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Loki log aggregation server, runs alongside VictoriaMetrics + Grafana
# on the central observability host. Clients forward logs here via Alloy
# (see ./client.alloy.nix).
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  lokiCfg = hostCfg.loki;
  wgIp = myconfig.metadatalib.getWgIp cfg.host_hostname;

  # ---------------------------------------------------------------------
  # Loki recording rules
  #
  # The ruler evaluates these LogQL expressions on a fixed interval and
  # remote-writes the resulting samples to VictoriaMetrics. The
  # `loki:host_logs_recent_count` metric is emitted *only when a host
  # produced at least one log line in the last evaluation window*, so
  # `time() - timestamp(last_over_time(loki:host_logs_recent_count[24h:]))`
  # in PromQL yields "seconds since the most recent log line was
  # received from this host" — which is what the home-lab-status
  # dashboard surfaces as "Time since last log received".
  # ---------------------------------------------------------------------
  recordingRulesFile = pkgs.writeText "myconfig-loki-recording-rules.yaml" (
    builtins.toJSON {
      groups = [
        {
          name = "myconfig-log-freshness";
          interval = "1m";
          rules = [
            {
              record = "loki:host_logs_recent_count";
              expr = ''sum by (host) (count_over_time({job="systemd-journal"}[1m]))'';
            }
          ];
        }
        # Attack counters derived from the dedicated journal sources that
        # myconfig.networking.hardening adds (log_stream="sshd" / "kernel").
        # Record only per-host totals. The dashboard extracts source IPs and
        # destination ports on demand so scans don't create persistent series.
        {
          name = "myconfig-attacks";
          interval = "1m";
          rules = [
            {
              record = "loki:firewall_refused_connections_count";
              expr = ''sum by (host) (count_over_time({job="systemd-journal", log_stream="kernel"} |= "refused connection:" [1m]))'';
            }
            {
              record = "loki:ssh_auth_failures_count";
              expr = ''sum by (host) (count_over_time({job="systemd-journal", log_stream="sshd"} |~ "Failed (password|publickey)" [1m]))'';
            }
          ];
        }
        # Privileged-exec counters derived from the audit journal source
        # that myconfig.security.audit adds (log_stream="audit"). Audit
        # records arrive through audisp-syslog at LOG_INFO, so they are
        # invisible to the generic priority-filtered source.
        #
        # An exec produces several audit records sharing one audit ID: a
        # SYSCALL record (carrying `exe=` and `key=`) and a separate EXECVE
        # record (carrying argv). They cannot be filtered on the same line,
        # so these rules match the SYSCALL record only; the dashboard reads
        # argv out of EXECVE records on demand.
        #
        # The rule key separates login-attributed root execs (`root_exec`,
        # auid>=1000) from root's own logins (`root_login_exec`, auid=0 —
        # e.g. the root@host path switch.sh deploys over).
        {
          name = "myconfig-root-activity";
          interval = "1m";
          rules = [
            {
              record = "loki:root_exec_count";
              expr = ''sum by (host) (count_over_time({job="systemd-journal", log_stream="audit"} |~ `type=SYSCALL.*key="root_exec"` [1m]))'';
            }
            {
              record = "loki:root_login_exec_count";
              expr = ''sum by (host) (count_over_time({job="systemd-journal", log_stream="audit"} |~ `type=SYSCALL.*key="root_login_exec"` [1m]))'';
            }
            {
              record = "loki:sudo_invocation_count";
              expr = ''sum by (host) (count_over_time({job="systemd-journal", log_stream="audit"} |~ `type=SYSCALL.*exe="/run/wrappers/bin/sudo"` [1m]))'';
            }
          ];
        }
      ];
    }
  );

  # Loki's ruler with `storage.type=local` expects the directory layout
  # `<dir>/<tenant id>/<file>.yaml`. In single-tenant mode the tenant
  # id is `fake`.
  recordingRulesDir = pkgs.runCommand "myconfig-loki-rules" { } ''
    mkdir -p $out/fake
    cp ${recordingRulesFile} $out/fake/myconfig.yaml
  '';

  # Levels considered "errors" for the error-rate panels. Alloy maps
  # the systemd `PRIORITY` field to a `level` label using the
  # `__journal_priority_keyword` source. Different versions emit the
  # keyword either lowercase (`emerg`, `alert`, `crit`, `err`) following
  # go-systemd's `Priority.String()`, or with the `error` spelling that
  # the Alloy reference docs show. Some setups also pass the raw
  # systemd level (uppercase, e.g. `ERR`, `EMERG`). LogQL stream-
  # selector regexes are fully anchored, so we list every plausible
  # spelling explicitly and use the case-insensitive RE2 flag `(?i)`.
  errorLevelRegex = "(?i)(emerg|alert|crit|err|error)";

  # Grafana dashboard for an overview of logs ingested into Loki:
  # total log rate, error rate, ratio, and breakdowns by host / level /
  # unit, plus a live tail of recent error logs.
  logsDashboard = {
    uid = "myconfig-logs";
    title = "Logs overview";
    tags = [
      "myconfig"
      "logs"
      "loki"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "30s";
    time = {
      from = "now-1h";
      to = "now";
    };
    annotations.list = [ ];
    templating.list = [
      {
        name = "host";
        label = "Host";
        type = "query";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        # Loki variable query: type=1 means "label values", and `label`
        # is the label whose values to enumerate. `stream` narrows the
        # search to a specific log stream selector.
        query = {
          label = "host";
          refId = "LokiVariableQueryEditor-VariableQuery";
          stream = ''{job="systemd-journal"}'';
          type = 1;
        };
        # `definition` is what older provisioning paths read; we mirror
        # the structured query into it as a fallback.
        definition = ''label_values({job="systemd-journal"}, host)'';
        refresh = 2;
        sort = 1;
        multi = true;
        includeAll = true;
        allValue = ".+";
        current = {
          selected = false;
          text = "All";
          value = "$__all";
        };
        options = [ ];
      }
    ];
    panels = [
      {
        id = 1;
        type = "stat";
        title = "Log rate (lines/s)";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 4;
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
          unit = "logs/s";
          decimals = 1;
        };
        targets = [
          {
            expr = ''sum(rate({job="systemd-journal", host=~"$host"}[1m]))'';
            refId = "A";
          }
        ];
      }
      {
        id = 2;
        type = "stat";
        title = "Error rate (lines/s)";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 4;
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
          unit = "logs/s";
          decimals = 2;
          thresholds = {
            mode = "absolute";
            steps = [
              {
                color = "green";
                value = null;
              }
              {
                color = "yellow";
                value = 0.1;
              }
              {
                color = "red";
                value = 1;
              }
            ];
          };
        };
        targets = [
          {
            expr = ''sum(rate({job="systemd-journal", host=~"$host", level=~"${errorLevelRegex}"}[1m]))'';
            refId = "A";
          }
        ];
      }
      {
        id = 3;
        type = "stat";
        title = "Error ratio";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 4;
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
        fieldConfig.defaults = {
          unit = "percentunit";
          decimals = 2;
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
            expr = ''
              sum(rate({job="systemd-journal", host=~"$host", level=~"${errorLevelRegex}"}[5m]))
              /
              sum(rate({job="systemd-journal", host=~"$host"}[5m]))
            '';
            refId = "A";
          }
        ];
      }
      {
        id = 4;
        type = "stat";
        title = "Reporting hosts";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 4;
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
          graphMode = "none";
          textMode = "auto";
        };
        fieldConfig.defaults = {
          unit = "short";
          decimals = 0;
        };
        targets = [
          {
            expr = ''count(sum by (host) (rate({job="systemd-journal", host=~"$host"}[5m])))'';
            refId = "A";
          }
        ];
      }
      {
        id = 10;
        type = "timeseries";
        title = "Log rate by host";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 9;
          w = 12;
          x = 0;
          y = 4;
        };
        fieldConfig.defaults = {
          unit = "logs/s";
          custom = {
            drawStyle = "line";
            lineInterpolation = "linear";
            fillOpacity = 10;
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
            "mean"
            "max"
          ];
        };
        targets = [
          {
            expr = ''sum by (host) (rate({job="systemd-journal", host=~"$host"}[1m]))'';
            legendFormat = "{{host}}";
            refId = "A";
          }
        ];
      }
      {
        id = 11;
        type = "timeseries";
        title = "Log rate by level";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 9;
          w = 12;
          x = 12;
          y = 4;
        };
        fieldConfig.defaults = {
          unit = "logs/s";
          custom = {
            drawStyle = "line";
            lineInterpolation = "linear";
            fillOpacity = 10;
            stacking = {
              mode = "normal";
              group = "A";
            };
          };
        };
        fieldConfig.overrides = [
          {
            matcher = {
              id = "byName";
              options = "emerg";
            };
            properties = [
              {
                id = "color";
                value = {
                  mode = "fixed";
                  fixedColor = "dark-red";
                };
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "alert";
            };
            properties = [
              {
                id = "color";
                value = {
                  mode = "fixed";
                  fixedColor = "red";
                };
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "crit";
            };
            properties = [
              {
                id = "color";
                value = {
                  mode = "fixed";
                  fixedColor = "red";
                };
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "err";
            };
            properties = [
              {
                id = "color";
                value = {
                  mode = "fixed";
                  fixedColor = "orange";
                };
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "warning";
            };
            properties = [
              {
                id = "color";
                value = {
                  mode = "fixed";
                  fixedColor = "yellow";
                };
              }
            ];
          }
          {
            matcher = {
              id = "byName";
              options = "info";
            };
            properties = [
              {
                id = "color";
                value = {
                  mode = "fixed";
                  fixedColor = "blue";
                };
              }
            ];
          }
        ];
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
            expr = ''sum by (level) (rate({job="systemd-journal", host=~"$host"}[1m]))'';
            legendFormat = "{{level}}";
            refId = "A";
          }
        ];
      }
      {
        id = 20;
        type = "timeseries";
        title = "Error rate by host";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 9;
          w = 12;
          x = 0;
          y = 13;
        };
        fieldConfig.defaults = {
          unit = "logs/s";
          custom = {
            drawStyle = "line";
            lineInterpolation = "linear";
            fillOpacity = 10;
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
            expr = ''sum by (host) (rate({job="systemd-journal", host=~"$host", level=~"${errorLevelRegex}"}[1m]))'';
            legendFormat = "{{host}}";
            refId = "A";
          }
        ];
      }
      {
        id = 21;
        type = "table";
        title = "Top noisy units (last 1h)";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 9;
          w = 12;
          x = 12;
          y = 13;
        };
        options = {
          showHeader = true;
          sortBy = [
            {
              displayName = "Value";
              desc = true;
            }
          ];
        };
        fieldConfig.defaults = {
          custom = {
            align = "left";
          };
        };
        targets = [
          {
            expr = ''
              topk(20, sum by (host, unit) (
                count_over_time({job="systemd-journal", host=~"$host", unit!=""}[1h])
              ))
            '';
            instant = true;
            refId = "A";
            format = "table";
          }
        ];
        transformations = [
          {
            id = "organize";
            options = {
              excludeByName = {
                Time = true;
              };
              indexByName = { };
              renameByName = {
                Value = "Lines (1h)";
                host = "Host";
                unit = "Unit";
              };
            };
          }
        ];
      }
      {
        id = 30;
        type = "table";
        title = "Top error-producing units (last 1h)";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 9;
          w = 12;
          x = 0;
          y = 22;
        };
        options = {
          showHeader = true;
          sortBy = [
            {
              displayName = "Errors (1h)";
              desc = true;
            }
          ];
        };
        targets = [
          {
            expr = ''
              topk(20, sum by (host, unit) (
                count_over_time({job="systemd-journal", host=~"$host", unit!="", level=~"${errorLevelRegex}"}[1h])
              ))
            '';
            instant = true;
            refId = "A";
            format = "table";
          }
        ];
        transformations = [
          {
            id = "organize";
            options = {
              excludeByName = {
                Time = true;
              };
              indexByName = { };
              renameByName = {
                Value = "Errors (1h)";
                host = "Host";
                unit = "Unit";
              };
            };
          }
        ];
      }
      {
        id = 31;
        type = "logs";
        title = "Recent errors";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 9;
          w = 12;
          x = 12;
          y = 22;
        };
        options = {
          showTime = true;
          showLabels = false;
          showCommonLabels = false;
          wrapLogMessage = true;
          prettifyLogMessage = false;
          enableLogDetails = true;
          dedupStrategy = "none";
          sortOrder = "Descending";
        };
        targets = [
          {
            expr = ''{job="systemd-journal", host=~"$host", level=~"${errorLevelRegex}"}'';
            refId = "A";
            maxLines = 500;
          }
        ];
      }
      {
        id = 40;
        type = "logs";
        title = "Recent logs (all levels)";
        datasource = {
          type = "loki";
          uid = "loki";
        };
        gridPos = {
          h = 10;
          w = 24;
          x = 0;
          y = 31;
        };
        options = {
          showTime = true;
          showLabels = false;
          showCommonLabels = false;
          wrapLogMessage = true;
          prettifyLogMessage = false;
          enableLogDetails = true;
          dedupStrategy = "none";
          sortOrder = "Descending";
        };
        targets = [
          {
            expr = ''{job="systemd-journal", host=~"$host"}'';
            refId = "A";
            maxLines = 500;
          }
        ];
      }
    ];
  };

  logsDashboardFile = pkgs.writeText "logs-dashboard.json" (builtins.toJSON logsDashboard);
in
{
  options.myconfig.observability.host.loki = with lib; {
    retentionPeriod = mkOption {
      type = types.str;
      default = "720h"; # 30 days
      description = "Loki log retention period (compactor delay until logs are deleted).";
    };

    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard ("Logs overview") with total log
        and error rates plus breakdowns by host, level and systemd
        unit, sourced from the Loki datasource.
      '';
    };
  };

  config = lib.mkIf hostCfg.enable {
    services.loki = {
      enable = true;

      configuration = {
        auth_enabled = false;

        server = {
          http_listen_address = wgIp;
          http_listen_port = cfg.lokiPort;
          grpc_listen_port = 9096;
        };

        common = {
          path_prefix = "/var/lib/loki";
          replication_factor = 1;
          ring = {
            instance_addr = "127.0.0.1";
            kvstore.store = "inmemory";
          };
        };

        ingester = {
          chunk_idle_period = "1h";
          max_chunk_age = "1h";
          chunk_target_size = 1048576;
          chunk_retain_period = "30s";
        };

        schema_config.configs = [
          {
            from = "2024-01-01";
            store = "tsdb";
            object_store = "filesystem";
            schema = "v13";
            index = {
              prefix = "index_";
              period = "24h";
            };
          }
        ];

        storage_config = {
          tsdb_shipper = {
            active_index_directory = "/var/lib/loki/tsdb-index";
            cache_location = "/var/lib/loki/tsdb-cache";
          };
          filesystem.directory = "/var/lib/loki/chunks";
        };

        compactor = {
          working_directory = "/var/lib/loki/compactor";
          compaction_interval = "10m";
          retention_enabled = true;
          retention_delete_delay = "2h";
          retention_delete_worker_count = 150;
          delete_request_store = "filesystem";
        };

        limits_config = {
          reject_old_samples = true;
          reject_old_samples_max_age = "168h";
          retention_period = hostCfg.loki.retentionPeriod;
          allow_structured_metadata = true;
          # The security dashboards extract one series per probed port / source
          # IP at query time (topk over sum by (dpt|src)). A full TCP port scan
          # alone can exceed the default of 500 and abort the query.
          max_query_series = 66000;
        };

        # Recording rules are evaluated by the embedded ruler and the
        # resulting samples are remote-written into VictoriaMetrics
        # (which exposes the Prometheus remote_write receiver at
        # /api/v1/write on its listen port). See
        # `recordingRulesDir` above for the rule definitions.
        ruler = {
          storage = {
            type = "local";
            local.directory = "${recordingRulesDir}";
          };
          rule_path = "/var/lib/loki/ruler";
          ring.kvstore.store = "inmemory";
          enable_api = true;
          # Evaluate rules slightly faster than the 1m group interval
          # so the freshness metric stays current.
          evaluation_interval = "30s";
          remote_write = {
            enabled = true;
            client = {
              url = "http://${wgIp}:${toString cfg.remoteWritePort}/api/v1/write";
              basic_auth = {
                username = cfg.basicAuthUsername;
                password_file = toString cfg.basicAuthPasswordFile;
              };
            };
          };
        };

        analytics.reporting_enabled = false;
      };
    };

    # The firewall opening for cfg.lokiPort is done centrally in host.nix
    # alongside the VictoriaMetrics + Grafana ports.

    services.grafana.provision.dashboards.settings = lib.mkIf lokiCfg.provisionDashboard {
      # `apiVersion` is also set by ./host.uptime.nix; mark this one as
      # default so the two modules coexist when both are enabled.
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-logs";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "logs-dashboards" { } ''
            mkdir -p $out
            cp ${logsDashboardFile} $out/logs.json
          '';
        }
        # The root-activity dashboard reads this host's recorded series, so
        # it is provisioned here rather than by myconfig.security.audit:
        # the audit producer runs on the audited host, which is not the
        # host running Grafana. Gating it on the producer would leave the
        # dashboard missing on the one machine that can display it.
        {
          name = "myconfig-security-root-activity";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          folder = "Security";
          options.path = pkgs.runCommand "security-root-activity-dashboards" { } ''
            mkdir -p $out
            cp ${./dashboards/security-root-activity.json} $out/security-root-activity.json
          '';
        }
      ];
    };
  };
}
