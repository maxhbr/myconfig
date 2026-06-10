# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "Scrape health": surfaces problems with data
# collection — failed Prometheus scrapes, vmagent remote_write errors,
# Loki ingestion lag, and blackbox probe failures.
#
# This is a meta-dashboard about the observability pipeline itself: if
# a panel here goes red it means *the monitoring is broken*, not the
# monitored thing. Use it to triage why other dashboards have gaps.
#
# Data sources:
#
#   * `up{job=...}`                       — standard Prometheus scrape
#                                            success indicator emitted by
#                                            vmagent for every target.
#   * `scrape_duration_seconds`           — per-target scrape latency.
#   * `scrape_samples_scraped`            — sample volume per scrape;
#                                            sudden drops to 0 indicate
#                                            an exporter regression.
#   * `vm_promscrape_scrapes_failed_total`— vmagent's per-job counter of
#                                            failed scrape attempts
#                                            (exposed on the vmagent
#                                            self-scrape endpoint).
#   * `vm_promscrape_scrapes_total`       — total scrape attempts (used
#                                            as denominator for the
#                                            failure ratio).
#   * `vmagent_remotewrite_*`             — remote_write health: errors,
#                                            retries, pending blocks,
#                                            dropped packets.
#   * `probe_success`                     — blackbox probe outcome
#                                            (failed external/HTTP
#                                            checks).
#   * `loki:host_logs_recent_count`       — recording rule emitted by
#                                            Loki's ruler; stale values
#                                            indicate log-shipping
#                                            outages from a host.
#
# All panels filter by a `host` template variable so problems can be
# localised to a single client quickly.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  scrapeCfg = hostCfg.scrapeHealth;

  # ---------------------------------------------------------------------
  # Overview row — at-a-glance counts of currently broken things
  # ---------------------------------------------------------------------
  overviewPanels = [
    {
      id = 100;
      type = "stat";
      title = "Targets down";
      description = ''
        Number of Prometheus scrape targets currently reporting
        `up == 0`. Each entry corresponds to one (job, instance)
        tuple that vmagent has failed to scrape on its last
        attempt.
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
        unit = "short";
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
              value = 1;
            }
            {
              color = "red";
              value = 3;
            }
          ];
        };
      };
      targets = [
        {
          expr = ''sum(up{host=~"$host"} == 0)'';
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 101;
      type = "stat";
      title = "Scrape failure ratio (5m)";
      description = ''
        Fraction of scrape attempts that failed across all jobs in
        the last 5 minutes, computed from vmagent's
        `vm_promscrape_scrapes_failed_total` /
        `vm_promscrape_scrapes_total`. Sustained values > 0 mean
        scrape configs are broken or targets are unreachable.
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
        colorMode = "background";
        graphMode = "area";
        textMode = "auto";
        orientation = "auto";
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
              color = "orange";
              value = 0.1;
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
          expr = ''
            sum(rate(vm_promscrape_scrapes_failed_total{host=~"$host"}[5m]))
              /
            clamp_min(sum(rate(vm_promscrape_scrapes_total{host=~"$host"}[5m])), 1)
          '';
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 102;
      type = "stat";
      title = "Remote_write errors (5m)";
      description = ''
        Rate of failed remote_write requests from vmagent to
        VictoriaMetrics
        (`vmagent_remotewrite_requests_total{status_code!~"2.."}`).
        A non-zero value means metrics are being dropped or
        retried before reaching the central TSDB.
      '';
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
        colorMode = "background";
        graphMode = "area";
        textMode = "auto";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "reqps";
        decimals = 3;
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
          # vmagent exports two variants of this metric across versions;
          # `or` with vector(0) keeps the panel green when neither
          # exists. status_code labels are strings, hence the regex.
          expr = ''
            sum(rate(vmagent_remotewrite_requests_total{host=~"$host", status_code!~"2.."}[5m]))
              or
            sum(rate(vm_promscrape_remotewrite_requests_total{host=~"$host", status_code!~"2.."}[5m]))
              or
            vector(0)
          '';
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 103;
      type = "stat";
      title = "Stale log streams";
      description = ''
        Number of hosts whose most recent journal log line was
        ingested by Loki more than 5 minutes ago (derived from the
        `loki:host_logs_recent_count` recording rule). A
        non-zero value means Alloy is down, the network to Loki
        is broken, or the host has stopped emitting logs entirely.
      '';
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
        colorMode = "background";
        graphMode = "none";
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
              color = "green";
              value = null;
            }
            {
              color = "yellow";
              value = 1;
            }
            {
              color = "red";
              value = 2;
            }
          ];
        };
      };
      targets = [
        {
          expr = ''
            count(
              (time() - timestamp(last_over_time(loki:host_logs_recent_count{host=~"$host"}[24h:1m]))) > 300
            )
              or
            vector(0)
          '';
          refId = "A";
          instant = true;
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Scrape row — per-target `up` status and failure rates
  # ---------------------------------------------------------------------
  scrapePanels = [
    {
      id = 200;
      type = "stat";
      title = "Target status (1 = up)";
      description = ''
        Per-target `up` metric. Each tile is one (host, job,
        instance) tuple. Red tiles are targets vmagent currently
        cannot scrape — check the exporter, the firewall, or the
        scrape config.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 10;
        w = 24;
        x = 0;
        y = 8;
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
      fieldConfig.defaults = {
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
      targets = [
        {
          expr = ''up{host=~"$host"}'';
          legendFormat = "{{host}} / {{job}} / {{instance}}";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 201;
      type = "timeseries";
      title = "Target availability over time";
      description = ''
        `up` plotted as a step function — drops to 0 mark the
        intervals where a target was unreachable.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 18;
      };
      fieldConfig.defaults = {
        unit = "short";
        min = 0;
        max = 1;
        custom = {
          drawStyle = "line";
          lineInterpolation = "stepAfter";
          fillOpacity = 20;
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "mean"
        ];
      };
      targets = [
        {
          expr = ''up{host=~"$host"}'';
          legendFormat = "{{host}} / {{job}} / {{instance}}";
          refId = "A";
        }
      ];
    }
    {
      id = 202;
      type = "timeseries";
      title = "Scrape failures per second (by job)";
      description = ''
        `rate(vm_promscrape_scrapes_failed_total[5m])` — number of
        failed scrape attempts per second, broken down by vmagent
        job. Use this together with the target-status panel above
        to distinguish "exporter is gone" (one instance down)
        from "the whole job is mis-configured" (every instance
        down).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 18;
      };
      fieldConfig.defaults = {
        unit = "ops";
        custom = {
          drawStyle = "line";
          fillOpacity = 10;
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
          expr = ''sum by (host, job) (rate(vm_promscrape_scrapes_failed_total{host=~"$host"}[5m]))'';
          legendFormat = "{{host}} / {{job}}";
          refId = "A";
        }
      ];
    }
    {
      id = 203;
      type = "timeseries";
      title = "Scrape duration p95 (by job)";
      description = ''
        95th percentile scrape latency. Scrapes that consistently
        approach the scrape interval (15s default) are at risk of
        timing out and producing gaps.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 26;
      };
      fieldConfig.defaults = {
        unit = "s";
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
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
          # `scrape_duration_seconds` is a gauge written per-scrape by
          # the scraper itself, so a high-quantile over a sliding
          # window is the most useful aggregation.
          expr = ''quantile by (host, job) (0.95, scrape_duration_seconds{host=~"$host"})'';
          legendFormat = "{{host}} / {{job}}";
          refId = "A";
        }
      ];
    }
    {
      id = 204;
      type = "timeseries";
      title = "Samples scraped per second (by job)";
      description = ''
        `rate(scrape_samples_scraped[5m])` — rough volume of
        samples coming out of each scrape job. A sudden cliff to
        0 usually means an exporter regressed (renamed/removed
        metrics, or stopped emitting). Long-term trends here help
        plan retention.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 26;
      };
      fieldConfig.defaults = {
        unit = "short";
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "mean"
        ];
      };
      targets = [
        {
          # `scrape_samples_scraped` is itself a gauge (last scrape's
          # count); summing per (host, job) and taking the average
          # gives a stable line.
          expr = ''avg by (host, job) (scrape_samples_scraped{host=~"$host"})'';
          legendFormat = "{{host}} / {{job}}";
          refId = "A";
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Remote_write row — vmagent → VictoriaMetrics pipeline health
  # ---------------------------------------------------------------------
  remoteWritePanels = [
    {
      id = 300;
      type = "timeseries";
      title = "remote_write request rate (by status class)";
      description = ''
        Outgoing remote_write requests from vmagent, grouped by
        HTTP status class. Anything other than 2xx means
        VictoriaMetrics rejected the batch (4xx) or was
        unreachable (5xx / connection errors counted under their
        own metric, see next panel).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 35;
      };
      fieldConfig.defaults = {
        unit = "reqps";
        custom = {
          drawStyle = "line";
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
          "lastNotNull"
          "mean"
        ];
      };
      targets = [
        {
          expr = ''
            sum by (host, status_code) (rate(vmagent_remotewrite_requests_total{host=~"$host"}[5m]))
              or
            sum by (host, status_code) (rate(vm_promscrape_remotewrite_requests_total{host=~"$host"}[5m]))
          '';
          legendFormat = "{{host}} / {{status_code}}";
          refId = "A";
        }
      ];
    }
    {
      id = 301;
      type = "timeseries";
      title = "Retries & connection errors";
      description = ''
        `vmagent_remotewrite_retries_count_total` and
        `vmagent_remotewrite_conn_errors_total` — symptomatic of
        transient remote_write problems (network blips,
        VictoriaMetrics restarts, basic-auth changes).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 35;
      };
      fieldConfig.defaults = {
        unit = "ops";
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "mean"
        ];
      };
      targets = [
        {
          expr = ''sum by (host) (rate(vmagent_remotewrite_retries_count_total{host=~"$host"}[5m]))'';
          legendFormat = "{{host}} retries";
          refId = "A";
        }
        {
          expr = ''sum by (host) (rate(vmagent_remotewrite_conn_errors_total{host=~"$host"}[5m]))'';
          legendFormat = "{{host}} conn errors";
          refId = "B";
        }
      ];
    }
    {
      id = 302;
      type = "timeseries";
      title = "Pending blocks (queue depth)";
      description = ''
        `vmagent_remotewrite_pending_data_bytes` — how many bytes
        vmagent is holding in its on-disk queue waiting to be
        shipped to VictoriaMetrics. A steadily growing line
        means vmagent is producing data faster than it can be
        accepted, and old samples will eventually be dropped.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 43;
      };
      fieldConfig.defaults = {
        unit = "bytes";
        custom = {
          drawStyle = "line";
          fillOpacity = 10;
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
          expr = ''sum by (host) (vmagent_remotewrite_pending_data_bytes{host=~"$host"})'';
          legendFormat = "{{host}}";
          refId = "A";
        }
      ];
    }
    {
      id = 303;
      type = "timeseries";
      title = "Dropped samples / packets per second";
      description = ''
        `vmagent_remotewrite_packets_dropped_total` (or
        `vm_remotewrite_packets_dropped_total`) — samples vmagent
        permanently discarded after exhausting retries. Any
        non-zero rate here means VictoriaMetrics is missing
        data points from that host.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 43;
      };
      fieldConfig.defaults = {
        unit = "ops";
        custom = {
          drawStyle = "line";
          fillOpacity = 10;
        };
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "green";
              value = null;
            }
            {
              color = "red";
              value = 0.001;
            }
          ];
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
            sum by (host) (rate(vmagent_remotewrite_packets_dropped_total{host=~"$host"}[5m]))
              or
            sum by (host) (rate(vm_remotewrite_packets_dropped_total{host=~"$host"}[5m]))
          '';
          legendFormat = "{{host}}";
          refId = "A";
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Probe / log row — adjacent data-collection failures
  # ---------------------------------------------------------------------
  externalPanels = [
    {
      id = 400;
      type = "timeseries";
      title = "Failing blackbox probes";
      description = ''
        Number of blackbox HTTP probes returning `probe_success
        == 0` over time. Distinct from the per-target `up` panel
        above because blackbox probes check the *user-facing*
        URL via Caddy, while `up` only checks that vmagent could
        reach each scrape target. Both can fail independently.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 52;
      };
      fieldConfig.defaults = {
        unit = "short";
        custom = {
          drawStyle = "line";
          lineInterpolation = "stepAfter";
          fillOpacity = 20;
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "max"
        ];
      };
      targets = [
        {
          # `probed_host` is added by the relabel rules in
          # ./host.uptime.nix; it filters on the host the URL belongs
          # to, not the host running the probe.
          expr = ''sum by (instance) (probe_success{job="blackbox", probed_host=~"$host"} == 0)'';
          legendFormat = "{{instance}}";
          refId = "A";
        }
      ];
    }
    {
      id = 401;
      type = "table";
      title = "Currently failing blackbox probes";
      description = ''
        One row per URL whose latest blackbox probe failed.
        Shows the HTTP status code returned (0 means the probe
        could not connect / timed out) so you can distinguish
        e.g. a 502 from Caddy vs. an outright DNS / connectivity
        failure.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 52;
      };
      options = {
        showHeader = true;
        sortBy = [
          {
            displayName = "Probed URL";
            desc = false;
          }
        ];
      };
      transformations = [
        {
          id = "organize";
          options = {
            excludeByName = {
              Time = true;
              __name__ = true;
              job = true;
            };
            renameByName = {
              instance = "Probed URL";
              probed_host = "Host";
              Value = "HTTP status";
            };
          };
        }
      ];
      targets = [
        {
          # Show the HTTP status code for *failing* probes (those with
          # probe_success == 0). The `and` keeps only the series where
          # both are reported, so the table contains exactly the
          # currently broken URLs.
          expr = ''
            probe_http_status_code{job="blackbox", probed_host=~"$host"}
              and on (instance) (probe_success{job="blackbox"} == 0)
          '';
          refId = "A";
          format = "table";
          instant = true;
        }
      ];
    }
    {
      id = 410;
      type = "timeseries";
      title = "Time since last log received (per host)";
      description = ''
        Seconds since Loki last received a journal log line from
        each host, computed from the `loki:host_logs_recent_count`
        recording rule. Climbing values indicate the client's
        Alloy agent is failing to push logs (process dead, disk
        full, or network path to Loki broken).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 24;
        x = 0;
        y = 60;
      };
      fieldConfig.defaults = {
        unit = "s";
        custom = {
          drawStyle = "line";
          lineInterpolation = "linear";
          fillOpacity = 10;
        };
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "green";
              value = null;
            }
            {
              color = "yellow";
              value = 300;
            }
            {
              color = "orange";
              value = 1800;
            }
            {
              color = "red";
              value = 7200;
            }
          ];
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "max"
        ];
      };
      targets = [
        {
          expr = ''
            time() - timestamp(last_over_time(loki:host_logs_recent_count{host=~"$host"}[24h:1m]))
          '';
          legendFormat = "{{host}}";
          refId = "A";
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # VictoriaMetrics ingest-side row — server-side rejections
  # ---------------------------------------------------------------------
  ingestPanels = [
    {
      id = 500;
      type = "timeseries";
      title = "VictoriaMetrics rows inserted/s";
      description = ''
        `rate(vm_rows_inserted_total[5m])` — total rows accepted
        by the central VictoriaMetrics. A sudden cliff to 0 with
        no corresponding drop in `vmagent_remotewrite_requests`
        means VM stopped accepting writes (out of disk,
        restarting, auth misconfiguration).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 69;
      };
      fieldConfig.defaults = {
        unit = "rowsps";
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
        };
      };
      options.legend = {
        displayMode = "table";
        placement = "right";
        calcs = [
          "lastNotNull"
          "mean"
        ];
      };
      targets = [
        {
          # Aggregate across all `type` labels (vmstorage emits one
          # series per ingestion path).
          expr = "sum(rate(vm_rows_inserted_total[5m]))";
          legendFormat = "rows/s";
          refId = "A";
        }
      ];
    }
    {
      id = 501;
      type = "timeseries";
      title = "VictoriaMetrics ignored / dropped rows /s";
      description = ''
        `vm_rows_ignored_total` (and friends) — rows VM
        explicitly refused, e.g. because of label cardinality
        limits, out-of-order samples, or stale-NaN markers.
        Should normally be flat at 0; a steady non-zero rate
        means producers are sending malformed data.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 69;
      };
      fieldConfig.defaults = {
        unit = "rowsps";
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
        };
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
              value = 1;
            }
          ];
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
          expr = "sum by (reason) (rate(vm_rows_ignored_total[5m]))";
          legendFormat = "{{reason}}";
          refId = "A";
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Rows
  # ---------------------------------------------------------------------
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

  scrapeRow = {
    id = 2;
    type = "row";
    title = "Scrape targets";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 7;
    };
    panels = [ ];
  };

  remoteWriteRow = {
    id = 3;
    type = "row";
    title = "vmagent → VictoriaMetrics (remote_write)";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 34;
    };
    panels = [ ];
  };

  externalRow = {
    id = 4;
    type = "row";
    title = "External probes & log shipping";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 51;
    };
    panels = [ ];
  };

  ingestRow = {
    id = 5;
    type = "row";
    title = "VictoriaMetrics ingest";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 68;
    };
    panels = [ ];
  };

  scrapeHealthDashboard = {
    uid = "myconfig-scrape-health";
    title = "Scrape health";
    tags = [
      "myconfig"
      "observability"
      "scrape-health"
      "meta"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "30s";
    time = {
      from = "now-6h";
      to = "now";
    };
    templating.list = [
      {
        name = "host";
        label = "host";
        type = "query";
        datasource = "VictoriaMetrics";
        # Use `up` to enumerate every host vmagent currently knows about.
        # This works even if a host stopped emitting node_exporter
        # metrics (since the `up` series stays in TSDB and reports 0).
        query = "label_values(up, host)";
        refresh = 2;
        includeAll = true;
        multi = true;
        allValue = ".+";
        sort = 1;
      }
    ];
    annotations.list = [ ];
    panels = [
      overviewRow
    ]
    ++ overviewPanels
    ++ [ scrapeRow ]
    ++ scrapePanels
    ++ [ remoteWriteRow ]
    ++ remoteWritePanels
    ++ [ externalRow ]
    ++ externalPanels
    ++ [ ingestRow ]
    ++ ingestPanels;
  };

  scrapeHealthDashboardFile = pkgs.writeText "scrape-health-dashboard.json" (
    builtins.toJSON scrapeHealthDashboard
  );
in
{
  options.myconfig.observability.host.scrapeHealth = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision the "Scrape health" Grafana dashboard, a
        meta-dashboard about the observability pipeline itself:
        Prometheus scrape failures (`up`,
        `vm_promscrape_scrapes_failed_total`), vmagent
        remote_write health (errors, retries, pending queue,
        dropped samples), blackbox probe failures, log-shipping
        freshness, and VictoriaMetrics ingest rejections.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && scrapeCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-scrape-health";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          # Group meta-dashboards (dashboards about the observability
          # pipeline itself) under a "Meta" folder in the Grafana UI.
          folder = "Meta";
          options.path = pkgs.runCommand "scrape-health-dashboards" { } ''
            mkdir -p $out
            cp ${scrapeHealthDashboardFile} $out/scrape-health.json
          '';
        }
      ];
    };
  };
}
