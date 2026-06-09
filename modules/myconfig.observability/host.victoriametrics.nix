# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard "VictoriaMetrics storage": tracks the on-disk
# footprint of the central TSDB (`vm_data_size_bytes`), the free disk
# space remaining on the data path (`vm_free_disk_space_bytes`),
# active time series, row counts and ingest throughput.
#
# The companion node_exporter `node_filesystem_*` series for the
# filesystem hosting VictoriaMetrics' StateDirectory
# (`/var/lib/victoriametrics`, which on host.nuc lives on `/`) are
# also surfaced so the dashboard answers two related questions in one
# place:
#
#   1. How much space is VictoriaMetrics itself using? (vm_data_size_bytes)
#   2. How much space is left on the filesystem hosting it?
#      (node_filesystem_avail_bytes / vm_free_disk_space_bytes)
#
# Data sources (all on the VictoriaMetrics self-scrape endpoint,
# scraped by vmagent via the job added in client.nix):
#
#   * `vm_data_size_bytes{type=...}`     — bytes on disk per
#                                            component (storage,
#                                            indexdb, metaindex, …).
#                                            Summed = total VM size.
#   * `vm_free_disk_space_bytes{path=…}` — bytes free on the storage
#                                            volume, as seen by VM.
#   * `vm_rows{type=...}`                 — total rows in storage.
#   * `vm_active_timeseries`              — currently-active series.
#   * `vm_rows_inserted_total`            — ingest counter.
#   * `vm_app_uptime_seconds`             — process uptime.
#   * `node_filesystem_avail_bytes`       — independent FS view of
#                                            free space (cross-checks
#                                            `vm_free_disk_space_bytes`).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  vmCfg = hostCfg.victoriametrics;

  # ---------------------------------------------------------------------
  # Overview row — at-a-glance numbers
  # ---------------------------------------------------------------------
  overviewPanels = [
    {
      id = 100;
      type = "stat";
      title = "Total on-disk size";
      description = ''
        `sum(vm_data_size_bytes)` — total bytes VictoriaMetrics is
        currently using on the storage path, summed across all
        components (storage, indexdb, metaindex, cache, …).
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
        colorMode = "value";
        graphMode = "area";
        textMode = "auto";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "bytes";
        decimals = 2;
      };
      targets = [
        {
          expr = "sum(vm_data_size_bytes)";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 101;
      type = "stat";
      title = "Free disk space (VM view)";
      description = ''
        `vm_free_disk_space_bytes` as reported by VictoriaMetrics
        itself for its storage path. When this approaches the
        configured `-storage.minFreeDiskSpaceBytes` threshold, VM
        will start rejecting writes — keep a generous headroom.
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
        unit = "bytes";
        decimals = 2;
        # Colour breakpoints expressed in bytes:
        # < 5 GiB red, < 20 GiB orange, < 50 GiB yellow, else green.
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "red";
              value = null;
            }
            {
              color = "orange";
              value = 5368709120;
            }
            {
              color = "yellow";
              value = 21474836480;
            }
            {
              color = "green";
              value = 53687091200;
            }
          ];
        };
      };
      targets = [
        {
          expr = "min(vm_free_disk_space_bytes)";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 102;
      type = "stat";
      title = "Active time series";
      description = ''
        `vm_active_timeseries` — number of unique series receiving
        samples in the current ingestion interval. Cardinality is
        the dominant factor in TSDB cost; sudden growth here
        usually means a new exporter started emitting
        high-cardinality labels (e.g. per-request IDs).
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
        colorMode = "value";
        graphMode = "area";
        textMode = "auto";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "short";
        decimals = 0;
      };
      targets = [
        {
          expr = "sum(vm_active_timeseries)";
          refId = "A";
          instant = true;
        }
      ];
    }
    {
      id = 103;
      type = "stat";
      title = "Rows stored";
      description = ''
        `sum(vm_rows)` — total raw samples currently persisted on
        disk across all VictoriaMetrics components. Grows
        monotonically until samples age out past the configured
        retention period (currently
        ${hostCfg.retentionPeriod}).
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
        colorMode = "value";
        graphMode = "area";
        textMode = "auto";
        orientation = "auto";
      };
      fieldConfig.defaults = {
        unit = "short";
        decimals = 0;
      };
      targets = [
        {
          expr = "sum(vm_rows)";
          refId = "A";
          instant = true;
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Storage size row
  # ---------------------------------------------------------------------
  sizePanels = [
    {
      id = 200;
      type = "timeseries";
      title = "Size on disk (by component)";
      description = ''
        `vm_data_size_bytes{type=...}` — the on-disk footprint
        broken down by VictoriaMetrics component. The big
        contributors are usually `storage/small`, `storage/big`
        (TSDB data files) and `indexdb` (inverted index from
        labels → series IDs).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 16;
        x = 0;
        y = 8;
      };
      fieldConfig.defaults = {
        unit = "bytes";
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
          "max"
        ];
      };
      targets = [
        {
          expr = "sum by (type) (vm_data_size_bytes)";
          legendFormat = "{{type}}";
          refId = "A";
        }
      ];
    }
    {
      id = 201;
      type = "timeseries";
      title = "Free disk space — VM vs node_exporter";
      description = ''
        Cross-check of two independent measurements of free space
        on the filesystem hosting VictoriaMetrics:

          * `vm_free_disk_space_bytes` — what VM sees via statfs(2)
            on its storage path.
          * `node_filesystem_avail_bytes{mountpoint="/"}` — what
            node_exporter sees for the root filesystem (where
            `/var/lib/victoriametrics` lives on host.nuc).

        These should track each other closely; divergence usually
        means VM is looking at a different filesystem (e.g.
        someone added a separate volume for VM).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 8;
        x = 16;
        y = 8;
      };
      fieldConfig.defaults = {
        unit = "bytes";
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
        };
      };
      options.legend = {
        displayMode = "list";
        placement = "bottom";
        calcs = [ "lastNotNull" ];
      };
      targets = [
        {
          expr = "min(vm_free_disk_space_bytes)";
          legendFormat = "vm_free_disk_space_bytes";
          refId = "A";
        }
        {
          # node_exporter view: avail bytes for the root FS on the
          # host running VictoriaMetrics.
          expr = ''node_filesystem_avail_bytes{host="${cfg.host_hostname}", mountpoint="/", fstype!~"tmpfs|overlay|squashfs"}'';
          legendFormat = "node_filesystem_avail_bytes (/)";
          refId = "B";
        }
      ];
    }
    {
      id = 202;
      type = "timeseries";
      title = "Size growth rate (per day)";
      description = ''
        `deriv(sum(vm_data_size_bytes)[1h:5m]) * 86400` —
        extrapolated daily growth of the TSDB based on the last
        hour. Together with the free-space panel above this gives
        you a rough "days until full" estimate.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 16;
      };
      fieldConfig.defaults = {
        unit = "bytes";
        custom = {
          drawStyle = "line";
          fillOpacity = 10;
        };
      };
      options.legend = {
        displayMode = "list";
        placement = "bottom";
        calcs = [
          "lastNotNull"
          "mean"
        ];
      };
      targets = [
        {
          expr = "deriv(sum(vm_data_size_bytes)[1h:5m]) * 86400";
          legendFormat = "bytes/day";
          refId = "A";
        }
      ];
    }
    {
      id = 203;
      type = "stat";
      title = "Estimated days until full";
      description = ''
        Free space divided by recent growth rate — a back-of-the-
        envelope projection of when the TSDB will fill the disk
        at the current write rate. Updates slowly because it
        smooths over a 1h window; treat as a planning hint, not a
        precise countdown.

        Returns the literal value `+Inf` when growth is zero or
        negative (retention is keeping up).
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 16;
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
        unit = "d";
        decimals = 1;
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "red";
              value = null;
            }
            {
              color = "orange";
              value = 14;
            }
            {
              color = "yellow";
              value = 60;
            }
            {
              color = "green";
              value = 180;
            }
          ];
        };
      };
      targets = [
        {
          # Guard against zero/negative growth — clamp_min to a tiny
          # positive number so dividing yields +Inf rather than NaN,
          # which Grafana renders as "No data".
          expr = ''
            min(vm_free_disk_space_bytes)
              /
            clamp_min(deriv(sum(vm_data_size_bytes)[1h:5m]), 1)
              / 86400
          '';
          refId = "A";
          instant = true;
        }
      ];
    }
  ];

  # ---------------------------------------------------------------------
  # Ingest row — rates and cardinality
  # ---------------------------------------------------------------------
  ingestPanels = [
    {
      id = 300;
      type = "timeseries";
      title = "Rows inserted per second";
      description = ''
        `rate(vm_rows_inserted_total[5m])` broken down by
        ingestion `type` (vmagent remote_write, OpenTSDB,
        Graphite, …). On this setup only `prometheus` /
        `vmagent` should be non-zero.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 0;
        y = 25;
      };
      fieldConfig.defaults = {
        unit = "rowsps";
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
          expr = "sum by (type) (rate(vm_rows_inserted_total[5m]))";
          legendFormat = "{{type}}";
          refId = "A";
        }
      ];
    }
    {
      id = 301;
      type = "timeseries";
      title = "Active time series";
      description = ''
        `vm_active_timeseries` — currently-active series. Steep
        ramps here are the leading indicator of cardinality
        explosions; correlate with the rows/sec panel to figure
        out whether you're seeing more data or just more series
        for the same data.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 12;
        x = 12;
        y = 25;
      };
      fieldConfig.defaults = {
        unit = "short";
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
        };
      };
      options.legend = {
        displayMode = "list";
        placement = "bottom";
        calcs = [
          "lastNotNull"
          "max"
        ];
      };
      targets = [
        {
          expr = "sum(vm_active_timeseries)";
          legendFormat = "active series";
          refId = "A";
        }
      ];
    }
    {
      id = 302;
      type = "timeseries";
      title = "Bytes-per-row (compression efficiency)";
      description = ''
        `sum(vm_data_size_bytes) / sum(vm_rows)` — average
        on-disk bytes per stored sample. A useful single number
        for tracking compression efficiency over time; healthy
        VictoriaMetrics deployments sit around 0.4–1.5 B/row.
        Persistent regressions suggest pathological label churn.
      '';
      datasource = "VictoriaMetrics";
      gridPos = {
        h = 8;
        w = 24;
        x = 0;
        y = 33;
      };
      fieldConfig.defaults = {
        unit = "bytes";
        decimals = 3;
        custom = {
          drawStyle = "line";
          fillOpacity = 5;
        };
      };
      options.legend = {
        displayMode = "list";
        placement = "bottom";
        calcs = [
          "lastNotNull"
          "mean"
        ];
      };
      targets = [
        {
          # clamp_min avoids div-by-zero during the first scrape
          # after a restart, before vm_rows has been published.
          expr = "sum(vm_data_size_bytes) / clamp_min(sum(vm_rows), 1)";
          legendFormat = "bytes/row";
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

  sizeRow = {
    id = 2;
    type = "row";
    title = "Storage size & free disk";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 7;
    };
    panels = [ ];
  };

  ingestRow = {
    id = 3;
    type = "row";
    title = "Ingest & cardinality";
    collapsed = false;
    gridPos = {
      h = 1;
      w = 24;
      x = 0;
      y = 24;
    };
    panels = [ ];
  };

  vmDashboard = {
    uid = "myconfig-victoriametrics";
    title = "VictoriaMetrics storage";
    tags = [
      "myconfig"
      "observability"
      "victoriametrics"
    ];
    schemaVersion = 39;
    version = 1;
    timezone = "browser";
    refresh = "1m";
    time = {
      from = "now-24h";
      to = "now";
    };
    templating.list = [ ];
    annotations.list = [ ];
    panels = [
      overviewRow
    ]
    ++ overviewPanels
    ++ [ sizeRow ]
    ++ sizePanels
    ++ [ ingestRow ]
    ++ ingestPanels;
  };

  vmDashboardFile = pkgs.writeText "victoriametrics-dashboard.json" (builtins.toJSON vmDashboard);
in
{
  options.myconfig.observability.host.victoriametrics = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision the "VictoriaMetrics storage" Grafana dashboard,
        which tracks the central TSDB's on-disk size
        (`vm_data_size_bytes`), free disk space
        (`vm_free_disk_space_bytes`), row counts, active series
        and ingest throughput.

        The dashboard depends on a vmagent scrape job named
        `victoriametrics` pointing at the VM `/metrics` endpoint;
        that job is added automatically by
        `myconfig.observability.client` whenever
        `services.victoriametrics.enable` is true on the same
        host.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && vmCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-victoriametrics";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = pkgs.runCommand "victoriametrics-dashboards" { } ''
            mkdir -p $out
            cp ${vmDashboardFile} $out/victoriametrics.json
          '';
        }
      ];
    };
  };
}
