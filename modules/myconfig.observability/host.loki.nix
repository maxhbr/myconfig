# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Loki log aggregation server, runs alongside VictoriaMetrics + Grafana
# on the central observability host. Clients forward logs here via Alloy
# (see ./client.alloy.nix).
{
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  wgIp = myconfig.metadatalib.getWgIp cfg.host_hostname;
in
{
  options.myconfig.observability.host.loki = with lib; {
    retentionPeriod = mkOption {
      type = types.str;
      default = "720h"; # 30 days
      description = "Loki log retention period (compactor delay until logs are deleted).";
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
        };

        analytics.reporting_enabled = false;
      };
    };

    # The firewall opening for cfg.lokiPort is done centrally in host.nix
    # alongside the VictoriaMetrics + Grafana ports.
  };
}
