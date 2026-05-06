# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
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
  playlistCfg = hostCfg.playlist;
  wgIp = myconfig.metadatalib.getWgIp cfg.host_hostname;

  # Database path used by the upstream nixpkgs grafana module when no
  # external database is configured (SQLite default).
  grafanaDb = "/var/lib/grafana/data/grafana.db";

  # Build the SQL that upserts the playlist row and replaces its items
  # with the list of dashboard UIDs. The script is idempotent: it
  # always rewrites the items in `playlistCfg.dashboardUids` order so
  # that re-deployments pick up newly added dashboards.
  playlistItemsSql = lib.concatStringsSep "\n" (
    lib.imap0 (i: uid: ''
      INSERT INTO playlist_item (playlist_id, type, value, title, "order")
        VALUES ((SELECT id FROM playlist WHERE uid = '${playlistCfg.uid}' AND org_id = 1),
                'dashboard_by_uid', '${uid}', '${uid}', ${toString (i + 1)});
    '') playlistCfg.dashboardUids
  );

  playlistSqlFile = pkgs.writeText "grafana-playlist.sql" ''
    BEGIN TRANSACTION;

    -- Upsert the playlist row keyed by (org_id, uid). created_at is
    -- only set on initial insert; updated_at is always bumped.
    INSERT INTO playlist (name, "interval", org_id, created_at, updated_at, uid)
      VALUES ('${playlistCfg.name}', '${playlistCfg.interval}', 1,
              strftime('%s','now'), strftime('%s','now'), '${playlistCfg.uid}')
      ON CONFLICT(org_id, uid) DO UPDATE SET
        name = excluded.name,
        "interval" = excluded."interval",
        updated_at = excluded.updated_at;

    -- Replace items.
    DELETE FROM playlist_item
      WHERE playlist_id = (SELECT id FROM playlist
                           WHERE uid = '${playlistCfg.uid}' AND org_id = 1);

    ${playlistItemsSql}

    COMMIT;
  '';
in
{
  options.myconfig.observability.host = with lib; {
    retentionPeriod = mkOption {
      type = types.str;
      default = "90d";
      description = "VictoriaMetrics retention period.";
    };

    grafana = {
      adminPassword = mkOption {
        type = types.str;
        default = "admin";
        description = ''
          Initial Grafana admin password (placeholder, override via
          private overlay).
        '';
      };
      secretKey = mkOption {
        type = types.str;
        default = "SW2YcwTIb9zpOOhoPsMm";
        description = ''
          Grafana secret_key (placeholder, override via private overlay).
        '';
      };
    };

    playlist = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Provision a Grafana playlist that walks through every
          dashboard registered in
          `myconfig.observability.host.playlist.dashboardUids`.

          Implemented by writing directly into Grafana's SQLite
          database during a one-shot systemd unit that runs after
          `grafana.service` has applied its migrations. Grafana has
          no file-based playlist provisioning, so this is the closest
          we can get to a Nix-native, declarative configuration.
        '';
      };

      uid = mkOption {
        type = types.str;
        default = "myconfig-all-dashboards";
        description = "Stable UID used to identify the provisioned playlist.";
      };

      name = mkOption {
        type = types.str;
        default = "All myconfig dashboards";
        description = "Human-readable name of the playlist.";
      };

      interval = mkOption {
        type = types.str;
        default = "5m";
        description = ''
          How long the playlist pauses on each dashboard before
          rotating to the next one. Accepts Grafana duration strings
          like `30s`, `5m`, `1h`.
        '';
      };

      dashboardUids = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          Ordered list of dashboard UIDs that the playlist will walk
          through. Other observability sub-modules append their
          dashboard UIDs here automatically when they provision a
          dashboard, so the playlist always reflects the active set.
        '';
      };
    };
  };

  config = lib.mkIf hostCfg.enable {
    services.victoriametrics = {
      enable = true;

      listenAddress = "${wgIp}:${toString cfg.remoteWritePort}";
      retentionPeriod = hostCfg.retentionPeriod;

      basicAuthUsername = cfg.basicAuthUsername;
      basicAuthPasswordFile = toString cfg.basicAuthPasswordFile;
    };

    services.grafana = {
      enable = true;

      settings = {
        server = {
          domain = "localhost";
          http_addr = "127.0.0.1";
          http_port = cfg.grafanaPort;
        };

        security = {
          admin_user = "admin";
          admin_password = lib.mkDefault hostCfg.grafana.adminPassword;
          secret_key = lib.mkDefault hostCfg.grafana.secretKey;
        };

        "auth.anonymous" = {
          enabled = false;
        };
      };

      provision.datasources.settings = {
        # Grafana matches datasources by name and never reassigns the
        # UID of an existing row, so simply adding `uid = ...` to a
        # previously-provisioned datasource has no effect (the auto-
        # generated UID stays). Listing them in `deleteDatasources`
        # forces a drop+recreate on each startup, after which the
        # `uid` fields below are honoured. Dashboards reference these
        # datasources by their stable UIDs (`victoriametrics`, `loki`).
        deleteDatasources = [
          {
            name = "VictoriaMetrics";
            orgId = 1;
          }
          {
            name = "Loki";
            orgId = 1;
          }
        ];

        datasources = [
          {
            name = "VictoriaMetrics";
            uid = "victoriametrics";
            type = "prometheus";
            url = "http://${wgIp}:${toString cfg.remoteWritePort}";
            access = "proxy";
            isDefault = true;

            basicAuth = true;
            basicAuthUser = cfg.basicAuthUsername;
            secureJsonData = {
              basicAuthPassword = "$__file{${toString cfg.basicAuthPasswordFile}}";
            };
          }
          {
            name = "Loki";
            uid = "loki";
            type = "loki";
            url = "http://${wgIp}:${toString cfg.lokiPort}";
            access = "proxy";
            isDefault = false;
          }
        ];
      };
    };

    networking.firewall.allowedTCPPorts = [
      cfg.remoteWritePort # VictoriaMetrics remote_write endpoint
      cfg.grafanaPort # Grafana UI (only if you want it reachable directly over LAN)
      cfg.lokiPort # Loki HTTP endpoint
    ];

    # Provision a Grafana playlist that cycles through every dashboard
    # contributed via `myconfig.observability.host.playlist.dashboardUids`.
    # Grafana has no file-based playlist provisioning, so we write
    # directly into the SQLite database after grafana has finished its
    # own schema migrations on startup.
    systemd.services.grafana-playlist-provision =
      lib.mkIf (playlistCfg.enable && playlistCfg.dashboardUids != [ ])
        {
          description = "Provision myconfig Grafana playlist (${playlistCfg.uid})";
          after = [ "grafana.service" ];
          requires = [ "grafana.service" ];
          wantedBy = [ "multi-user.target" ];

          # Re-run whenever the desired playlist contents change so a
          # `nixos-rebuild switch` after adding a dashboard updates the
          # playlist instead of silently leaving it stale.
          restartTriggers = [ playlistSqlFile ];

          serviceConfig = {
            Type = "oneshot";
            User = "grafana";
            Group = "grafana";
            RemainAfterExit = true;
            ExecStart = pkgs.writeShellScript "grafana-playlist-provision" ''
              set -euo pipefail

              db=${grafanaDb}

              # Wait until grafana has created the database and run its
              # migrations (the `playlist` table appears as part of the
              # initial migration set).
              for _ in $(seq 1 60); do
                if [ -f "$db" ] && \
                   ${pkgs.sqlite}/bin/sqlite3 "$db" \
                     "SELECT 1 FROM sqlite_master WHERE type='table' AND name='playlist';" \
                   | grep -q 1; then
                  break
                fi
                sleep 1
              done

              ${pkgs.sqlite}/bin/sqlite3 "$db" < ${playlistSqlFile}
            '';
          };
        };
  };
}
