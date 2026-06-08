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
  wgIp = myconfig.metadatalib.getWgIp cfg.host_hostname;
in
{
  options.myconfig.observability.host = with lib; {
    retentionPeriod = mkOption {
      type = types.str;
      default = "900d";
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
      adminPasswordFile = mkOption {
        type = types.path;
        description = ''
          File containing the Grafana admin password, read at runtime by
          the playlist-provision service. Defaults to a file derived from
          `adminPassword` (stays in sync automatically). Override via
          private overlay with a path outside the Nix store to avoid
          exposing the real password world-readably.
        '';
      };
      secretKey = mkOption {
        type = types.str;
        default = "SW2YcwTIb9zpOOhoPsMm";
        description = ''
          Grafana secret_key (placeholder, override via private overlay).
        '';
      };
      allowAnonymous = mkEnableOption "anonymous access to Grafana (useful for kiosk displays)";

      playlist = {
        enable = mkEnableOption "auto-cycling Grafana playlist through all provisioned dashboards";

        intervalSeconds = mkOption {
          type = types.ints.positive;
          default = 60;
          description = ''
            How long (in seconds) each dashboard is shown before the
            playlist advances to the next one.
          '';
        };

        dashboardUids = mkOption {
          type = types.listOf types.str;
          default = [
            "myconfig-home-lab-status"
            "myconfig-host-status"
            "myconfig-co2"
            "myconfig-weather"
            "myconfig-temperature-humidity"
            "myconfig-power"
            "myconfig-logs"
            "myconfig-scrape-health"
          ];
          description = ''
            Ordered list of dashboard UIDs to include in the playlist.
            Dashboards that are not provisioned on this host should be
            removed from the list.
          '';
        };
      };
    };
  };

  config = lib.mkIf hostCfg.enable {
    # Derive the password file from the string option so the two stay in
    # sync automatically; override adminPasswordFile in the private overlay
    # with a path outside the Nix store to avoid world-readable exposure.
    myconfig.observability.host.grafana.adminPasswordFile = lib.mkDefault (
      pkgs.writeText "grafana-admin-password" hostCfg.grafana.adminPassword
    );

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
          allowed_origins = "https://grafana.nuc.wg0.maxhbr.local https://grafana.nuc.vserver.wg0.maxhbr.local";
        };

        security = {
          admin_user = "admin";
          admin_password = lib.mkDefault hostCfg.grafana.adminPassword;
          secret_key = lib.mkDefault hostCfg.grafana.secretKey;
        };

        "auth.anonymous" = {
          enabled = hostCfg.grafana.allowAnonymous;
          org_name = "Main Org.";
          hide_version = true;
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

    # Provision a Grafana playlist via the HTTP API after Grafana has
    # started. The playlist cycles through all configured dashboard UIDs
    # at the given interval. Because Grafana does not support playlist
    # provisioning via YAML files, a one-shot service creates or updates
    # the playlist on every Grafana restart.
    systemd.services.grafana-playlist-provision = lib.mkIf hostCfg.grafana.playlist.enable {
      description = "Provision Grafana all-dashboards playlist";
      after = [ "grafana.service" ];
      wants = [ "grafana.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "grafana";
      };
      script =
        let
          grafanaUrl = "http://127.0.0.1:${toString cfg.grafanaPort}";
          # Grafana 10+ moved playlists to a Kubernetes-style API that
          # rejects basic auth when anonymous access is enabled (the
          # request lands as an unauthenticated Viewer). We therefore
          # authenticate via a short-lived service account token instead:
          #   1. Create (or reuse) an Admin service account.
          #   2. Mint a token for it.
          #   3. Use that Bearer token for the playlist upsert.
          #   4. Delete the token afterwards.
          playlistSpec = builtins.toJSON {
            name = "All Dashboards";
            interval = "${toString hostCfg.grafana.playlist.intervalSeconds}s";
            items = map (uid: {
              title = uid;
              type = "dashboard_by_uid";
              value = uid;
            }) hostCfg.grafana.playlist.dashboardUids;
          };
          playlistFile = pkgs.writeText "grafana-playlist.json" playlistSpec;
          curl = "${pkgs.curl}/bin/curl";
          jq = "${pkgs.jq}/bin/jq";
        in
        ''
          set -euo pipefail

          GRAFANA_URL="${grafanaUrl}"
          BASIC_AUTH="admin:$(cat ${toString hostCfg.grafana.adminPasswordFile})"
          SA_NAME="playlist-provisioner"
          PLAYLIST_NAME="All Dashboards"

          # Wait until Grafana's API is reachable.
          for i in $(seq 1 30); do
            if ${curl} -sf "$GRAFANA_URL/api/health" >/dev/null 2>&1; then
              break
            fi
            echo "Waiting for Grafana… ($i/30)"
            sleep 2
          done

          # ── 1. Ensure the provisioning service account exists ──────────
          SA_ID=$(
            ${curl} -sf -u "$BASIC_AUTH" \
              "$GRAFANA_URL/api/serviceaccounts/search?query=$SA_NAME" \
            | ${jq} -r --arg n "$SA_NAME" \
                '.serviceAccounts[] | select(.name == $n) | .id' \
            || true
          )

          if [ -z "$SA_ID" ]; then
            SA_ID=$(
              ${curl} -sf -u "$BASIC_AUTH" \
                -X POST -H "Content-Type: application/json" \
                -d "{\"name\":\"$SA_NAME\",\"role\":\"Admin\",\"isDisabled\":false}" \
                "$GRAFANA_URL/api/serviceaccounts" \
              | ${jq} -r '.id'
            )
            echo "Created service account id=$SA_ID"
          else
            echo "Reusing service account id=$SA_ID"
          fi

          # ── 2. Mint a short-lived token ────────────────────────────────
          TOKEN=$(
            ${curl} -sf -u "$BASIC_AUTH" \
              -X POST -H "Content-Type: application/json" \
              -d '{"name":"playlist-provision-token","secondsToLive":120}' \
              "$GRAFANA_URL/api/serviceaccounts/$SA_ID/tokens" \
            | ${jq} -r '.key'
          )

          cleanup() {
            # Best-effort: remove ephemeral tokens left by previous runs.
            ${curl} -sf -u "$BASIC_AUTH" \
              "$GRAFANA_URL/api/serviceaccounts/$SA_ID/tokens" \
            | ${jq} -r '.[].id' \
            | while read -r TID; do
                ${curl} -sf -u "$BASIC_AUTH" -X DELETE \
                  "$GRAFANA_URL/api/serviceaccounts/$SA_ID/tokens/$TID" \
                  >/dev/null || true
              done
          }
          trap cleanup EXIT

          AUTH_HDR="Authorization: Bearer $TOKEN"

          # ── 3. Upsert the playlist ─────────────────────────────────────
          EXISTING_UID=$(
            ${curl} -sf -H "$AUTH_HDR" "$GRAFANA_URL/api/playlists" \
            | ${jq} -r --arg name "$PLAYLIST_NAME" \
                '.[] | select(.name == $name) | .uid' \
            || true
          )

          if [ -n "$EXISTING_UID" ]; then
            echo "Updating existing playlist uid=$EXISTING_UID"
            ${curl} -sf -H "$AUTH_HDR" \
              -X PUT -H "Content-Type: application/json" \
              -d @${playlistFile} \
              "$GRAFANA_URL/api/playlists/$EXISTING_UID"
          else
            echo "Creating new playlist"
            ${curl} -sf -H "$AUTH_HDR" \
              -X POST -H "Content-Type: application/json" \
              -d @${playlistFile} \
              "$GRAFANA_URL/api/playlists"
          fi

          echo "Grafana playlist provisioned."
        '';
    };
  };
}
