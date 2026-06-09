# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# UniFi controller monitoring via `unpoller` in Prometheus mode.
#
#   UniFi controller (UCG-Fibre, UDM, etc.)
#       │ HTTPS, /api/auth/login (UniFi OS) or /api/login (legacy)
#       ▼
#   prometheus-unpoller-exporter on 127.0.0.1:9130
#       │ Prometheus scrape
#       ▼
#   vmagent (same host) ──remote_write──► VictoriaMetrics
#                                                │
#                                                ▼
#                                            Grafana
#
# Metrics are exposed under the `unpoller_*` prefix and visualised
# via the upstream UniFi-Poller dashboards vendored under
# `./dashboards/` (grafana.com IDs 11310/11311/11312/11313/11314/11315).
#
# ─── UniFi-side setup (UI-only, NOT NixOS) ───────────────────────────
#
# Before enabling this module, create a dedicated read-only local
# admin on the UniFi controller — Ubiquiti SSO accounts and accounts
# with 2FA enabled do NOT work with the controller API.
#
#   1. Log in to https://<controller>/ as the primary admin.
#   2. Settings → Admins & Users → Create New Admin.
#   3. Check "Restrict to local access only".
#   4. Role: "View Only".
#   5. 2FA: disabled on this account.
#   6. Username e.g. `unpoller`, strong password.
#   7. Smoke-test (UniFi OS — UCG / UDM / UDM-Pro):
#        curl -k -c /tmp/c -X POST -H 'Content-Type: application/json' \
#          -d '{"username":"unpoller","password":"…"}' \
#          https://<controller>/api/auth/login
#      → expect HTTP 200 + Set-Cookie.
#
# ─── Secrets ─────────────────────────────────────────────────────────
#
# The password lives in `../priv/` as an agenix secret. Declare a stub
# via `myconfig.secrets.unifi-unpoller-password = { };` on the consuming
# host and point `passwordFile` at
# `config.age.secrets.unifi-unpoller-password.path`. The default value
# below is a deliberately-obvious placeholder that lets `nix flake
# check` succeed before the private overlay has wired the real secret.
#
# This module never reads or writes anything under `../priv/`.
#
# ─── Hard-coded defaults (was previously options, now inlined) ───────
#
# These knobs are kept inline because every realistic deployment of
# this module on my network has the same answer:
#
#   url             https://192.168.1.1   (only UCG on the LAN)
#   user            unpoller              (matches the priv/ secret name)
#   verify_ssl      false                 (UCG ships a self-signed cert)
#   exporter port   9130                  (upstream default; loopback-only)
#   scrape_interval 30s                   (matches unpoller poll cadence)
#   save_*          false                 (DPI/IDS/events/alarms/anomalies
#                                          — turn on by patching here if
#                                          ever needed)
#   provisioned dashboards: sites + client / USG / UAP / USW Insights
#   (NOT Client DPI — only useful when save_dpi is on, and it isn't).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  hostCfg = cfg.host;
  unifiCfg = hostCfg.unifi;

  exporterPort = 9130;

  # ─── Build-time dashboard rewrite ────────────────────────────────
  # Each upstream dashboard declares a required Prometheus datasource
  # via `__inputs` (typically `DS_PROMETHEUS` and/or
  # `DS_UNIFI_POLLER`). Panels reference them as `"${DS_…}"` strings
  # (Grafana 5/6 schema). We replace every such reference with the
  # local VictoriaMetrics datasource UID (`victoriametrics`) and pin
  # a stable top-level `uid` so dashboards persist across rebuilds.
  rewriteJq = pkgs.writeText "unifi-dashboard-rewrite.jq" ''
    del(.__inputs, .__requires)
    | .uid = $newUid
    | (.. | objects | select(has("datasource"))) |=
        (.datasource =
          (if (.datasource | type) == "string"
           then (if .datasource == "-- Grafana --" then .datasource
                 else { type: "prometheus", uid: "victoriametrics" } end)
           elif .datasource == null then null
           else (.datasource + { uid: "victoriametrics", type: "prometheus" })
           end))
  '';

  # Maps the vendored JSON files to a stable dashboard UID.
  # `unifi-client-dpi.json` is intentionally excluded — it is only
  # useful when `save_dpi` is enabled on the controller poll, which
  # this module does not do.
  dashboards = [
    {
      file = ./dashboards/unifi-sites.json;
      uid = "myconfig-unifi-sites";
    }
    {
      file = ./dashboards/unifi-client-insights.json;
      uid = "myconfig-unifi-client-insights";
    }
    {
      file = ./dashboards/unifi-usg-insights.json;
      uid = "myconfig-unifi-usg-insights";
    }
    {
      file = ./dashboards/unifi-uap-insights.json;
      uid = "myconfig-unifi-uap-insights";
    }
    {
      file = ./dashboards/unifi-usw-insights.json;
      uid = "myconfig-unifi-usw-insights";
    }
  ];

  dashboardsDir =
    pkgs.runCommand "myconfig-unifi-dashboards"
      {
        nativeBuildInputs = [ pkgs.jq ];
      }
      (
        ''
          mkdir -p $out
        ''
        + lib.concatMapStrings (d: ''
          jq --arg newUid ${lib.escapeShellArg d.uid} \
             -f ${rewriteJq} \
             ${d.file} > $out/${baseNameOf (toString d.file)}
        '') dashboards
      );
in
{
  options.myconfig.observability.host.unifi = with lib; {
    enable = mkEnableOption "myconfig.observability.host.unifi (unpoller → vmagent → VictoriaMetrics → Grafana)";

    passwordFile = mkOption {
      type = types.path;
      default = pkgs.writeText "unifi-unpoller-default.password" "unsafe:CHANGE_ME";
      defaultText = lib.literalExpression ''pkgs.writeText "..." "unsafe:CHANGE_ME"'';
      description = ''
        Absolute path to a file containing the password for the local
        read-only UniFi admin (username `unpoller`, see module header).
        Defaults to an obviously-insecure placeholder so the module
        evaluates before the agenix secret is materialised; override
        with `config.age.secrets.<name>.path` on real hosts.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && unifiCfg.enable) {
    assertions = [
      {
        assertion = cfg.client.enable;
        message = ''
          myconfig.observability.host.unifi requires
          myconfig.observability.client to be enabled on the same
          host: the unpoller exporter binds to 127.0.0.1 and is
          scraped by the local `vmagent`, which is provisioned by
          the client module.
        '';
      }
    ];

    services.prometheus.exporters.unpoller = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = exporterPort;
      # `openFirewall` left at default false — exporter is loopback-only.
      controllers = [
        {
          url = "https://192.168.1.1";
          user = "unpoller";
          # The upstream module's `pass` option wraps the value as
          # `file://<absolute-path>` for unpoller to read at runtime.
          pass = unifiCfg.passwordFile;
          sites = "all";
          verify_ssl = false;
          save_sites = true;
          save_dpi = false;
          save_ids = false;
          save_alarms = false;
          save_anomalies = false;
          save_events = false;
          hash_pii = false;
        }
      ];
      log.quiet = true;
    };

    # Piggyback on the local vmagent. NixOS module merging unions
    # `scrape_configs` lists, so this composes with the base config
    # in `client.nix` and any other module's additions.
    services.vmagent = {
      prometheusConfig = {
        scrape_configs = [
          {
            job_name = "unifi";
            scrape_interval = "30s";
            static_configs = [
              { targets = [ "127.0.0.1:${toString exporterPort}" ]; }
            ];
          }
        ];
      };
    };

    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-unifi";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          options.path = dashboardsDir;
        }
      ];
    };
  };
}
