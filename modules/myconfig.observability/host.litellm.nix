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
# `http://<litellm host>:<port>/metrics/` (job=`litellm`) and pushes into
# the central VictoriaMetrics instance via remote_write.
#
# This module only runs on the observability *host* (where Grafana is)
# and provisions a dashboard so the metrics are immediately visualised.
#
# ─── Dashboard source ────────────────────────────────────────────────
#
# Rather than hand-maintaining panel JSON, this module provisions the
# *upstream* LiteLLM Grafana dashboard:
#
#   https://github.com/BerriAI/litellm/blob/main/cookbook/litellm_proxy_server/grafana_dashboard/dashboard_v2/grafana_dashboard.json
#
# The source is pinned through `nvfetcher.toml` (entry
# `litellm-grafana-dashboard`), which tracks LiteLLM's `main` branch but
# fetches *only* the dashboard JSON from the exact Git commit it resolves.
# The immutable commit and its hash live in `_sources/generated.nix`
# (regenerated with `nix run nixpkgs#nvfetcher`). The upstream JSON is
# never committed to this repository.
#
# At Nix build time the JSON is rewritten with `jq` (see
# `dashboardRewriteJq` below) so it drops import metadata, pins a stable
# dashboard UID, removes panels that depend on features this deployment
# does not run, and points every Prometheus datasource reference at the
# local VictoriaMetrics instance (datasource UID `victoriametrics`).
#
# ─── No PostgreSQL / database required ───────────────────────────────
#
# The operational Prometheus metrics (request rates, latency histograms,
# token throughput, deployment health) are emitted by LiteLLM's
# `prometheus` callback and need *no* database. Accordingly this module
# introduces no PostgreSQL, Prisma, virtual-key, team, or spend-log
# configuration. The upstream dashboard ships some panels that *do*
# depend on virtual keys / teams (grouped under the "LiteLLM Metrics by
# Virtual Key and Team" row, e.g. "Requests per second by Key Alias" /
# "... by Team Alias", which group by the `api_key_alias` / `team_alias`
# labels). Those panels — and that collapsed row — are stripped by the
# build-time rewrite, so the provisioned dashboard may show fewer panels
# than the upstream file. Spend-related panels are absent upstream too.
# Enable a LiteLLM database only if you need budget/spend/key features;
# it is out of scope for this module.
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

  # nvfetcher-pinned upstream dashboard JSON (see header). `src` is a
  # `fetchurl` store path pointing at the immutable-commit raw GitHub URL.
  sources = pkgs.callPackage ../../_sources/generated.nix { };
  upstreamDashboard = sources.litellm-grafana-dashboard.src;

  # ─── Build-time dashboard rewrite ────────────────────────────────
  # The upstream dashboard references a Prometheus datasource through the
  # `${DS_PROMETHEUS}` template variable and declares its own UID/title.
  # We rewrite it so it is self-contained for this deployment:
  #
  #   1. Strip `__inputs` / `__requires` (defensive — absent upstream).
  #   2. Clear the numeric Grafana DB id (let Grafana assign locally).
  #   3. Pin a stable `uid` so rebuilds update the same dashboard row.
  #   4. Localise title / tags.
  #   5. Drop the `DS_PROMETHEUS` datasource template variable (panels
  #      no longer reference `$DS_PROMETHEUS` after the rewrite below).
  #   6. Remove panels only useful with virtual keys / teams /
  #      database-backed accounting (see header). Matching by title
  #      substring *and* by PromQL label usage survives upstream renames.
  #   7. Rewrite every Prometheus datasource reference (string or object
  #      form) to `{ type: "prometheus", uid: "victoriametrics" }`.
  #      The Grafana built-in annotation datasource
  #      (`{ type: "grafana", uid: "-- Grafana --" }`, and the string
  #      `"-- Grafana --"`) and `null` datasource values are preserved.
  #
  # Uses the recursive `(.. | objects | select(has("datasource")))` update
  # pattern (built-in `jq`, no `walk` dependency) — the same approach as
  # the UniFi dashboard rewrite in `host.unifi.nix`.
  dashboardRewriteJq = pkgs.writeText "litellm-dashboard-rewrite.jq" ''
    del(.__inputs, .__requires)
    | .id = null
    | .uid = "myconfig-litellm"
    | .title = "LiteLLM"
    | .tags = ((.tags // []) + [ "myconfig", "litellm", "llm" ] | unique)
    # Drop the DS_PROMETHEUS datasource template variable: panels no
    # longer reference $DS_PROMETHEUS after the rewrite below.
    | .templating.list = [ .templating.list[]? | select(.name != "DS_PROMETHEUS") ]
    # Remove panels that are only useful with LiteLLM virtual keys /
    # teams / database-backed accounting, which this deployment does not
    # run (see the module header comment). Match by title substring and
    # by PromQL label usage so upstream renames are still caught.
    | .panels = [
        .panels[]
        | select(
            (
              ( (.title // "") | test("Virtual Key and Team|by Key Alias|by Team Alias"; "i") )
              or
              ( [ .targets[]?.expr // empty ]
                | map(test("api_key_alias|team_alias"))
                | any )
            ) | not
          )
      ]
    # Rewrite every datasource field to the local VictoriaMetrics instance,
    # preserving the Grafana built-in annotation datasource and null values.
    | (.. | objects | select(has("datasource"))) |=
        ( .datasource =
            ( if (.datasource | type) == "string"
              then ( if .datasource == "-- Grafana --" then .datasource
                     else { type: "prometheus", uid: "victoriametrics" } end )
              elif .datasource == null then null
              elif (.datasource.type // "") == "prometheus"
              then { type: "prometheus", uid: "victoriametrics" }
              else .datasource
              end ) )
  '';

  # Produce the final dashboard directory consumed by the Grafana file
  # provider. `jq -e .` fails the build if the upstream file or the
  # transformed output is not valid JSON.
  dashboardsDir =
    pkgs.runCommand "myconfig-litellm-dashboards"
      {
        nativeBuildInputs = [ pkgs.jq ];
      }
      ''
        mkdir -p "$out"
        jq -f ${dashboardRewriteJq} ${upstreamDashboard} > "$out/litellm.json"
        jq -e . "$out/litellm.json" > /dev/null
      '';
in
{
  options.myconfig.observability.host.litellm = with lib; {
    provisionDashboard = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision a Grafana dashboard for the LiteLLM proxy metrics
        (job=`litellm`, scraped by vmagent on every host that runs
        `services.litellm`). The dashboard is the upstream LiteLLM
        Grafana dashboard, pinned via `nvfetcher` and rewritten at
        build time to use the local VictoriaMetrics datasource.
      '';
    };
  };

  config = lib.mkIf (hostCfg.enable && litellmCfg.provisionDashboard) {
    services.grafana.provision.dashboards.settings = {
      apiVersion = lib.mkDefault 1;
      providers = [
        {
          name = "myconfig-litellm";
          type = "file";
          disableDeletion = true;
          updateIntervalSeconds = 60;
          # Group the dashboard under an "AI" folder in the Grafana UI
          # sidebar (created on first sync if missing).
          folder = "AI";
          options.path = dashboardsDir;
        }
      ];
    };
  };
}
