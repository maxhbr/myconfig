# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Grafana dashboard for LiteLLM proxy metrics.
#
# The metrics themselves are produced by the LiteLLM service running on
# any client where `services.litellm.enable = true` and
# `myconfig.observability.client.enable = true` — the client module wraps the
# litellm package with `prometheus_client` and registers the `prometheus`
# callback (see modules/myconfig.ai/services.litellm.nix). The local vmagent
# on the client scrapes `http://<litellm host>:<port>/metrics/` (job=`litellm`,
# note the trailing slash — the bare `/metrics` endpoint redirects with HTTP
# 307) and remote-writes into the central VictoriaMetrics instance. vmagent
# attaches a `host` label to every series via `external_labels`, so every
# LiteLLM metric carries `host=<hostname>` in VictoriaMetrics.
#
# This module only runs on the observability *host* (where Grafana is) and
# provisions a dashboard so the metrics are immediately visualised.
#
# ─── Dashboard source: upstream + local additions ─────────────────────
#
# Rather than hand-maintaining panel JSON, this module provisions the
# *upstream* LiteLLM Grafana dashboard, pinned through `nvfetcher.toml`
# (entry `litellm-grafana-dashboard`). That source tracks LiteLLM's `main`
# branch but `fetch.url` downloads *only* the dashboard JSON from the exact
# immutable Git commit nvfetcher resolves (see `_sources/generated.nix`,
# regenerated with `nix run nixpkgs#nvfetcher`). The upstream JSON is never
# committed to this repository.
#
# The upstream dashboard is not usable as-is for this deployment, so it is
# rewritten at Nix build time with `jq` (the rewrite program lives in
# ./dashboards/litellm-rewrite.jq). The rewrite:
#
#   * strips Grafana import metadata and pins a stable dashboard identity
#     (`uid = "myconfig-litellm"`, `title = "LiteLLM"`, local tags) so
#     rebuilds update the same dashboard row instead of cloning it;
#   * drops the upstream `DS_PROMETHEUS` datasource template variable and
#     rewrites every *Prometheus* datasource reference (string or object
#     form) to `{ type: "prometheus", uid: "victoriametrics" }` — the local
#     VictoriaMetrics datasource. Only Prometheus references are rewritten, so
#     a future Loki/Tempo/etc. datasource is left untouched. Grafana's
#     built-in `-- Grafana --` annotation datasource and `null` datasources
#     are preserved;
#   * removes panels that depend on LiteLLM virtual keys / teams /
#     database-backed spend (`api_key_alias`, `team_alias`,
#     `litellm_spend_metric_total`) — this deployment runs no LiteLLM
#     database, so those panels would always be empty;
#   * inserts a `host=~"$host"` selector into every upstream LiteLLM PromQL
#     target (every scraped series carries the `host` external label) and
#     switches the fixed `[2m]` window to `$__rate_interval`. It also fixes
#     two legacy gauge names the pinned upstream still references.
#
# The upstream dashboard aggregates globally and has no host/model filters,
# only p50 (not p95) latency, and no token/TTFT/deployment/overhead panels.
# Those views are therefore added as locally maintained panels, defined in
# ./dashboards/litellm-local.json (a small repository-owned fragment — *not* a
# copy of the upstream dashboard) and appended by the rewrite. The fragment
# also contributes the `$host` and `$model` template variables:
#
#   * `$host`  — `label_values(litellm_proxy_total_requests_metric_total, host)`,
#     multi-select with an `All` option defaulting to all hosts, so multiple
#     LiteLLM hosts are never silently aggregated together;
#   * `$model` — `label_values(litellm_proxy_total_requests_metric_total{host=~"$host"},
#     requested_model)`, depending on `$host`, multi-select with an `All`
#     option. `requested_model` is the model-group label LiteLLM emits on the
#     request/latency/token/deployment metrics (verified against the pinned
#     LiteLLM source). The overhead metric exposes `model_group` instead
#     (same values as `requested_model`), so its panel filters on `model_group`.
#
# The rewrite ends with structural assertions (see litellm-rewrite.jq) so that
# an upstream change violating the assumptions fails the build loudly instead
# of shipping a silently incomplete dashboard. `jq -e .` additionally fails
# the build on invalid input or output JSON.
#
# ─── No PostgreSQL / database required ───────────────────────────────
#
# The operational Prometheus metrics (request rates, latency histograms,
# token throughput, deployment health) are emitted by LiteLLM's `prometheus`
# callback and need *no* database. Accordingly this module introduces no
# PostgreSQL, Prisma, virtual-key, team, or spend-log configuration. Spend
# logs remain disabled (services.litellm.settings.general_settings.
# disable_spend_logs = true, set in modules/myconfig.ai/services.litellm.nix).
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

  # Build-time dashboard rewrite (see ./dashboards/litellm-rewrite.jq) plus
  # the locally maintained additions (./dashboards/litellm-local.json). Both
  # are committed, reviewable files; the Nix store path is taken via the
  # relative path so a `git add` is required for Nix to see them.
  rewriteJq = ./dashboards/litellm-rewrite.jq;
  localFragment = ./dashboards/litellm-local.json;

  # Produce the final dashboard directory consumed by the Grafana file
  # provider. `jq -f` applies the rewrite (slurping the local fragment with
  # `--slurpfile local`); the rewrite's own assertions fail the build on
  # upstream drift, and the trailing `jq -e .` fails it on invalid JSON.
  dashboardsDir =
    pkgs.runCommand "myconfig-litellm-dashboards"
      {
        nativeBuildInputs = [ pkgs.jq ];
      }
      ''
        mkdir -p "$out"
        jq --slurpfile local ${localFragment} -f ${rewriteJq} ${upstreamDashboard} \
          > "$out/litellm.json"
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
        `services.litellm`). The dashboard is the upstream LiteLLM Grafana
        dashboard, pinned via `nvfetcher` and rewritten at build time to use
        the local VictoriaMetrics datasource with host/model filtering and
        locally maintained operational panels.
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
          # sidebar (created on first sync if missing). The stable
          # `uid = "myconfig-litellm"` (pinned by the rewrite) means rebuilds
          # update the same dashboard row instead of creating duplicates.
          folder = "AI";
          options.path = dashboardsDir;
        }
      ];
    };
  };
}
