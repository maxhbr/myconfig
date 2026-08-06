# UniFi-Poller dashboards (vendored)

The six JSON files in this directory are upstream
[UniFi-Poller](https://github.com/unpoller/unpoller) Grafana dashboards
published on https://grafana.com/grafana/dashboards/ by the unpoller
project. They are committed verbatim — the build-time `runCommand` in
`../host.unifi.nix` patches the datasource references to point at the
local VictoriaMetrics instance (UID `victoriametrics`) and pins the
dashboard `uid` so each row remains stable across rebuilds.

## Provenance

Fetched on 2026-06-09 via `curl https://grafana.com/api/dashboards/<id>/revisions/latest/download`.

| File                            | grafana.com ID | Revision (`.revision` field) | Dashboard title                              | SHA-256 (full file)                                                |
|---------------------------------|----------------|------------------------------|----------------------------------------------|--------------------------------------------------------------------|
| `unifi-sites.json`              | 11311          | 9                            | UniFi-Poller: Network Sites - Prometheus     | `ab26d2b9aa9cd75b4ca4e9cf081e9b316ae1b9b9e6c6e6ad7c6e79729b2b9f5f` |
| `unifi-client-insights.json`    | 11315          | 30                           | UniFi-Poller: Client Insights - Prometheus   | `e93f592796f3233b7d8c1aa5f378cd66c2c3b31f458a1fdd50b0ab561604c6e5` |
| `unifi-client-dpi.json`         | 11310          | 50                           | UniFi-Poller: Client DPI - Prometheus        | `9971ec22d88ce0b918ff6778bb98f377d0782685e93e4df848b10c5e3aa0c358` |
| `unifi-usg-insights.json`       | 11313          | 36                           | UniFi-Poller: USG Insights - Prometheus      | `eb7fb7278c940b4969a564769b9d9e21ad442fe144595cbce96ce29b117db651` |
| `unifi-uap-insights.json`       | 11314          | 24                           | UniFi-Poller: UAP Insights - Prometheus      | `db02acdf9ac2ff638860291963029a49e095b70655c961d5a044d41fa8842cbc` |
| `unifi-usw-insights.json`       | 11312          | 35                           | UniFi-Poller: USW Insights - Prometheus      | `a192f47f1b08e10fbcbe725390c27817349d0938a341c6e4aefb9b5aee5f02ed` |

Note: the grafana.com dashboard IDs do **not** follow an obvious naming
pattern (e.g. `11314` is UAP Insights, not Sites). The table above is
the authoritative mapping; double-check by opening the URL
`https://grafana.com/grafana/dashboards/<id>/` if in doubt.

## Refreshing

To pull a newer upstream revision:

```bash
for pair in 11311:unifi-sites 11315:unifi-client-insights \
            11310:unifi-client-dpi 11313:unifi-usg-insights \
            11314:unifi-uap-insights 11312:unifi-usw-insights; do
  id=${pair%%:*}
  name=${pair##*:}
  curl -sSL -o "${name}.json" \
    "https://grafana.com/api/dashboards/${id}/revisions/latest/download"
  jq -e . "${name}.json" > /dev/null  # validate
done
```

After refreshing, update the revision and SHA-256 columns above and run
`./nixfmtall.sh` + `nix flake check`.

## Build-time transform (see `../host.unifi.nix`)

Each dashboard is passed through `jq` at NixOS build time to:
1. Strip `__inputs` / `__requires` (we hard-code the datasource).
2. Rewrite every `datasource` field that looks like `${DS_…}` (string)
   or `{ uid: "${DS_…}", ... }` (object) to
   `{ type: "prometheus", uid: "victoriametrics" }`. The Grafana
   built-in `"-- Grafana --"` annotation datasource is preserved as
   is; `null` datasources (custom variables) stay `null`.
3. Replace the top-level `uid` with a stable `myconfig-unifi-<slug>`
   so subsequent rebuilds update the same dashboard row in Grafana
   instead of cloning a new one.

---

## LiteLLM dashboard (fetched via nvfetcher, NOT vendored here)

Unlike the UniFi JSON files above, the LiteLLM Grafana dashboard is
**not** committed to this directory. It is fetched at build time from
the upstream LiteLLM repository and rewritten on the fly. Two helper
files *are* committed here and contain only repository-specific
additions — never a copy of the upstream dashboard:

- `litellm-rewrite.jq` — the build-time `jq` rewrite program.
- `litellm-local.json` — a small fragment with the `$host` / `$model`
  template variables and the locally maintained operational panels.

### Pin

`nvfetcher.toml` entry `litellm-grafana-dashboard` tracks LiteLLM's
`main` branch (`src.git`), but `fetch.url` downloads only the dashboard
JSON from the exact commit nvfetcher resolves (`$ver` -> immutable
commit). The commit and hash are frozen in `_sources/generated.nix`
(regenerate with `nix run nixpkgs#nvfetcher`); the generated URL
contains a full 40-char Git commit, so the fetch is immutable. No
upstream repository clone enters the Nix store — only the single JSON
file, consumed via `pkgs.fetchurl` (not `fetchFromGitHub`).

Upstream path (inside `BerriAI/litellm`):
`cookbook/litellm_proxy_server/grafana_dashboard/dashboard_v2/grafana_dashboard.json`.

### Build-time rewrite (`litellm-rewrite.jq`, wired by `../host.litellm.nix`)

The upstream JSON is piped through `litellm-rewrite.jq` inside a
`runCommand`, with `litellm-local.json` slurped via `--slurpfile local`.
The rewrite:

1. Strips `__inputs` / `__requires`, clears the numeric Grafana `id`,
   pins a stable `uid = "myconfig-litellm"` (so rebuilds update the same
   dashboard row instead of cloning), and localises `title` / `tags`.
2. Drops the upstream `DS_PROMETHEUS` datasource template variable.
3. Removes panels that depend on LiteLLM virtual keys / teams /
   database-backed spend (grouped by `api_key_alias` / `team_alias`, or
   referencing `litellm_spend_metric_total`). This deployment runs no
   LiteLLM database, so those panels would always be empty. Removal
   matches on a stable-title substring **and** PromQL-label usage, not
   panel position.
4. Rewrites only *known Prometheus* datasource references (string or
   object form — `${DS_PROMETHEUS}`, `$DS_PROMETHEUS`, `prometheus`, or
   `{ type: "prometheus", uid: "..." }`) to
   `{ type: "prometheus", uid: "victoriametrics" }`. The Grafana
   built-in `"-- Grafana --"` annotation datasource (string or object)
   and `null` datasources are preserved, as are any unrelated
   datasource types — the rewrite will not silently turn a future
   Loki/Tempo datasource into Prometheus.
5. Rewrites upstream PromQL: fixes two legacy gauge names the pinned
   upstream still references, inserts `host=~"$host"` into every
   `litellm_*` metric (all scraped series carry the `host` external
   label from vmagent), and switches the fixed `[2m]` window to
   `$__rate_interval`. Model filtering is **not** blanket-applied (not
   every metric exposes the `requested_model` label); the local panels
   add it where the metric supports it.
6. Appends the local additions from `litellm-local.json`: the `$host`
   and `$model` template variables plus the locally maintained panels
   (request-by-model, failure-rate ratio, latency p50/p95, TTFT,
   overhead, token throughput, deployment health, fallbacks). Local
   panels are offset below the last upstream panel so nothing overlaps
   regardless of upstream drift.
7. Asserts the final structure (panels array, title, `$host` / `$model`
   variables, a `victoriametrics` datasource reference, at least one
   LiteLLM PromQL target, and at least one request / failure / latency
   panel) so an upstream change that breaks the assumptions fails the
   build loudly. A trailing `jq -e .` fails it on invalid JSON.

### Scrape & datasource

- vmagent scrapes the **trailing-slash** endpoint `/metrics/` (the bare
  `/metrics` endpoint redirects with HTTP 307). See
  `modules/myconfig.observability/client.nix` (job `litellm`).
- The dashboard uses the datasource UID `victoriametrics` (the existing
  VictoriaMetrics Grafana datasource). No PostgreSQL, Prisma, virtual-key,
  team, or spend-log configuration is involved — the operational metrics
  come from LiteLLM's `prometheus` callback and need no database. Spend
  logs remain disabled.

### Result

A single `litellm.json` placed in a `runCommand` output dir consumed by
the Grafana file provider (folder `AI`, UID `myconfig-litellm`).

To refresh the upstream dashboard, run `nix run nixpkgs#nvfetcher`
(which re-checks every nvfetcher source, not only LiteLLM) and review
the resulting `_sources/generated.nix` diff. If the rewrite assertions
then fail, `litellm-rewrite.jq` / `litellm-local.json` need updating to
match the new upstream layout.
