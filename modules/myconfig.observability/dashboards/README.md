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
