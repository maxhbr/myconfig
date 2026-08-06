# LiteLLM Grafana dashboard — build-time rewrite program (jq).
#
# Consumed by modules/myconfig.observability/host.litellm.nix. The upstream
# dashboard JSON (pinned via nvfetcher, see _sources/generated.nix) is piped
# through this program; the local additions fragment
# (modules/myconfig.observability/dashboards/litellm-local.json) is slurped
# with `--slurpfile local` so it is available here as `$local[0]`.
#
# The rewrite is intentionally conservative so an unrelated upstream datasource
# (Loki/Tempo/etc.) is never silently rewritten to Prometheus, and so an
# upstream restructure fails the build loudly (see the assertions at the end)
# rather than producing a silently incomplete dashboard.
#
# Steps:
#   1. Strip Grafana import metadata and pin a local identity (uid/title/tags).
#   2. Drop the upstream `DS_PROMETHEUS` datasource template variable.
#   3. Remove panels that depend on LiteLLM virtual keys / teams /
#      database-backed spend — this deployment runs no LiteLLM database, so
#      those metrics (grouped by `api_key_alias` / `team_alias`, or
#      `litellm_spend_metric_total`) are never populated. Matching combines a
#      stable-title substring with PromQL-label usage so upstream renames are
#      still caught; panel position is not relied upon.
#   4. Rewrite only *known Prometheus* datasource references (string or object
#      form) to the local VictoriaMetrics instance (uid `victoriametrics`).
#      The Grafana built-in annotation datasource (`-- Grafana --`, string or
#      object) and `null` datasource values are preserved, as are any
#      unrelated datasource types.
#   5. Rewrite upstream PromQL: fix two legacy gauge names the pinned upstream
#      still references, insert a `host=~"$host"` selector into every
#      `litellm_*` metric (all scraped series carry the `host` external label
#      via vmagent), and switch the fixed `[2m]` window to `$__rate_interval`.
#      Model filtering is NOT blanket-applied here (not every metric exposes
#      the `requested_model` label); the locally maintained panels below add
#      it where the metric supports it.
#   6. Append the local additions (the `$host` and `$model` template variables
#      plus the locally maintained operational panels) from the slurped
#      fragment, offsetting the local panels below the last upstream panel so
#      nothing overlaps regardless of upstream drift.
#   7. Assert the final structure so an upstream change that breaks the
#      assumptions fails the build instead of shipping a broken dashboard.

# ── 1. Strip import metadata & pin local identity ──
del(.__inputs, .__requires)
| .id = null
| .uid = "myconfig-litellm"
| .title = "LiteLLM"
| .tags = ((.tags // []) + [ "myconfig", "litellm", "llm" ] | unique)

# ── 2. Drop the upstream DS_PROMETHEUS datasource variable ──
# Panels no longer reference $DS_PROMETHEUS after step 4.
| .templating.list = [ .templating.list[]? | select(.name != "DS_PROMETHEUS") ]

# ── 3. Remove key / team / spend panels ──
| .panels = [
    .panels[]
    | select(
        (
          ( (.title // "") | test("Virtual Key and Team|by Key Alias|by Team Alias|spend"; "i") )
          or
          ( [ .targets[]?.expr // empty ]
            | map(test("api_key_alias|team_alias|litellm_spend_metric"))
            | any )
        ) | not
      )
  ]

# ── 4. Rewrite Prometheus datasource references to VictoriaMetrics ──
| (.. | objects | select(has("datasource"))) |=
    ( .datasource =
        ( if (.datasource | type) == "string" then
            ( if   .datasource == "${DS_PROMETHEUS}"
                 or .datasource == "$DS_PROMETHEUS"
                 or (.datasource | ascii_downcase) == "prometheus"
              then { type: "prometheus", uid: "victoriametrics" }
              else .datasource end )
          elif (.datasource | type) == "object" then
            ( if   (.datasource.type // "") == "prometheus"
                 or (.datasource.uid // "") == "${DS_PROMETHEUS}"
                 or (.datasource.uid // "") == "$DS_PROMETHEUS"
              then { type: "prometheus", uid: "victoriametrics" }
              else .datasource end )
          else .datasource end ) )

# ── 5. Rewrite upstream PromQL ──
# 5a. The pinned upstream references legacy gauge names without the `_metric`
#     suffix; the deployed LiteLLM version emits `litellm_remaining_requests_metric`
#     and `litellm_remaining_tokens_metric`. The negative lookahead avoids
#     double-suffixing an already-correct name.
# 5b. Insert `host=~"$host"` into every litellm_* metric selector (bare, with
#     an existing `{...}`, or with a `[range]`). The `(?!host=)` lookahead and
#     the bare-match lookahead prevent inserting twice. Only the `host` label
#     is added here; `requested_model` filtering belongs to the local panels.
# 5c. Use Grafana's `$__rate_interval` instead of the upstream's fixed `[2m]`.
| (.. | objects | select(has("expr"))?) |=
    ( .expr =
        ( (.expr // "")
          | gsub("litellm_remaining_requests(?![A-Za-z0-9_])"; "litellm_remaining_requests_metric")
          | gsub("litellm_remaining_tokens(?![A-Za-z0-9_])"; "litellm_remaining_tokens_metric")
          | gsub("(?<m>litellm_[A-Za-z0-9_]+)\\["; "\(.m){host=~\"$host\"}[")
          | gsub("(?<m>litellm_[A-Za-z0-9_]+)\\{(?!host=)(?<b>[^}]*)\\}"; "\(.m){host=~\"$host\",\(.b)}")
          | gsub("(?<m>litellm_[A-Za-z0-9_]+)(?![A-Za-z0-9_\\[{])"; "\(.m){host=~\"$host\"}")
          | gsub("\\[2m\\]"; "[$__rate_interval]") ) )

# ── 6. Append local additions (variables + panels) ──
# $local is the slurped local fragment; $local[0] = { variables, panels }.
# Local panels use relative gridPos.y; offset them below the last upstream
# panel (max y+h) so nothing overlaps regardless of upstream drift.
| ( [ .panels[] | ((.gridPos.y // 0) + (.gridPos.h // 0)) ] | max ) as $maxy
| .templating.list = .templating.list + $local[0].variables
| .panels = .panels + ( $local[0].panels | map(.gridPos.y = ((.gridPos.y // 0) + $maxy)) )

# ── 7. Drift / structural assertions ──
| if (.panels | type) != "array" or (.panels | length) == 0
  then error("LiteLLM dashboard: no panels array (upstream structure changed?)") else . end
| if (.title // "") == ""
  then error("LiteLLM dashboard: missing title") else . end
| if ([.templating.list[]?.name] | index("host") | not)
  then error("LiteLLM dashboard: $host variable missing") else . end
| if ([.templating.list[]?.name] | index("model") | not)
  then error("LiteLLM dashboard: $model variable missing") else . end
| if ([.. | objects | select(has("datasource")) | .datasource.uid // empty]
        | any(. == "victoriametrics") | not)
  then error("LiteLLM dashboard: no victoriametrics datasource reference") else . end
| if ([.. | objects | select(has("expr")) | .expr // empty] | any(test("litellm_")) | not)
  then error("LiteLLM dashboard: no LiteLLM PromQL target found") else . end
| if ([.panels[] | select(.targets) | .targets[].expr // empty]
        | any(test("litellm_proxy_total_requests_metric")) | not)
  then error("LiteLLM dashboard: no request panel") else . end
| if ([.panels[] | select(.targets) | .targets[].expr // empty]
        | any(test("litellm_proxy_failed_requests_metric")) | not)
  then error("LiteLLM dashboard: no failure panel") else . end
| if ([.panels[] | select(.targets) | .targets[].expr // empty]
        | any(test("_bucket|histogram_quantile")) | not)
  then error("LiteLLM dashboard: no latency panel") else . end
