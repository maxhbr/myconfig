#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Fetch current weather + daily forecast from Open-Meteo and write
# Prometheus textfile metrics.
#
# All site-specific values are injected via pkgs.replaceVars substitutions
# before the script is installed:
#
#   textfileDir      directory to write weather.prom into
#   apiUrl           full Open-Meteo API URL with lat/lon/vars/tz
#   commonLabels     label set baked into every sample
#   httpTimeout      curl --max-time value (seconds)
#   wmoCaseBranches  case-statement branches for WMO code -> description
#
# shellcheck disable=SC2154  # variables are substituted by Nix at build time

set -euo pipefail

target="@textfileDir@/weather.prom"
tmp="$(mktemp "@textfileDir@/.weather.prom.XXXXXX")"
trap 'rm -f "$tmp"' EXIT

now_ts="$(date +%s)"

# -----------------------------------------------------------------------
# Fetch — with a short overall timeout. On failure we still emit
# `weather_scrape_success 0` plus a stale `weather_scrape_timestamp`
# so the dashboard / alerts can detect the outage.
response="$(mktemp)"
trap 'rm -f "$tmp" "$response"' EXIT

http_status="$(curl --silent --show-error \
     --max-time @httpTimeout@ \
     --user-agent "myconfig-weather-exporter/1.0" \
     --output "$response" \
     --write-out '%{http_code}' \
     '@apiUrl@' 2>/dev/null || echo 000)"

if [ "$http_status" != "200" ] || ! jq -e . "$response" > /dev/null 2>&1; then
    {
        printf '# HELP weather_scrape_success 1 if the most recent Open-Meteo fetch succeeded, 0 otherwise.\n'
        printf '# TYPE weather_scrape_success gauge\n'
        printf 'weather_scrape_success{@commonLabels@} 0\n'
        printf '# HELP weather_scrape_timestamp_seconds Unix time when the most recent scrape attempt finished.\n'
        printf '# TYPE weather_scrape_timestamp_seconds gauge\n'
        printf 'weather_scrape_timestamp_seconds{@commonLabels@} %s\n' "$now_ts"
    } > "$tmp"
    chmod 0644 "$tmp"
    mv -f "$tmp" "$target"
    trap - EXIT
    rm -f "$response"
    echo "weather-exporter: fetch failed (http_status=$http_status); emitted scrape_success=0" >&2
    exit 0
fi

# -----------------------------------------------------------------------
# Extract — one `jq -r` per scalar.
get() { jq -r "$1 // empty" "$response"; }

tval()        { get '.current.temperature_2m'; }
tapp()        { get '.current.apparent_temperature'; }
hum()         { get '.current.relative_humidity_2m'; }
pmsl()        { get '.current.pressure_msl'; }
psurf()       { get '.current.surface_pressure'; }
wspd()        { get '.current.wind_speed_10m'; }
wgust()       { get '.current.wind_gusts_10m'; }
wdir()        { get '.current.wind_direction_10m'; }
cloud()       { get '.current.cloud_cover'; }
precip()      { get '.current.precipitation'; }
rainmm()      { get '.current.rain'; }
showmm()      { get '.current.showers'; }
snowcm()      { get '.current.snowfall'; }
isday()       { get '.current.is_day'; }
wcode()       { get '.current.weather_code'; }

tmax()        { get '.daily.temperature_2m_max[0]'; }
tmin()        { get '.daily.temperature_2m_min[0]'; }
uvmax()       { get '.daily.uv_index_max[0]'; }
precipsum()   { get '.daily.precipitation_sum[0]'; }
sunrise_iso() { get '.daily.sunrise[0]'; }
sunset_iso()  { get '.daily.sunset[0]'; }

data_iso()    { get '.current.time'; }
tz()          { get '.timezone'; }

iso_to_unix() {
    local iso="$1" zone="$2"
    if [ -z "$iso" ]; then
        return 0
    fi
    TZ="$zone" date -d "$iso" +%s 2>/dev/null || true
}

emit_gauge() {
    local name="$1" help="$2" value="$3" extra="${4:-}"
    if [ -z "$value" ]; then
        return 0
    fi
    printf '# HELP %s %s\n' "$name" "$help"
    printf '# TYPE %s gauge\n' "$name"
    if [ -n "$extra" ]; then
        printf '%s{@commonLabels@,%s} %s\n' "$name" "$extra" "$value"
    else
        printf '%s{@commonLabels@} %s\n' "$name" "$value"
    fi
}

describe_wmo() {
    case "$1" in
        @wmoCaseBranches@
        *) printf 'Unknown' ;;
    esac
}

timezone="$(tz)"
[ -z "$timezone" ] && timezone="@timezone@"

data_ts="$(iso_to_unix "$(data_iso)" "$timezone")"
sunrise_ts="$(iso_to_unix "$(sunrise_iso)" "$timezone")"
sunset_ts="$(iso_to_unix  "$(sunset_iso)"  "$timezone")"
wcode_val="$(wcode)"
wdesc="$(describe_wmo "${wcode_val:-_}")"

{
    emit_gauge weather_temperature_celsius \
        "Air temperature at 2 m above ground (°C)." "$(tval)"
    emit_gauge weather_apparent_temperature_celsius \
        "Apparent (feels-like) temperature (°C)." "$(tapp)"
    emit_gauge weather_humidity_percent \
        "Relative humidity at 2 m above ground (%)." "$(hum)"
    emit_gauge weather_pressure_msl_hpa \
        "Atmospheric pressure reduced to mean sea level (hPa)." "$(pmsl)"
    emit_gauge weather_surface_pressure_hpa \
        "Atmospheric pressure at the surface (hPa)." "$(psurf)"
    emit_gauge weather_wind_speed_kmh \
        "Wind speed at 10 m above ground (km/h)." "$(wspd)"
    emit_gauge weather_wind_gusts_kmh \
        "Wind gusts at 10 m above ground (km/h)." "$(wgust)"
    emit_gauge weather_wind_direction_degrees \
        "Wind direction at 10 m above ground, meteorological (°)." "$(wdir)"
    emit_gauge weather_cloud_cover_percent \
        "Total cloud cover (%)." "$(cloud)"
    emit_gauge weather_precipitation_mm \
        "Total precipitation in the reporting interval (mm)." "$(precip)"
    emit_gauge weather_rain_mm \
        "Rain in the reporting interval (mm)." "$(rainmm)"
    emit_gauge weather_showers_mm \
        "Showers in the reporting interval (mm)." "$(showmm)"
    emit_gauge weather_snowfall_cm \
        "Snowfall in the reporting interval (cm)." "$(snowcm)"
    emit_gauge weather_is_day \
        "1 if current time is daytime at the location, 0 otherwise." "$(isday)"

    emit_gauge weather_temperature_max_celsius \
        "Forecast daily maximum air temperature (°C)." "$(tmax)"
    emit_gauge weather_temperature_min_celsius \
        "Forecast daily minimum air temperature (°C)." "$(tmin)"
    emit_gauge weather_uv_index_max \
        "Forecast daily maximum UV index (dimensionless)." "$(uvmax)"
    emit_gauge weather_precipitation_sum_mm \
        "Forecast daily total precipitation (mm)." "$(precipsum)"

    emit_gauge weather_sunrise_timestamp_seconds \
        "Unix timestamp of today's sunrise at the location." "$sunrise_ts"
    emit_gauge weather_sunset_timestamp_seconds \
        "Unix timestamp of today's sunset at the location." "$sunset_ts"

    emit_gauge weather_data_timestamp_seconds \
        "Unix timestamp of the current weather sample reported by Open-Meteo." "$data_ts"
    emit_gauge weather_scrape_timestamp_seconds \
        "Unix time when the most recent scrape attempt finished." "$now_ts"
    emit_gauge weather_scrape_success \
        "1 if the most recent Open-Meteo fetch succeeded, 0 otherwise." 1

    if [ -n "${wcode_val:-}" ]; then
        printf '# HELP weather_info Static info about the current weather (WMO code + human description).\n'
        printf '# TYPE weather_info gauge\n'
        printf 'weather_info{@commonLabels@,weather_code="%s",description="%s"} 1\n' \
            "$wcode_val" "$wdesc"
    fi
} > "$tmp"

chmod 0644 "$tmp"
mv -f "$tmp" "$target"
trap - EXIT
rm -f "$response"
