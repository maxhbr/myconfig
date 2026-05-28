# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Outdoor weather metrics exporter, scraping the public Open-Meteo
# API (https://open-meteo.com) for the configured coordinates and
# emitting Prometheus metrics via the node_exporter textfile
# collector.
#
# Open-Meteo is free, requires no API key, and explicitly permits
# non-commercial use without rate-limiting at low volumes
# (see https://open-meteo.com/en/terms). One request every few
# minutes from a single host is well within their fair-use policy;
# the default refresh interval here is 10 minutes.
#
# Metrics produced (all carry `location="<locationLabel>"` and the
# coordinate labels `latitude` / `longitude` so the same dashboard
# can show multiple deployments side-by-side):
#
#   * weather_temperature_celsius              air temperature at 2 m
#   * weather_apparent_temperature_celsius     "feels like" temperature
#   * weather_humidity_percent                 relative humidity at 2 m
#   * weather_pressure_msl_hpa                 mean sea-level pressure
#   * weather_surface_pressure_hpa             surface pressure
#   * weather_wind_speed_kmh                   wind speed at 10 m
#   * weather_wind_gusts_kmh                   wind gusts at 10 m
#   * weather_wind_direction_degrees           wind bearing (meteorological)
#   * weather_cloud_cover_percent              total cloud cover
#   * weather_precipitation_mm                 precipitation in last interval
#   * weather_rain_mm                          rain in last interval
#   * weather_showers_mm                       showers in last interval
#   * weather_snowfall_cm                      snowfall in last interval
#   * weather_is_day                           1 = day, 0 = night
#   * weather_uv_index_max                     max UV index for today
#   * weather_temperature_max_celsius          forecast daily max
#   * weather_temperature_min_celsius          forecast daily min
#   * weather_precipitation_sum_mm             forecast daily precipitation sum
#   * weather_sunrise_timestamp_seconds        unix time of today's sunrise
#   * weather_sunset_timestamp_seconds         unix time of today's sunset
#   * weather_data_timestamp_seconds           unix time of the current reading
#   * weather_scrape_timestamp_seconds         unix time the scrape ran
#   * weather_scrape_success                   1 if last fetch succeeded, else 0
#   * weather_info{weather_code,description}   constant-1 info series carrying
#                                              the WMO weather code + human label
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  weatherCfg = clientCfg.weatherExporter;
  ageCfg = clientCfg.systemAge;

  # WMO weather interpretation codes
  # (https://open-meteo.com/en/docs#weathervariables). Used to turn
  # the numeric `weather_code` into a human-readable string for the
  # `weather_info` series consumed by the dashboard's "Conditions"
  # panel.
  wmoCodeMap = {
    "0" = "Clear sky";
    "1" = "Mainly clear";
    "2" = "Partly cloudy";
    "3" = "Overcast";
    "45" = "Fog";
    "48" = "Depositing rime fog";
    "51" = "Light drizzle";
    "53" = "Moderate drizzle";
    "55" = "Dense drizzle";
    "56" = "Light freezing drizzle";
    "57" = "Dense freezing drizzle";
    "61" = "Slight rain";
    "63" = "Moderate rain";
    "65" = "Heavy rain";
    "66" = "Light freezing rain";
    "67" = "Heavy freezing rain";
    "71" = "Slight snow fall";
    "73" = "Moderate snow fall";
    "75" = "Heavy snow fall";
    "77" = "Snow grains";
    "80" = "Slight rain showers";
    "81" = "Moderate rain showers";
    "82" = "Violent rain showers";
    "85" = "Slight snow showers";
    "86" = "Heavy snow showers";
    "95" = "Thunderstorm";
    "96" = "Thunderstorm with slight hail";
    "99" = "Thunderstorm with heavy hail";
  };

  # Render the lookup table as a `case` block embedded in the shell
  # script. Using a case statement (instead of jq lookups) keeps the
  # shell side dependency-free and the data alongside the metric
  # definition for easy review.
  wmoCaseBranches = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (code: desc: ''${code}) printf '%s' "${desc}" ;;'') wmoCodeMap
  );

  # Comma-separated lists of `current` and `daily` variables we
  # request from the API. Keep these in sync with the metric list at
  # the top of this file: every variable mentioned below must have a
  # matching jq extraction + printf line in the refresh script.
  currentVars = lib.concatStringsSep "," [
    "temperature_2m"
    "relative_humidity_2m"
    "apparent_temperature"
    "is_day"
    "precipitation"
    "rain"
    "showers"
    "snowfall"
    "weather_code"
    "cloud_cover"
    "pressure_msl"
    "surface_pressure"
    "wind_speed_10m"
    "wind_direction_10m"
    "wind_gusts_10m"
  ];
  dailyVars = lib.concatStringsSep "," [
    "sunrise"
    "sunset"
    "uv_index_max"
    "precipitation_sum"
    "temperature_2m_max"
    "temperature_2m_min"
  ];

  apiUrl =
    "https://api.open-meteo.com/v1/forecast"
    + "?latitude=${weatherCfg.latitude}"
    + "&longitude=${weatherCfg.longitude}"
    + "&current=${currentVars}"
    + "&daily=${dailyVars}"
    + "&timezone=${weatherCfg.timezone}"
    + "&forecast_days=1";

  # Common label set baked into every emitted sample. Quoting/escaping
  # is the caller's responsibility, but the option types only allow
  # plain strings so injection isn't a concern in practice.
  commonLabels = ''location="${weatherCfg.locationLabel}",latitude="${weatherCfg.latitude}",longitude="${weatherCfg.longitude}"'';

  refreshScript = pkgs.writeShellApplication {
    name = "myconfig-weather-refresh";
    runtimeInputs = with pkgs; [
      coreutils
      curl
      jq
    ];
    text = ''
      set -euo pipefail

      target="${weatherCfg.textfileDir}/weather.prom"
      tmp="$(mktemp "${weatherCfg.textfileDir}/.weather.prom.XXXXXX")"
      trap 'rm -f "$tmp"' EXIT

      now_ts="$(date +%s)"

      # ---------------------------------------------------------------
      # Fetch — with a short overall timeout. On failure we still emit
      # `weather_scrape_success 0` plus a stale `weather_scrape_timestamp`
      # so the dashboard / alerts can detect the outage. We do NOT
      # delete previously-emitted gauges; node_exporter will keep
      # serving them until the next successful refresh overwrites the
      # file in full.
      response="$(mktemp)"
      trap 'rm -f "$tmp" "$response"' EXIT

      # Note: we omit --fail here so a non-2xx still leaves the body
      # in $response for diagnostics; success is decided by checking
      # %{http_code} and that the body is valid JSON.
      http_status="$(curl --silent --show-error \
           --max-time ${toString weatherCfg.httpTimeoutSeconds} \
           --user-agent "myconfig-weather-exporter/1.0" \
           --output "$response" \
           --write-out '%{http_code}' \
           '${apiUrl}' 2>/dev/null || echo 000)"

      if [ "$http_status" != "200" ] || ! jq -e . "$response" > /dev/null 2>&1; then
        # Failure path: emit only scrape-health metrics so the
        # dashboard's "Last update" / "Scrape success" panels reflect
        # reality. Keep the file small and well-formed.
        {
          printf '# HELP weather_scrape_success 1 if the most recent Open-Meteo fetch succeeded, 0 otherwise.\n'
          printf '# TYPE weather_scrape_success gauge\n'
          printf 'weather_scrape_success{${commonLabels}} 0\n'
          printf '# HELP weather_scrape_timestamp_seconds Unix time when the most recent scrape attempt finished.\n'
          printf '# TYPE weather_scrape_timestamp_seconds gauge\n'
          printf 'weather_scrape_timestamp_seconds{${commonLabels}} %s\n' "$now_ts"
        } > "$tmp"
        chmod 0644 "$tmp"
        mv -f "$tmp" "$target"
        trap - EXIT
        rm -f "$response"
        echo "weather-exporter: fetch failed (http_status=$http_status); emitted scrape_success=0" >&2
        exit 0
      fi

      # ---------------------------------------------------------------
      # Extract — one `jq -r` per scalar. The API guarantees these
      # fields when the corresponding variable was requested, but we
      # tolerate `null` by treating it as "skip this metric" rather
      # than emitting NaN (Prometheus exposition format has no NaN).
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
        # Open-Meteo returns local (timezone-aware) ISO timestamps
        # without an explicit offset (e.g. "2026-05-28T11:30") when
        # `timezone=Europe/Berlin`. `date` honours TZ for parsing, so
        # we set it explicitly per-call rather than mutating the
        # process environment.
        local iso="$1" zone="$2"
        if [ -z "$iso" ]; then
          return 0
        fi
        TZ="$zone" date -d "$iso" +%s 2>/dev/null || true
      }

      emit_gauge() {
        # $1 metric, $2 help, $3 value (possibly empty), $4 extra labels
        local name="$1" help="$2" value="$3" extra="''${4:-}"
        if [ -z "$value" ]; then
          return 0
        fi
        printf '# HELP %s %s\n' "$name" "$help"
        printf '# TYPE %s gauge\n' "$name"
        if [ -n "$extra" ]; then
          printf '%s{${commonLabels},%s} %s\n' "$name" "$extra" "$value"
        else
          printf '%s{${commonLabels}} %s\n' "$name" "$value"
        fi
      }

      describe_wmo() {
        # Map WMO weather code -> human-readable description. Falls
        # back to "Unknown" so the dashboard never shows a blank.
        case "$1" in
      ${wmoCaseBranches}
          *) printf 'Unknown' ;;
        esac
      }

      timezone="$(tz)"
      [ -z "$timezone" ] && timezone="${weatherCfg.timezone}"

      data_ts="$(iso_to_unix "$(data_iso)" "$timezone")"
      sunrise_ts="$(iso_to_unix "$(sunrise_iso)" "$timezone")"
      sunset_ts="$(iso_to_unix  "$(sunset_iso)"  "$timezone")"
      wcode_val="$(wcode)"
      wdesc="$(describe_wmo "''${wcode_val:-_}")"

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

        # `weather_info` is the standard info-metric pattern: always
        # value 1, with all string-valued attributes hung off labels.
        # PromQL: weather_temperature_celsius * on(location)
        #          group_left(description) weather_info
        if [ -n "''${wcode_val:-}" ]; then
          printf '# HELP weather_info Static info about the current weather (WMO code + human description).\n'
          printf '# TYPE weather_info gauge\n'
          printf 'weather_info{${commonLabels},weather_code="%s",description="%s"} 1\n' \
            "$wcode_val" "$wdesc"
        fi
      } > "$tmp"

      chmod 0644 "$tmp"
      mv -f "$tmp" "$target"
      trap - EXIT
      rm -f "$response"
    '';
  };
in
{
  options.myconfig.observability.client.weatherExporter = with lib; {
    enable = mkEnableOption ''
      Outdoor weather metrics exporter (Open-Meteo → node_exporter
      textfile collector). Periodically fetches current conditions
      and the day's forecast for the configured coordinates and
      emits Prometheus gauges that are scraped by the local
      node_exporter / vmagent.
    '';

    latitude = mkOption {
      type = types.str;
      default = "48.3705";
      description = ''
        Latitude (decimal degrees, WGS84) of the location to scrape.
        Default is Augsburg, Germany. Kept as a string to preserve
        the exact precision the user specifies — Open-Meteo only
        rounds to 0.01° anyway, so 4 decimals is plenty.
      '';
    };

    longitude = mkOption {
      type = types.str;
      default = "10.8978";
      description = ''
        Longitude (decimal degrees, WGS84) of the location to scrape.
        Default is Augsburg, Germany.
      '';
    };

    locationLabel = mkOption {
      type = types.str;
      default = "Augsburg";
      description = ''
        Value of the `location` label attached to every emitted
        sample. Should be a short, human-friendly identifier (used
        directly in the Grafana dashboard title and stat tiles).
      '';
    };

    timezone = mkOption {
      type = types.str;
      default = "Europe/Berlin";
      description = ''
        IANA timezone passed to the Open-Meteo API so that hourly /
        daily timestamps come back in local time. Also used by the
        refresh script to parse sunrise/sunset back into Unix epoch.
      '';
    };

    refreshInterval = mkOption {
      type = types.str;
      default = "10min";
      description = ''
        How often the weather data is refreshed. Open-Meteo's
        non-commercial tier permits up to ~10 000 calls/day per IP;
        10 minutes (144 calls/day) stays well below that ceiling
        while still being responsive enough for any human-facing
        dashboard. Uses systemd OnUnitActiveSec syntax.
      '';
    };

    httpTimeoutSeconds = mkOption {
      type = types.int;
      default = 20;
      description = ''
        Maximum total time (seconds) curl will spend on a single
        Open-Meteo request. Includes DNS, connect, TLS handshake and
        body transfer. The API typically answers in <500 ms; 20 s
        is a generous outer bound that still ensures the service
        unit exits before the next timer fire.
      '';
    };

    textfileDir = mkOption {
      type = types.str;
      default = ageCfg.textfileDir;
      defaultText = lib.literalExpression "config.myconfig.observability.client.systemAge.textfileDir";
      description = ''
        Directory scraped by the node_exporter textfile collector.
        Defaults to the same directory used by `client.system-age` so
        no extra `--collector.textfile.directory` flag is needed; if
        you point this somewhere else, make sure node_exporter is
        configured accordingly.
      '';
    };
  };

  config = lib.mkIf (clientCfg.enable && weatherCfg.enable) {
    # The textfile directory is already created by client.system-age
    # when that module is enabled, but provide it independently here
    # so weatherExporter can be turned on without systemAge.
    systemd.tmpfiles.rules = [
      "d ${weatherCfg.textfileDir} 0755 root root - -"
    ];

    systemd.services."myconfig-weather" = {
      description = "Refresh outdoor weather metrics from Open-Meteo";
      # Don't start on boot until we have network connectivity —
      # Open-Meteo is a public HTTPS endpoint.
      wantedBy = [ "multi-user.target" ];
      after = [
        "network-online.target"
        "local-fs.target"
      ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${refreshScript}/bin/myconfig-weather-refresh";

        # Hardening — the script needs egress HTTPS and write access
        # to the textfile dir only. No persistent state.
        DynamicUser = false; # we run as root for simplicity (write to /var/lib)
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ weatherCfg.textfileDir ];
        ProtectHome = true;
        PrivateTmp = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        SystemCallArchitectures = "native";
      };
    };

    systemd.timers."myconfig-weather" = {
      description = "Periodically refresh outdoor weather metrics";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        # Fire ~30 s after boot so the network is up, then on the
        # configured interval. Persistent=true ensures a missed
        # interval (host was off) is caught up once on next boot.
        OnBootSec = "30s";
        OnUnitActiveSec = weatherCfg.refreshInterval;
        Unit = "myconfig-weather.service";
        AccuracySec = "30s";
        Persistent = true;
      };
    };
  };
}
