# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:
let
  haCfg = config.myconfig.smart-home.home-assistant;
  haPort = 8123;
  obsClientCfg = config.myconfig.observability.client;
  obsCfg = config.myconfig.observability;
in
{
  options.myconfig.smart-home.home-assistant = with lib; {
    enable = mkEnableOption "Home Assistant native installation";

    exposeOnLan = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to expose Home Assistant on the local network
        (opens the HTTP port in the firewall on all interfaces).
        This allows the mobile companion app to connect directly
        via the LAN IP, e.g. `http://192.168.1.92:8123`.
      '';
    };

    prometheus = {
      enable = mkOption {
        type = types.bool;
        default = obsClientCfg.enable or false;
        defaultText = literalExpression "config.myconfig.observability.client.enable";
        description = ''
          Whether to enable the Home Assistant `prometheus` integration
          (exposes metrics on `/api/prometheus`) and have the local
          vmagent scrape it. Defaults to the value of
          `myconfig.observability.client.enable`.
        '';
      };

      namespace = mkOption {
        type = types.str;
        default = "hass";
        description = ''
          Metric name prefix used by the Home Assistant prometheus
          integration. Becomes the leading component of every emitted
          metric (e.g. `hass_temperature_c`).
        '';
      };

      scrapeInterval = mkOption {
        type = types.str;
        default = "30s";
        description = ''
          Prometheus scrape interval for the Home Assistant target.
          Home Assistant rebuilds metrics on every request, so a slower
          interval than the global default is recommended.
        '';
      };
    };
  };

  config = lib.mkIf haCfg.enable {
    services.home-assistant = {
      enable = true;
      # Override the bundled `broadlink` Python library to register the
      # Broadlink RM5 Plus IR blaster, which upstream does not yet
      # recognise ("Device not supported"). This mirrors the manual fix
      # from
      # https://github.com/orgs/home-assistant/discussions/765#discussioncomment-14803873
      # but applies it declaratively at build time (instead of editing
      # the library inside the running container, which would be lost on
      # every HA update). The device id `0x5224` is added to the
      # `rm4pro` block of `broadlink/__init__.py`.
      #
      # Remove this override once upstream python-broadlink ships the
      # RM5 Plus device id.
      package = pkgs.home-assistant.override {
        packageOverrides = self: super: {
          broadlink = super.broadlink.overridePythonAttrs (oldAttrs: {
            # Register the Broadlink RM5 Plus IR blaster, see
            # https://github.com/orgs/home-assistant/discussions/765#discussioncomment-14803873
            postPatch = (oldAttrs.postPatch or "") + ''
              rm5plus=$'    rm4pro: {\n        0x5224: ("RM5 plus", "Broadlink"),'
              substituteInPlace broadlink/__init__.py \
                --replace-fail '    rm4pro: {' "$rm5plus"
            '';
          });
        };
      };
      extraComponents = [
        # Basic/default UI pieces
        "analytics"
        "default_config"
        "google_translate"
        "met"
        "radio_browser"
        "shopping_list"
        "isal"

        # Common local integrations
        "mobile_app"
        "shelly"
        "esphome"
        "mqtt"
        "wled"
        "zha"
        "zeroconf"
        "ssdp"
        "dhcp"
        "broadlink" # Broadlink IR/RF blasters (RM5 Plus, see package override above)

        # Media / cameras / streams
        "ffmpeg"
        "stream"
        "dlna_dmr"
        "cast"

        # Device ecosystems
        # Zigbee is handled via Zigbee2MQTT -> the `mqtt` component
        # above picks up Z2M's MQTT discovery messages.
        "elgato" # Elgato Key Light / Light Strip (discovered via zeroconf _elg._tcp.local.)
        "hue"
        "switchbot"
        "unifi"
        "tplink"
        "brother"
        "ipp"

        # Climate / thermostats
        "tado"

        # Optional but common
        "bluetooth"
        "bluetooth_adapters"
        "ibeacon"
        "homekit_controller"
        "matter"
      ]
      ++ lib.optional haCfg.prometheus.enable "prometheus"
      ++ lib.optional obsCfg.host.enable "influxdb";
      config = {
        # default_config = { }; # this breaks metrics collection
        home = {
          name = "Home";
          latitude = 48.1351;
          longitude = 11.5820;
          elevation = 520;
          unit_system = "metric";
          currency = "EUR";
        };
        frontend = { };
        # Automations are declared in ./services.home-assistant.automations.nix.
        http = {
          server_port = haPort;
          use_x_forwarded_for = true;
          trusted_proxies = [
            "127.0.0.1"
            "::1"
            "10.0.0.0/8"
            "172.16.0.0/12"
            "192.168.0.0/16"
            "fd00::/8"
            "fe80::/10"
          ];
        };
        group = { };
      }
      // lib.optionalAttrs haCfg.prometheus.enable {
        # https://www.home-assistant.io/integrations/prometheus/
        # Endpoint: GET /api/prometheus (requires Bearer auth)
        prometheus = {
          namespace = haCfg.prometheus.namespace;
        };
      }
      // lib.optionalAttrs obsCfg.host.enable {
        # https://www.home-assistant.io/integrations/influxdb/
        #
        # As of HA 2026.9.0 the InfluxDB connection settings (api_version,
        # host, port, ssl, verify_ssl, ssl_ca_cert, username, password,
        # database, token, organization, bucket, path) are no longer
        # configurable via YAML — they live in the UI config entry, which
        # HA auto-imports from the previous YAML on first startup. Only the
        # filter knobs (include, exclude, tags, …) remain in YAML.
        #
        # The connection points at VictoriaMetrics' InfluxDB v1 write API
        # at /write (HTTP Basic Auth, api_version 1) and is configured in
        # the HA UI under Settings → Devices & services → InfluxDB.
        influxdb = {
          exclude = {
            domains = [
              "binary_sensor"
              "device_tracker"
              "group"
              "input_boolean"
              "input_text"
              "persistent_notification"
              "scene"
              "script"
              "update"
              "zone"
            ];
          };
        };
      };
    };

    # Append a scrape job to the local vmagent (if observability.client
    # is enabled). vmagent reads `Authorization: Bearer <token>` from
    # `bearer_token_file`. The token is a Home Assistant Long-Lived
    # Access Token created in the HA UI.
    #
    # The vmagent NixOS module sets `DynamicUser=true`, which means the
    # `vmagent` user/group only exist while the unit is running — so
    # the `chown vmagent:vmagent` agenix would run during the boot-time
    # activation script silently fails (the user doesn't exist yet),
    # leaving the decrypted file as `root:root 0400` and unreadable by
    # vmagent. To dodge that, the agenix secret stays at its default
    # location with default ownership, and systemd materialises a copy
    # in the unit's credentials directory via `LoadCredential=` — which
    # systemd reads as root and makes available to the dynamic user.
    services.vmagent.prometheusConfig.scrape_configs =
      lib.mkIf (haCfg.prometheus.enable && config.services.vmagent.enable)
        [
          {
            job_name = "home-assistant";
            metrics_path = "/api/prometheus";
            scrape_interval = haCfg.prometheus.scrapeInterval;
            # systemd materialises the credential at this path inside
            # the unit's runtime (a tmpfs only readable by the unit's
            # user). See `LoadCredential=` below.
            bearer_token_file = "/run/credentials/vmagent.service/hass_bearer_token";
            static_configs = [
              { targets = [ "127.0.0.1:${toString haPort}" ]; }
            ];
          }
        ];

    # Gate on `age.secrets.hass_bearer_token_file` actually existing —
    # the secret is only materialised when the private overlay
    # provides `source = ...` (see `myconfig.secrets.nix`). On hosts
    # that build the public flake without the overlay (e.g. CI), the
    # secret is filtered out and there's no file to load.
    systemd.services.vmagent.serviceConfig.LoadCredential =
      lib.mkIf
        (
          haCfg.prometheus.enable
          && config.services.vmagent.enable
          && (config.age.secrets ? hass_bearer_token_file)
        )
        [
          # systemd reads the source as root (which can read the
          # `root:root 0400` agenix file), then makes a copy available
          # to the unit's (dynamic) user under `%d/hass_bearer_token`.
          "hass_bearer_token:${config.age.secrets.hass_bearer_token_file.path}"
        ];

    networking.firewall.allowedTCPPorts = lib.mkIf haCfg.exposeOnLan [ haPort ];

    systemd.services.home-assistant = {
      serviceConfig = {
        PrivateDevices = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = "/var/lib/home-assistant";
      };
    };

    users.groups.homeassistant = { };
    users.users.homeassistant = {
      group = "homeassistant";
      home = "/var/lib/home-assistant";
      isSystemUser = true;
      createHome = true;
    };
    # Declare the bearer-token secret slot. The actual `source = ...`
    # is provided by the private overlay (`priv/hosts/host.nuc/`),
    # which holds the encrypted token. Owner/permissions stay at the
    # agenix defaults (`root:root 0400`) — vmagent reads it indirectly
    # via the `LoadCredential=` hop above rather than reading the
    # decrypted file directly, so the file does not need to be owned
    # by the (dynamic) `vmagent` user.
    myconfig.secrets = lib.mkIf (haCfg.prometheus.enable && config.services.vmagent.enable) {
      "hass_bearer_token_file" = { };
    };
  };
}
