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
  haPort = config.services.home-assistant.config.http.server_port or 8123;
  obsClientCfg = config.myconfig.observability.client;
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
      package = pkgs.home-assistant;
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

        # Media / cameras / streams
        "ffmpeg"
        "stream"
        "dlna_dmr"
        "cast"

        # Device ecosystems
        "deconz"
        "hue"
        "switchbot"
        "unifi"
        "tplink"
        "brother"
        "ipp"

        # Optional but common
        "bluetooth"
        "bluetooth_adapters"
        "ibeacon"
        "homekit_controller"
        "matter"
      ]
      ++ lib.optional haCfg.prometheus.enable "prometheus";
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
        http = {
          base_url = "http://hass.nuc.wg0.maxhbr.local:${toString haPort}/";
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
      };
    };

    # Append a scrape job to the local vmagent (if observability.client
    # is enabled). vmagent reads `Authorization: Bearer <token>` from
    # `bearer_token_file`. The token is a Home Assistant Long-Lived
    # Access Token created in the HA UI; see `tokenFile` option above.
    services.vmagent.prometheusConfig.scrape_configs =
      lib.mkIf (haCfg.prometheus.enable && config.services.vmagent.enable)
        [
          {
            job_name = "home-assistant";
            metrics_path = "/api/prometheus";
            scrape_interval = haCfg.prometheus.scrapeInterval;
            bearer_token_file = toString config.myconfig.secrets."hass_bearer_token_file".dest;
            static_configs = [
              { targets = [ "127.0.0.1:${toString haPort}" ]; }
            ];
          }
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
    myconfig.secrets = lib.mkIf (haCfg.prometheus.enable && config.services.vmagent.enable) {
      "hass_bearer_token_file" = {
        dest = "/run/vmagent/hass_bearer_token_file";
        owner = "vmagent";
        group = "vmagent";
      };
    };
  };
}
