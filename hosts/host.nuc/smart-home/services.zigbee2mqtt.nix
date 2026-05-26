# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.myconfig.smart-home.zigbee2mqtt;
  mqttCfg = config.myconfig.smart-home.mosquitto;
in
{
  options.myconfig.smart-home.zigbee2mqtt = with lib; {
    enable = mkEnableOption "Zigbee2MQTT bridge (replaces deCONZ/Phoscon)";

    device = mkOption {
      type = types.str;
      default = "/dev/ttyACM0";
      description = ''
        Serial device for the Zigbee coordinator (ConBee II / RaspBee II).
        Make sure the kernel exposes the adapter at this path; if it
        moves between boots, pin it via a udev rule and reference the
        stable `/dev/serial/by-id/...` symlink here.
      '';
    };

    adapter = mkOption {
      type = types.enum [
        "deconz"
        "zstack"
        "ezsp"
        "zigate"
        "ember"
        "auto"
      ];
      default = "deconz";
      description = ''
        Zigbee2MQTT adapter driver. `deconz` matches ConBee II / RaspBee II.
      '';
    };

    port = mkOption {
      type = types.port;
      default = 8080;
      description = ''
        TCP port for the Zigbee2MQTT frontend (web UI). Not exposed to
        the LAN by default; reach it via the reverse proxy on `nas`.
      '';
    };

    exposeOnLan = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to open `port` in the local firewall so the Z2M frontend
        is reachable on the LAN without going through the reverse proxy.
      '';
    };

    permitJoin = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to allow new devices to pair at startup. Leave `false`
        in steady state; enable joining from the Z2M frontend when
        pairing a device.
      '';
    };

  };

  config = lib.mkIf cfg.enable {
    # Z2M relies on a local MQTT broker. Auto-enable Mosquitto unless
    # the user has explicitly turned it off (e.g. to point at a remote
    # broker via `services.zigbee2mqtt.settings.mqtt.server`).
    myconfig.smart-home.mosquitto.enable = lib.mkDefault true;

    services.zigbee2mqtt = {
      enable = true;
      settings = {
        # Version 2 of the on-disk settings schema; required by recent
        # Zigbee2MQTT releases.
        version = 2;

        homeassistant = {
          enabled = config.services.home-assistant.enable;
        };

        # Frontend (web UI). Bind to all interfaces; access control is
        # handled by the firewall + reverse proxy.
        frontend = {
          enabled = true;
          port = cfg.port;
          host = "0.0.0.0";
        };

        mqtt = {
          base_topic = "zigbee2mqtt";
          server = "mqtt://127.0.0.1:${toString mqttCfg.port}";
        };

        serial = {
          port = cfg.device;
          adapter = cfg.adapter;
        };

        advanced = {
          # Don't log to a file inside /var/lib/zigbee2mqtt; journald
          # captures stdout and rotates it for us.
          log_output = [ "console" ];
          # Generate a fresh network key on first run if none is set.
          # Z2M will persist the generated key into its data dir.
          network_key = "GENERATE";
          pan_id = "GENERATE";
          ext_pan_id = "GENERATE";
        };

        # Device Availability — ping devices and report offline state
        # when they stop responding.
        # See: https://www.zigbee2mqtt.io/guide/configuration/device-availability.html
        availability = {
          enabled = true;
          active.timeout = 10;
          passive.timeout = 1500;
        };

        permit_join = cfg.permitJoin;
      };
    };

    # Make sure Z2M starts only after Mosquitto is up; otherwise it
    # spins retrying MQTT connections on every boot.
    systemd.services.zigbee2mqtt = {
      after = [ "mosquitto.service" ];
      requires = [ "mosquitto.service" ];
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.exposeOnLan [ cfg.port ];
  };
}
