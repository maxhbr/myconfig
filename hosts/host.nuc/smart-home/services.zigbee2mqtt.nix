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
      default = "/dev/serial/by-id/usb-SMLIGHT_SMLIGHT_SLZB-07p7_6878f140e8d6ef119f1a50878f302768-if00-port0";
      description = ''
        Serial device for the Zigbee coordinator. Default targets the
        SMLIGHT SLZB-07p7 (CC2652P7 + CP2102N) via its stable
        `/dev/serial/by-id/...` symlink so the path survives reboots
        and USB re-enumeration. Adjust the suffix if the dongle exposes
        a different serial number — check
        `ls -l /dev/serial/by-id/` after plugging it in.
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
      default = "zstack";
      description = ''
        Zigbee2MQTT adapter driver. `zstack` matches Texas Instruments
        CC2652/CC1352-based coordinators such as the SMLIGHT SLZB-07p7
        (CC2652P7). Use `deconz` for ConBee II / RaspBee II.
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

    panId = mkOption {
      type =
        with types;
        oneOf [
          ints.u16
          (strMatching "GENERATE")
        ];
      default = "GENERATE";
      description = ''
        16-bit Zigbee PAN ID as a decimal integer, or the sentinel
        string `"GENERATE"` to ask Z2M to pick a random value on first
        start.

        `"GENERATE"` is unsafe across rebuilds in this module because
        the upstream NixOS service's `ExecStartPre` copies the
        Nix-generated YAML over the persisted one on every start — so
        on the second boot Z2M re-generates a *different* value that
        no longer matches what the coordinator already committed to
        NVRAM, and refuses to start with a "configuration is not
        consistent with adapter state/backup" error.

        For any host with paired devices, pin this to the adapter's
        actual PAN ID (see `pan_id` in
        `/var/lib/zigbee2mqtt/coordinator_backup.json` — it's stored
        there as a hex string and must be converted to decimal). To
        re-commission the network from scratch, delete
        `coordinator_backup.json` and `database.db` from the Z2M data
        directory, leave this at `"GENERATE"` for the first boot,
        then read the freshly chosen value back out of the backup
        and pin it here.
      '';
    };

    extPanId = mkOption {
      type =
        with types;
        oneOf [
          (listOf ints.u8)
          (strMatching "GENERATE")
        ];
      default = "GENERATE";
      description = ''
        64-bit extended Zigbee PAN ID as an 8-element list of bytes
        (each 0..255), or the sentinel string `"GENERATE"`. See
        `panId` for why `"GENERATE"` is unsafe across rebuilds. Pin
        to the `extended_pan_id` field of
        `/var/lib/zigbee2mqtt/coordinator_backup.json`, converted
        from a hex string to a byte list (e.g. hex `bc2b5b...` →
        `[ 188 43 91 ... ]`).
      '';
    };

    networkKey = mkOption {
      type =
        with types;
        oneOf [
          (listOf ints.u8)
          (strMatching "GENERATE")
        ];
      default = "GENERATE";
      description = ''
        128-bit Zigbee network (master) key as a 16-element list of
        bytes (each 0..255), or the sentinel string `"GENERATE"`. See
        `panId` for why `"GENERATE"` is unsafe across rebuilds.

        This is the master key for the entire Zigbee mesh; treat it
        as a secret. Keep the actual value out of the public repo —
        set it from a private overlay (e.g.
        `priv/hosts/host.nuc/smart-home.nix`) rather than inline
        here.
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
          # See the `panId` / `extPanId` / `networkKey` option
          # descriptions for why these must be pinned (not
          # `"GENERATE"`) once the network is commissioned.
          network_key = cfg.networkKey;
          pan_id = cfg.panId;
          ext_pan_id = cfg.extPanId;
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
