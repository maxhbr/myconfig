# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.myconfig.smart-home.mosquitto;
in
{
  options.myconfig.smart-home.mosquitto = with lib; {
    enable = mkEnableOption "local Mosquitto MQTT broker for Zigbee2MQTT/Home Assistant";

    port = mkOption {
      type = types.port;
      default = 1883;
      description = ''
        TCP port the local MQTT broker listens on. Only bound to
        127.0.0.1, so this port is never exposed to the LAN.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Local-only broker shared by Home Assistant (`mqtt` integration)
    # and Zigbee2MQTT. Bound to 127.0.0.1, no authentication: only
    # processes on this host can connect, and the firewall is not
    # touched.
    services.mosquitto = {
      enable = true;
      listeners = [
        {
          address = "127.0.0.1";
          port = cfg.port;
          omitPasswordAuth = true;
          settings.allow_anonymous = true;
          acl = [ "topic readwrite #" ];
        }
      ];
    };
  };
}
