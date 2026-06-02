# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    ./services.deconz.nix
    ./services.mosquitto.nix
    ./services.zigbee2mqtt.nix
    ./services.home-assistant.nix
    ./services.home-assistant.automations.nix
    ./services.node-red.nix
  ];

  options.myconfig.smart-home = with lib; {
    enable = mkEnableOption "smart home things";
  };

  config = lib.mkIf config.myconfig.smart-home.enable {
    myconfig.smart-home = {
      home-assistant = {
        enable = true;
        exposeOnLan = true;
      };
      # Migrated from deCONZ/Phoscon to Zigbee2MQTT. Mosquitto is
      # auto-enabled by the zigbee2mqtt module. To roll back, flip
      # `zigbee2mqtt.enable` to `false` and set `deconz.enable = true`
      # (only one may hold /dev/ttyACM0 at a time).
      deconz.enable = false;
      zigbee2mqtt.enable = true;
      node-red.enable = true;
    };
  };
}
