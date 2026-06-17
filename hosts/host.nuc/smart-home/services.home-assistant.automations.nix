# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Declarative Home Assistant automations.
#
# This file contributes to `services.home-assistant.config` via the
# NixOS module merge. HA's `automation:` key only accepts a single
# value, so to keep both Nix-declared *and* UI-edited automations
# working we use HA's labelled-merge convention: any top-level key
# starting with `automation` is picked up by the automation
# integration and the lists are merged. See:
#   https://www.home-assistant.io/docs/automation/editor/#troubleshooting-missing-automations
#
# - `automation manual` — declared here in Nix, version-controlled.
# - `automation ui`     — `!include` of the UI-managed file at
#                         `${configDir}/automations.yaml`. The leading
#                         `!` is preserved by the nixpkgs YAML
#                         renderer's post-processing (see
#                         `renderYAMLFile` in nixpkgs'
#                         home-assistant module — it converts
#                         `'!foo bar'` back into the bare YAML tag
#                         `!foo bar`).
{
  config,
  lib,
  ...
}:
let
  haCfg = config.myconfig.smart-home.home-assistant;

  # Zigbee2MQTT-discovered Schreibtisch button. `device_id` is HA's
  # internal UUID for the button device; stable across restarts but
  # changes if the device is removed and re-paired. Pull it out into a
  # `let` binding so multiple automations targeting the same button
  # only need updating in one place.
  schreibtischButtonDeviceId = "b5ce603d4259ece94bb8653ca5dc131b";
  shellySchreibtisch = "switch.shelly_schreibtisch";
  shellyTV = "01KRH9B9KHKGJ4WGNFQ0R6FMYH";

in
{
  config = lib.mkIf haCfg.enable {
    services.home-assistant.config = {
      "automation manual" = [
        {
          id = "schreibtisch_button_single_turn_on_shelly";
          alias = "Schreibtisch Button Single -> On";
          mode = "single";
          # Trigger copied from the HA UI's "Edit in YAML" for the
          # Zigbee2MQTT-discovered button.
          triggers = [
            {
              trigger = "device";
              domain = "mqtt";
              device_id = schreibtischButtonDeviceId;
              type = "action";
              subtype = "single";
            }
          ];
          conditions = [ ];
          # Use a plain service call rather than the UI's device
          # action — it's readable and survives entity-registry UUID
          # churn (the UI form references entities by their internal
          # UUID, not their entity_id).
          actions = [
            {
              action = "switch.turn_on";
              target.entity_id = shellySchreibtisch;
            }
          ];
        }
        {
          id = "schreibtisch_button_long_turn_off_shelly";
          alias = "Schreibtisch Button Long -> Off";
          mode = "single";
          # Long-press on the same Zigbee2MQTT-discovered button.
          # Z2M maps "hold" to the device-trigger subtype `long`
          # (see the device's `action` enum in the Z2M frontend —
          # `single`, `double`, `long`, …).
          triggers = [
            {
              trigger = "device";
              domain = "mqtt";
              device_id = schreibtischButtonDeviceId;
              type = "action";
              subtype = "long";
            }
          ];
          conditions = [ ];
          actions = [
            {
              action = "switch.turn_off";
              target.entity_id = shellySchreibtisch;
            }
            {
              type = "turn_off";
              device_id = "92d93babffd096fb5f398af883b6ee2c";
              entity_id = "5d2126fae9ea142ee7152d6e3b5c3c63";
              domain = "light";
            }
          ];
        }
        {
          id = "vibration_sensor_turn_on_light";
          alias = "Vibration -> Light On";
          mode = "single";
          triggers = [
            {
              trigger = "device";
              domain = "binary_sensor";
              device_id = "ee7fefee50ee9dec379e57eb1986d7cf";
              entity_id = "b09c35791298c879bb5fc163b66cca3d";
              type = "vibration";
              metadata = {
                secondary = false;
              };
            }
          ];
          conditions = [ ];
          actions = [
            {
              type = "turn_on";
              device_id = "c775cf1cf67f40ad8864c8f17de5cdf0";
              entity_id = "95436ab5cf3d9360079e61f68c0515e3";
              domain = "light";
            }
          ];
        }
        {
          id = "tv_on_if_occupied";
          alias = "TV on if occupied";
          description = "TV on if occupied";
          mode = "single";
          triggers = [
            {
              trigger = "device";
              domain = "binary_sensor";
              device_id = "a766b7b2420015a3c7b51f9e6dc03c47";
              entity_id = "83b9f62ae68ea5f5e5723d04c653e20c";
              type = "occupied";
              metadata = {
                secondary = false;
              };
            }
          ];
          conditions = [ ];
          actions = [
            {
              type = "turn_on";
              device_id = "cd95b4bafcba5175d7da2de59fcaf278";
              entity_id = "9c5be7577f2bebabc1b6a36b22cfedd9";
              domain = "switch";
            }
          ];
        }
        {
          id = "start_tv";
          alias = "Start TV";
          description = "Start TV";
          mode = "single";
          triggers = [
            {
              trigger = "device";
              domain = "mqtt";
              device_id = "b6c71e3e8e32a626000c4b2d02b92701";
              type = "action";
              subtype = "single";
              metadata = { };
            }
          ];
          conditions = [ ];
          actions = [
            {
              type = "turn_on";
              device_id = "cd95b4bafcba5175d7da2de59fcaf278";
              entity_id = "9c5be7577f2bebabc1b6a36b22cfedd9";
              domain = "switch";
            }
            {
              type = "turn_off";
              device_id = "92d93babffd096fb5f398af883b6ee2c";
              entity_id = "5d2126fae9ea142ee7152d6e3b5c3c63";
              domain = "light";
            }
            {
              type = "turn_off";
              device_id = "0e33acb12394f87c8a0bb8196b7e264e";
              entity_id = "a5d5cce4128eadb1c5a351e16ff0675e";
              domain = "light";
            }
            {
              type = "turn_off";
              device_id = "95615f6f124bdb91c579209b3ffb3b9a";
              entity_id = "84cc62db8aa3a40c2f746f4fe6cdfb0b";
              domain = "light";
            }
            {
              type = "brightness_decrease";
              device_id = "b26f6249db2a8bca092185068d1a49e9";
              entity_id = "c0bd08fd366f92f11003dc5327fbd852";
              domain = "light";
            }
            {
              type = "turn_off";
              device_id = "2bc1aacf480c2db99dadd56c15febd98";
              entity_id = "1e8c296f6dc0013bd587b4df2340b842";
              domain = "light";
            }
          ];
        }
        {
          id = "stop_tv";
          alias = "Stop TV";
          description = "Stop TV";
          mode = "single";
          triggers = [
            {
              trigger = "device";
              domain = "mqtt";
              device_id = "b6c71e3e8e32a626000c4b2d02b92701";
              type = "action";
              subtype = "long";
            }
          ];
          conditions = [ ];
          actions = [
            {
              type = "turn_off";
              device_id = "cd95b4bafcba5175d7da2de59fcaf278";
              entity_id = "9c5be7577f2bebabc1b6a36b22cfedd9";
              domain = "switch";
            }
            {
              type = "turn_off";
              device_id = "b26f6249db2a8bca092185068d1a49e9";
              entity_id = "c0bd08fd366f92f11003dc5327fbd852";
              domain = "light";
            }
            {
              type = "turn_off";
              device_id = "18249ff79b00be7c1494666d5263948d";
              entity_id = "c7d67b1de43e79890ff9a7655bdda1a5";
              domain = "light";
            }
          ];
        }
        {
          id = "movement_schreibtisch_an";
          alias = "Movement -> Schreibtisch an";
          description = "Movement -> Schreibtisch an";
          mode = "single";
          triggers = [
            {
              trigger = "device";
              type = "occupied";
              device_id = "07e1effd9653e7b1acf05fc636072598";
              entity_id = "2268c8e0376953ed64c404bf5241bac0";
              domain = "binary_sensor";
              metadata = {
                secondary = false;
              };
            }
          ];
          conditions = [ ];
          actions = [
            {
              type = "turn_on";
              device_id = "d668f489803490e725f296b1284aa762";
              entity_id = "884a914bd6170fa1c25bd11c946fa6f7";
              domain = "switch";
            }
          ];
        }
        {
          id = "boxen_on";
          alias = "Boxen on";
          description = "Boxen on";
          mode = "single";
          triggers = [
            {
              trigger = "device";
              domain = "mqtt";
              device_id = "b6c71e3e8e32a626000c4b2d02b92701";
              type = "action";
              subtype = "double";
            }
          ];
          conditions = [ ];
          actions = [
            {
              type = "turn_on";
              device_id = "18249ff79b00be7c1494666d5263948d";
              entity_id = "c7d67b1de43e79890ff9a7655bdda1a5";
              domain = "light";
            }
          ];
        }
      ];
      "automation ui" = "!include automations.yaml";
    };
  };
}
