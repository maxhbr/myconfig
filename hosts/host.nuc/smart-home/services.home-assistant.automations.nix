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
          ];
        }
      ];
      "automation ui" = "!include automations.yaml";
    };
  };
}
