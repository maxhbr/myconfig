# Copyright 2017 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }: let
  mute_mic = pkgs.writeShellScriptBin "mute_mic" ''
set -euo pipefail

lastStateFile="/tmp/$(basename "$0")-lastState"
source="''${1:-@DEFAULT_AUDIO_SOURCE@}"

MUTED="MUTED"
UNMUTED="UNMUTED"

# blink1-tool --rgb 0xff,0xff,0x00 --blink 1 &>/dev/null || true &

updateCurrentStatus() {
    if ${pkgs.wireplumber}/bin/wpctl get-volume "$source" | grep -q '\[MUTED\]'; then
        echo "$MUTED" | tee "$lastStateFile"
        ${pkgs.blink1-tool}/bin/blink1-tool --rgb '#770000' &>/dev/null || true &
        echo 1 > /sys/class/leds/platform::micmute/brightness
    else
        echo "$UNMUTED" | tee "$lastStateFile"
        ${pkgs.blink1-tool}/bin/blink1-tool --rgb '#007700' &>/dev/null || true &
        echo 0 > /sys/class/leds/platform::micmute/brightness
    fi
}

toggleMute() (
    set -x
    ${pkgs.wireplumber}/bin/wpctl set-mute "$source" toggle
)

if [[ "$#" -gt 0 && "$1" == "--print-status" ]]; then
    shift
else
    toggleMute
fi
updateCurrentStatus
'';
in {
  config = (lib.mkIf config.services.pipewire.wireplumber.enable {
    home-manager.sharedModules = [{
      home.packages = [mute_mic];
      xdg.configFile = {
        "wireplumber/bluetooth.lua.d/50-bluez_monitor.lua".text = ''
          bluez_monitor.properties = {
          	["bluez5.enable-sbc-xq"] = true,
          	["bluez5.enable-msbc"] = true,
          	["bluez5.enable-hw-volume"] = true,
          	["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]"
          }
        '';
      };
    }];
  });
}
