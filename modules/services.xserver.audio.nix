# Copyright 2017 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
let
  pactl-monitor = pkgs.writeShellScriptBin "pactl-monitor" ''
    set -e
    pactl_monitor_file=/tmp/.pactl_monitor_file
    if [[ ! -f $pactl_monitor_file ]]; then
      touch $pactl_monitor_file
      ${config.hardware.pulseaudio.package}/bin/pactl load-module module-loopback latency_msec=1
      echo "enable loopback"
    else
      rm $pactl_monitor_file
      ${config.hardware.pulseaudio.package}/bin/pactl unload-module module-loopback || true
      echo "disable disable"
    fi
  '';
  pw-simultaneous = pkgs.writeShellScriptBin "pw-simultaneous" ''
    set -euo pipefail

    outputs="$( pw-link -io )"

    if echo "$outputs" | grep --quiet "Simultaneous:"; then
        echo "Simultaneous sink already exists"
    else
        ${pkgs.pulseaudio}/bin/pactl load-module module-null-sink media.class=Audio/Sink sink_name=Simultaneous channel_map=stereo
    fi

    echo "$outputs" | while read -r line; do
        if [[ "$line" != *"Simultaneous"* ]]; then
            if [[ "$line" == *"playback_FL" ]] ; then
                ${pkgs.pipewire}/bin/pw-link Simultaneous:monitor_FL "$line" || true
            elif [[ "$line" == *"playback_FR" ]] ; then
                ${pkgs.pipewire}/bin/pw-link Simultaneous:monitor_FR "$line" || true
            fi
        fi
    done
  '';
in {
  home-manager.sharedModules =
    [{ home.packages = with pkgs; [ pavucontrol pamix ]; }];
  programs.noisetorch.enable = true;
  imports = [
    {
      config = (lib.mkIf config.hardware.pulseaudio.enable {
        home-manager.sharedModules =
          [{ home.packages = with pkgs; [ pactl-monitor ]; }];
        hardware.pulseaudio = {
          package = pkgs.pulseaudioFull;
          extraConfig =
            "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
        };
        nixpkgs.config.pulseaudio = true;
      });
    }
    {
      config = (lib.mkIf config.services.pipewire.enable {
        home-manager.sharedModules =
          [{ home.packages = with pkgs; [ qjackctl helvum pw-simultaneous ]; }];
        hardware.pulseaudio.enable = false;
        security.rtkit.enable = true;
        services.pipewire = {

          # config.pipewire = {
          #   # "context.modules" = [{
          #   #   name = "libpipewire-module-echo-cancel";
          #   #   #   args = { };
          #   # }];
          #   # "context.objects" = [
          #   #   {
          #   #     # A default dummy driver. This handles nodes
          #   #     # marked with the "node.always-driver"
          #   #     # properyty when no other driver is
          #   #     # currently active. JACK clients need
          #   #     # this.
          #   #     factory = "spa-node-factory";
          #   #     args = {
          #   #       "factory.name" = "support.node.driver";
          #   #       "node.name" = "Dummy-Driver";
          #   #       "priority.driver" = 8000;
          #   #     };
          #   #   }
          #   #   {
          #   #     factory = "adapter";
          #   #     args = {
          #   #       "factory.name" = "support.null-audio-sink";
          #   #       "node.name" = "Microphone-Proxy";
          #   #       "node.description" = "Microphone";
          #   #       "media.class" = "Audio/Source/Virtual";
          #   #       "audio.position" = "MONO";
          #   #     };
          #   #   }
          #   #   {
          #   #     factory = "adapter";
          #   #     args = {
          #   #       "factory.name" = "support.null-audio-sink";
          #   #       "node.name" = "Main-Output-Proxy";
          #   #       "node.description" = "Main Output";
          #   #       "media.class" = "Audio/Sink";
          #   #       "audio.position" = "FL,FR";
          #   #     };
          #   #   }
          #   # ];
          # };
          alsa = {
            enable = true;
            support32Bit = true;
          };
          pulse.enable = true;
          media-session.config.bluez-monitor.rules = [
            {
              # Matches all cards
              matches = [{ "device.name" = "~bluez_card.*"; }];
              actions = {
                "update-props" = {
                  "bluez5.reconnect-profiles" =
                    [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
                  # mSBC is not expected to work on all headset + adapter combinations.
                  "bluez5.msbc-support" = true;
                  # SBC-XQ is not expected to work on all headset + adapter combinations.
                  "bluez5.sbc-xq-support" = true;
                };
              };
            }
            {
              matches = [
                { "node.name" = "~bluez_input.*"; }
                { "node.name" = "~bluez_output.*"; }
              ];
              actions = { "node.pause-on-idle" = false; };
            }
          ];
        };
      });
    }
  ];
}
