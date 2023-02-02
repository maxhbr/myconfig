# Copyright 2017 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

let
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
  config = (lib.mkIf config.services.pipewire.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [ qjackctl pw-simultaneous ]
        ++ lib.optionals config.myconfig.desktop.full [ # helvum easyeffects
        ];
    }];
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      wireplumber.enable = true;

      config.pipewire = {
        config.pipewire = {
          "context.properties" = {
            #"link.max-buffers" = 64;
            "link.max-buffers" =
              16; # version < 3 clients can't handle more than this
            "log.level" = 2; # https://docs.pipewire.org/page_daemon.html
            #"default.clock.rate" = 48000;
            #"default.clock.quantum" = 1024;
            #"default.clock.min-quantum" = 32;
            #"default.clock.max-quantum" = 8192;
          };
        };
        # "context.modules" = [{
        #   name = "libpipewire-module-echo-cancel";
        #   #   args = { };
        # }];
        # "context.objects" = [
        #   {
        #     # A default dummy driver. This handles nodes
        #     # marked with the "node.always-driver"
        #     # properyty when no other driver is
        #     # currently active. JACK clients need
        #     # this.
        #     factory = "spa-node-factory";
        #     args = {
        #       "factory.name" = "support.node.driver";
        #       "node.name" = "Dummy-Driver";
        #       "priority.driver" = 8000;
        #     };
        #   }
        #   {
        #     factory = "adapter";
        #     args = {
        #       "factory.name" = "support.null-audio-sink";
        #       "node.name" = "Microphone-Proxy";
        #       "node.description" = "Microphone";
        #       "media.class" = "Audio/Source/Virtual";
        #       "audio.position" = "MONO";
        #     };
        #   }
        #   {
        #     factory = "adapter";
        #     args = {
        #       "factory.name" = "support.null-audio-sink";
        #       "node.name" = "Main-Output-Proxy";
        #       "node.description" = "Main Output";
        #       "media.class" = "Audio/Sink";
        #       "audio.position" = "FL,FR";
        #     };
        #   }
        # ];
      };
    };
  });
}
