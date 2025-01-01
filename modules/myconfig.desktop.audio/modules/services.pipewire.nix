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
  pw-record-all = pkgs.writeShellScriptBin "pw-record-all" ''
    set -euo pipefail
    ${pkgs.pipewire}/bin/pw-record -P '{ stream.capture.sink=true }' "$HOME/recording-$(date "+%F_%H-%M-%S").flac"
  '';

in {
  config = (lib.mkIf config.services.pipewire.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [ qjackctl pw-simultaneous pw-record-all ];
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

      # config.pipewire = {
      #   config.pipewire = {
      #     "context.properties" = {
      #       #"link.max-buffers" = 64;
      #       "link.max-buffers" =
      #         16; # version < 3 clients can't handle more than this
      #       "log.level" = 2; # https://docs.pipewire.org/page_daemon.html
      #       #"default.clock.rate" = 48000;
      #       #"default.clock.quantum" = 1024;
      #       #"default.clock.min-quantum" = 32;
      #       #"default.clock.max-quantum" = 8192;
      #     };
      #   };
      # };
    };
  });
}
