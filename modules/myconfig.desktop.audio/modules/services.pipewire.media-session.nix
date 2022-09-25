# Copyright 2017 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  config = (lib.mkIf (config.services.pipewire.media-session.enable) {
    services.pipewire.media-session = {
      config.bluez-monitor.rules = [
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
