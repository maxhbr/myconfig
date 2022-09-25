# Copyright 2017 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
    {
      config = (lib.mkIf config.services.pipewire.wireplumber.enable {
        home-manager.sharedModules = [
          { home.packages = with pkgs; [ pavucontrol pamix ]; }
          {
            home.file = {
              ".config/wireplumber/bluetooth.lua.d".text =
                "	bluez_monitor.properties = {\n		[\"bluez5.enable-sbc-xq\"] = true,\n		[\"bluez5.enable-msbc\"] = true,\n		[\"bluez5.enable-hw-volume\"] = true,\n		[\"bluez5.headset-roles\"] = \"[ hsp_hs hsp_ag hfp_hf hfp_ag ]\"\n	}\n";
            };
          }
        ];
      });
    }
