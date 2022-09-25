# Copyright 2017 Maximilian nuber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }: {
  config = (lib.mkIf config.services.pipewire.wireplumber.enable {
    home-manager.sharedModules = [{
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
