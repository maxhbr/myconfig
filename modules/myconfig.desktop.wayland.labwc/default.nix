# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.wayland.labwc = {
      enable = mkEnableOption "labwc";
    };
  };
  config = (lib.mkIf
    (cfg.desktop.wayland.enable && cfg.desktop.wayland.labwc.enable) {
      home-manager.sharedModules = [{
        xdg.configFile = {
          "labwc/rc.xml".source = ./labwc/rc.xml;
          "labwc/autostart".text = ''
# swaybg -i foo.png >/dev/null 2>&1 &
${pkgs.waybar}/bin/waybar >/dev/null 2>&1 &
'';
          "labwc/environment".text = ''
XKB_DEFAULT_LAYOUT=${config.environment.sessionVariables."XKB_DEFAULT_LAYOUT"}
XKB_DEFAULT_VARIANT=${config.environment.sessionVariables."XKB_DEFAULT_VARIANT"}
'';
          "labwc/menu.xml".source = ./labwc/menu.xml;
        };
        home.packages = [ pkgs.labwc ];
      }];
    });
}
