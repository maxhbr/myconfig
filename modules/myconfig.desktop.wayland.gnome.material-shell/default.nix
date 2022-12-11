{ config, lib, pkgs, ... }:
let cfg = config.myconfig;
in {
  config = lib.mkIf (cfg.desktop.wayland.enable
    && config.services.xserver.desktopManager.gnome.enable) {
      home-manager.sharedModules = [{
        home.packages = with pkgs.gnomeExtensions; [
          true-color-invert
          miniview
          material-shell
        ];
      }];
    };
}
