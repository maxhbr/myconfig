{ config, lib, pkgs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.wayland.gnome = { enable = mkEnableOption "gnome"; };
  };
  config = lib.mkIf (cfg.desktop.wayland.enable
    && cfg.desktop.wayland.gnome.enable) {
      services.xserver.desktopManager.gnome.enable = true;
      home-manager.sharedModules = [{
        home.packages = with pkgs.gnomeExtensions; [
          true-color-invert
          miniview
          material-shell
        ];
      }];
    };
}
