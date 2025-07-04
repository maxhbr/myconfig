{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
  signal-desktop = pkgs.nixos-unstable.signal-desktop;
  signal-cli = pkgs.nixos-unstable.signal-cli;
in {
  options.myconfig = with lib; {
    desktop.messengers.enable = mkEnableOption "myconfig.desktop.messengers";
  };
  config = lib.mkIf cfg.desktop.messengers.enable {
    home-manager.sharedModules = [{
      home.packages = [ signal-desktop signal-cli pkgs.smile ];
      myconfig.persistence.directories = [ ".config/Signal" ];
    }];
    myconfig.desktop.wayland.launcherCommands = [ "signal-desktop" ];
  };
}
