{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    desktop.messengers.enable = mkEnableOption "myconfig.desktop.messengers";
  };
  config = lib.mkIf cfg.desktop.messengers.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          signal-desktop
          # signal-cli
          pkgs.smile
        ];
        myconfig.persistence.directories = [ ".config/Signal" ];
      }
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "signal-desktop"
      "smile"
    ];
  };
}
