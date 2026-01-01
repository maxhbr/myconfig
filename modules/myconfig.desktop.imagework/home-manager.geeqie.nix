{pkgs, lib, config, ... }:
let
  cfg = config.myconfig.desktop.imagework.geeqie;
in  
{
  options.myconfig.desktop.imagework.geeqie.enable = lib.mkEnableOption "geeqie";
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ geeqie ];
    myconfig.persistence.files = [
      ".config/geeqie/geeqierc.xml"
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "geeqie"
    ];
  };
}