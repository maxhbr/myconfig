{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    desktop.logseq.enable = mkEnableOption "logseq";
  };
  config = lib.mkIf (cfg.desktop.enable && cfg.desktop.logseq.enable) {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs.master; [ logseq ];
        myconfig.persistence.directories = [
          ".config/Logseq"
        ];
      }
    ];
    myconfig.desktop.wayland.launcherCommands = [ "logseq" ];
  };
}
