{
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    desktop.joplin-desktop.enable = mkEnableOption "joplin-desktop";
  };
  config = lib.mkIf (cfg.desktop.enable && cfg.desktop.joplin-desktop.enable) {
    home-manager.sharedModules = [
      {
        programs.joplin-desktop.enable = true;
        myconfig.persistence.directories = [
          ".config/Joplin"
          ".config/joplin-desktop"
        ];
      }
    ];
    myconfig.desktop.wayland.launcherCommands = [ "joplin-desktop" ];
  };
}
