{ config, lib, pkgs, ... }:

let
  cfg = config.myconfig;
  user = cfg.user;
in {
  options.myconfig = with lib; {
    desktop.enable = mkEnableOption "myconfig.desktop";
    # desktop.xmonad.enable = mkOption {
    #   default = config.myconfig.desktcop.enable;
    #   example = true;
    #   description = "Whether to enable myconfig.xmonad.desktop.";
    #   type = lib.types.bool;
    # };
  };
  config = (lib.mkIf cfg.desktop.enable {
    services.xserver.enable = true;
    home-manager.users."${user}" = {
      programs.firefox.enable = lib.mkDefault true;
      programs.zathura.enable = lib.mkDefault true;
    };
  });
}
