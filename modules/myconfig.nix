{ config, lib, pkgs, ... }:

{
  options.myconfig = with lib; {
    # general configuration
    user = mkOption {
      type = types.str;
      default = "mhuber";
      example = "mhuber";
      description = ''
        The username of the main user
      '';
    };

    # desktop.enable = mkEnableOption "myconfig.desktop";
    # desktop.xmonad.enable = mkOption {
    #   default = config.myconfig.desktcop.enable;
    #   example = true;
    #   description = "Whether to enable myconfig.xmonad.desktop.";
    #   type = lib.types.bool;
    # };
    # headless.enable = mkEnableOption "headless.desktop";
  };
}
