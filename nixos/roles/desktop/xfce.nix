{ config, lib, pkgs, ... }:
let
  unstable = (import <unstable> {});
in {
  options = {
    myconfig.roles.xfce = {
      enable = lib.mkEnableOption "Xfce desktop environment";
    };
  };

  config = lib.mkIf config.myconfig.roles.xfce.enable {
    services.xserver.desktopManager.xfce.enable = true;
  };
}
