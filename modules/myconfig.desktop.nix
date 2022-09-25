{ config, lib, pkgs, ... }:

let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.enable = mkEnableOption "myconfig.desktop";
    desktop.full = mkEnableOption "myconfig.desktop";
  };
  config = (lib.mkIf cfg.desktop.enable {
    myconfig.desktop.full = lib.mkDefault true;
    services.xserver.enable = true;
  });
}
