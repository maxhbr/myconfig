{ config, lib, pkgs, ... }:
{
  options = {
    myconfig.roles.clamav = {
      enable = lib.mkEnableOption "Clamav role";
    };
  };

  config = lib.mkIf config.myconfig.roles.clamav.enable {
    environment.systemPackages = with pkgs; [
      clamav
    ];
    services.clamav = {
      daemon.enable = true;
      updater.enable = true;
    };
  };
}
