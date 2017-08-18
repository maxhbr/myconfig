{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.wine = {
      enable = lib.mkEnableOption "Wine role";
    };
  };

  config = lib.mkIf config.myconfig.roles.wine.enable {
    environment.systemPackages = with pkgs; [
      wineStaging
      winetricks
    ];
  };
}
