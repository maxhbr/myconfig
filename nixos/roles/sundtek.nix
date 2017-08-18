{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.sundtek = {
      enable = lib.mkEnableOption "Sundtek role";
    };
  };

  config = lib.mkIf config.myconfig.roles.sundtek.enable {
    environment.systemPackages = with pkgs; [
      sundtek
    ];
  };
}
