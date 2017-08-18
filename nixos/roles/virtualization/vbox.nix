{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.vbox = {
      enable = lib.mkEnableOption "Vbox role";
    };
  };

  config = lib.mkIf config.myconfig.roles.vbox.enable {
    environment.systemPackages = with pkgs; [
      vagrant
    ];

    virtualisation.virtualbox.host.enable = true;
  };
}
