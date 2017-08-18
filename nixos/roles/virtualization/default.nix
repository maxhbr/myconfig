{ config, pkgs, lib, ... }:

{
  imports = [
    ./docker.nix
    ./vbox.nix
  ];

  options = {
    myconfig.roles.virtualization = {
      enable = lib.mkEnableOption "Virtualization role";
    };
  };

  config = lib.mkIf config.myconfig.roles.virtualization.enable {
    myconfig.roles.docker.enable = true;
    myconfig.roles.vbox.enable = true;
  };
}
