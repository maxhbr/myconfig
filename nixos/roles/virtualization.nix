{ config, pkgs, lib, ... }:

{
  options = {
    myconfig.roles.virtualization = {
      enable = lib.mkEnableOption "Virtualization role";
    };
  };

  config = lib.mkIf config.myconfig.roles.virtualization.enable {
    environment.systemPackages = with pkgs; [
      docker
      python35Packages.docker_compose
      vagrant
    ];

    virtualisation.docker = {
        enable = true;
        extraOptions = "-g /home/docker";
        storageDriver = "overlay2";
    };

    virtualisation.virtualbox.host.enable = true;
  };
}
