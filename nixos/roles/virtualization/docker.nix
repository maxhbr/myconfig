{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.docker = {
      enable = lib.mkEnableOption "Docker role";
    };
  };

  config = lib.mkIf config.myconfig.roles.docker.enable {
    environment.systemPackages = with pkgs; [
      docker
      python35Packages.docker_compose
    ];

    virtualisation.docker = {
        enable = true;
        extraOptions = "-g /home/docker";
        storageDriver = "overlay2";
    };
  };
}
