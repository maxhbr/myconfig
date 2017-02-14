{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    docker
    python35Packages.docker_compose
  ];

  virtualisation.docker = {
      enable = true;
      extraOptions = "-g /home/docker";
      storageDriver = "overlay2";
  };

  # users.extraGroups.docker.members = [ "mhuber" ];
}
