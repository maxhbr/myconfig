{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    vagrant
    docker
    python35Packages.docker_compose
  ];

  virtualisation = {
    docker = {
        enable = true;
        extraOptions = "-g /home/docker";
        storageDriver = "overlay2";
    };
    virtualbox.host.enable = true;
  };

  users.extraGroups = {
    vboxusers.members = [ "mhuber" ];
    docker.members = [ "mhuber" ];
  };
}
