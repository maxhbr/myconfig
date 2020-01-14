# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        docker_compose
      ];
      home.file = {
        "bin/docker" = {
          source = ./bin;
          recursive = true;
        };
        "dockerfiles" = {
          source = ./bin;
          recursive = true;
        };
      };
    };
    environment.systemPackages = with pkgs; [
      docker
      docker-machine
    ];

    virtualisation.docker = {
        enable = true;
        extraOptions = "--data-root /home/docker";
        storageDriver = "overlay2";
        # socketActivation = false;
        autoPrune.enable = true;
        package = pkgs.docker-edge;
    };
  };
}
