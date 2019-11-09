# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
  ];

  options = {
  };

  config = {
    environment.systemPackages = with pkgs; [
      docker
      docker-machine
      # docker-gc
      docker_compose
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
