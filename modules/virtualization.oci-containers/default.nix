# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  enablePodman = config.virtualisation.podman.enable;
  enableDocker = config.virtualisation.docker.enable;
in {
  imports = [ ./podman.nix ./docker.nix ];
  config = (lib.mkIf (config.virtualisation.podman.enable
    || config.virtualisation.docker.enable) {
      # virtualisation.podman.dockerCompat = !config.virtualisation.docker.enable;
      home-manager.sharedModules =
        [{ home.packages = with pkgs; [ buildkit ]; }];
    });
}
