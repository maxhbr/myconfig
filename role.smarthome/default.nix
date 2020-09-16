# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [
    ../role.headless
    ../role.dev/virtualization.docker
    # ./docker.openhab.nix
    ./service.deconz.nix
    ./service.nextcloud.nix
    ./service.homeassistant.nix
  ];
}
