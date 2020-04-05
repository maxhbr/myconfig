# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./core.nix
    # configuration
    ./modules/nixos.auto-upgrade.nix
    ./modules/service.openssh.nix
    ./modules/service.vsftp.nix
  ];

  config = {
    system.autoUpgrade.allowReboot = true;
  };
}
