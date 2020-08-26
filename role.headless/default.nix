# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../role.core
    # configuration
    ./service.monitoring.nix
    ./service.vsftp.nix
  ];

  config =
    { home-manager.users.mhuber =
        { home.packages = with pkgs; [vnstat]; };
      system.autoUpgrade.allowReboot = true;
      services.vnstat.enable = true;
    };
}
