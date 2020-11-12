# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let user = config.myconfig.user;
in {
  imports = [
    ../role.core
    # configuration
    ./service.monitoring.nix
    ./service.vsftp.nix
  ];

  config = {
    home-manager.users."${user}" = { home.packages = with pkgs; [ vnstat ]; };
    system.autoUpgrade.allowReboot = true;
    services.vnstat.enable = true;
  };
}
