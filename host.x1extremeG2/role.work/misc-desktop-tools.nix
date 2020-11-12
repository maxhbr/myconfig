# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
let user = config.myconfig.user;
in {
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        libreoffice
        nixos-unstable-small.zoom-us
        bluejeans-gui
        slack
        rambox
        remmina
      ];
    };
  };
}
