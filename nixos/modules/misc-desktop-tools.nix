# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        libreoffice
        zoom-us bluejeans-gui
        slack-dark tdesktop
      ];
    };
  };
}
