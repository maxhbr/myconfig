# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./X
  ];

  config = {
    services.xserver.desktopManager.xfce.enable = true;
  };
}
