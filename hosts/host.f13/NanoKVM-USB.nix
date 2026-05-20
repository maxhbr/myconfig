# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, inputs, ... }:
{
  home-manager.sharedModules = [
    {
      home.packages = [
        inputs.NanoKVM-USB.packages.x86_64-linux.desktop
      ];
    }
  ];

  myconfig.desktop.wayland.launcherCommands = [
    "nanokvm-usb"
  ];
}
