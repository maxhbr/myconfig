# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    services.udev.packages = [
      pkgs.openhantek6022
      pkgs.saleae-logic
    ];
    home-manager.sharedModules = [
      {
        home.packages = [
          pkgs.openhantek6022
          # pkgs.pulseview
          pkgs.saleae-logic
        ];
      }
    ];
  };
}
