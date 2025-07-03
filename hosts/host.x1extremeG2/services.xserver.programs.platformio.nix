# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  config = {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          # platformio
        ];
      }
    ];
  };
}
