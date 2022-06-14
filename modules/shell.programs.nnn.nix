# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, myconfig, ... }:
{
  config = {
    home-manager.sharedModules = [{
      programs.nnn.enable = true;
    }];
  };
}
