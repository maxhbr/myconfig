# Copyright 2023 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, myconfig, ... }: {
  config = { 
    home-manager.sharedModules = [{
      programs.man = {
        enable = true;
        generateCaches = true;
      };
    }];
  };
}