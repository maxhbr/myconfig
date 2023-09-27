# Copyright 2023 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, myconfig, lib, ... }: {
  config = {
    home-manager.sharedModules = [{
      programs.man = {
        enable = lib.mkForce false;
        generateCaches = true;
      };
    }];
  };
}
