# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig.dev.ruby;
in {
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [{
      packages = with pkgs; [ruby];
    }];
  };
}
