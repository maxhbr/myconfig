# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig.dev.rust;
in {
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [ rustup ];
      # home.packages = with pkgs; [ rustc cargo cargo-generate ];
    }];
  };
}

