# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let openhantek6022 = pkgs.libsForQt5.callPackage ./openhantek6022.nix { };
in {
  config = {
    services.udev.packages = [ openhantek6022 pkgs.saleae-logic ];
    home-manager.sharedModules = [{
      home.packages = [
        openhantek6022
        # pkgs.pulseview
        pkgs.saleae-logic
      ];
    }];
  };
}
