# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.imagework.enable = mkEnableOption "imagework";
  };
  config = (lib.mkIf cfg.desktop.imagework.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        gphoto2
        gphoto2fs

        gimp # -with-plugins
        # rawtherapee
        # darktable
        # ansel # A darktable fork minus the bloat plus some design vision
        geeqie
        # gthumb
        # krita
        # inkscape
        sigal # static website generator
      ];
    }];
  });
}
