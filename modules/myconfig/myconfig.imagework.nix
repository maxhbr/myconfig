# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    imagework.enable = mkEnableOption "imagework";
  };
  config = (lib.mkIf cfg.imagework.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        gphoto2
        gphoto2fs

        gimp # -with-plugins
        rawtherapee
        unstable.darktable
        unstable.geeqie
        unstable.gthumb
        krita
        # inkscape

        # blender
        librecad # 2D
        freecad # 3D
      ];
    }];
  });
}
