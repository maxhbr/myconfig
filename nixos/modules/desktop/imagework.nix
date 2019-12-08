# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    environment.systemPackages = with pkgs; [
      gphoto2 gphoto2fs

      gimp-with-plugins
      rawtherapee unstable.darktable
      unstable.geeqie unstable.gthumb
      # krita
      # inkscape

      # blender
      librecad # 2D
      freecad # 3D
    ];
  };
}
