# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs }:
{
  inherit (pkgs)
    photo-scripts
    rawtherapee
    # krita
    # inkscape

    # blender
    librecad;
  inherit (pkgs.unstable)
    gimp-with-plugins
    darktable
    geeqie
    gthumb;
}
