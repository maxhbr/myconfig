#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
with pkgs; [
  photo-scripts
  unstable.gimp-with-plugins
  rawtherapee unstable.darktable
  unstable.geeqie unstable.gthumb
  # krita
  # inkscape

  # blender
  librecad
]
