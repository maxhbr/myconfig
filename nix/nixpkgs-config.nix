# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
let
  myconfig = import ../. {};
in {
  allowUnfree = true;
  mplayer.useUnfreeCodecs = true;
  # packageOverrides = myconfig.overlays;
  # virtualbox.enableExtensionPack = true;

  packageOverrides = pkgs_: with pkgs_; {
    find-cursor = pkgs_.callPackage ./pkgs/find-cursor {};
    myconfig = {
      dotfiles = myconfig.dotfiles;
      scripts = myconfig.scripts;
      background = myconfig.background;
      slim-theme = myconfig.slim-theme;
    };
  };
}
