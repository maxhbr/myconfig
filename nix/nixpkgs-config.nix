# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
let
  mypkgs = import ../. {};
in {
  allowUnfree = true;
  mplayer.useUnfreeCodecs = true;
  # packageOverrides = myconfig.overlays;
  # virtualbox.enableExtensionPack = true;

  packageOverrides = pkgs_: with pkgs_; {
    inherit (mypkgs) myconfig;
  };
}
