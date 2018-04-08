# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
{
  allowUnfree = true;
  mplayer.useUnfreeCodecs = true;
  # packageOverrides = myconfig.overlays;
  # virtualbox.enableExtensionPack = true;

  packageOverrides = pkgs_: with pkgs_; {
    find-cursor = pkgs_.callPackage ./pkgs/find-cursor {};
    myconfig-background = pkgs_.callPackage ../background { pkgs = self; stdenv = super.stdenv; };
    myconfig-slim-theme = pkgs_.callPackage ../background/slim-theme { pkgs = self; stdenv = super.stdenv; background = myconfig-background; };
  };
}
