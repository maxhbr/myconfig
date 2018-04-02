# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
let
  # myconfigPath = /myconfig;
  # myconfig = import myconfigPath { inherit pkgs; };
in {
  allowUnfree = true;
  mplayer.useUnfreeCodecs = true;
  # packageOverrides = myconfig.overlays;
  # virtualbox.enableExtensionPack = true;
}
