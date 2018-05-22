# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs: {
  allowUnfree = true;
  mplayer.useUnfreeCodecs = true;
  # packageOverrides = myconfig.overlays;
  # virtualbox.enableExtensionPack = true;
}
