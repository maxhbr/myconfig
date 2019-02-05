# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# this adds the LIBGL_ALWAYS_SOFTWARE=1 flag to fix the gui issue
#
# related:
#  - https://github.com/NixOS/nixpkgs/issues/43350
#  - https://github.com/NixOS/nixpkgs/issues/47608


self: super:
{
  zoom-us = super.zoom-us.overrideAttrs ( oldAttrs: {
    fixupPhase = ''
sed -n -i 'p;1a export QT_DEBUG_PLUGINS=0' $out/bin/zoom-us
sed -n -i 'p;1a export LIBGL_ALWAYS_SOFTWARE=1' $out/bin/zoom-us
    '';
  });
}
