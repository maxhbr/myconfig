
#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
with pkgs; [
  # see: https://nixos.wiki/wiki/Wine
  (wine.override { wineBuild = "wineWow"; })
  (winetricks.override { wine = (wine.override { wineBuild = "wineWow"; }); })
]
