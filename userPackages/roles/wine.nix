
#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
with pkgs; [
  # support both 32- and 64-bit applications
  (wine.override { wineBuild = "wineWow"; })
  # (winetricks.override { wine = wineWowPackages.staging; })
]
