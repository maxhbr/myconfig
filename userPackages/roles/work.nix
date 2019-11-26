
#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
with pkgs; [
  thunderbird
  openvpn networkmanager_openvpn
  # rdesktop
  openjdk11 maven gradle
  libreoffice
  zoom-us # unstable.zoom-us
  rambox # franz hipchat
  p7zip
  thrift011
  idea-ultimate jetbrains.phpstorm
  dia
]
