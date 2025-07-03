# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.network;
in
{
  config = lib.mkIf cfg.enable {
    programs.wireshark.enable = true;
    programs.firejail.enable = true;
  };
}
