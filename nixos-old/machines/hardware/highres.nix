# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  fonts.fontconfig.dpi = 120;
  services.xserver.dpi = 120;
}
