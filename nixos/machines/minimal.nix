# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [];
  myconfig.active-roles = [ "xmonad" "xfce" ];
}
