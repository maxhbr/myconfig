# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  services.xserver.wacom.enable = true;
  environment.systemPackages = with pkgs; [ xf86_input_wacom ];
}
