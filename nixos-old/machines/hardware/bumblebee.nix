# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
{
  hardware.bumblebee.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
}
