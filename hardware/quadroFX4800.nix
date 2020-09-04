# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

# Hardware:
#   GeForce Quadro FX 4800

{
  services.xserver.videoDrivers = [ "nvidiaLegacy340" ];
  # services.xserver.videoDrivers = ["nouveau"];
}
