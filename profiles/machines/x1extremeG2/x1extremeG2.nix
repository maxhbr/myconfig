# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

#
#
# Hardware:
#   Thinkpad X1 Extreme Generation 2
#   FHD Display  => lowres.nix
#   i7-9750H
#   GeForce GTX 1650
#
#

{
  imports = [
    ../../hardware/notebook-generic.nix
    ../../hardware/nixos-hardware/lenovo/thinkpad/x1-extreme/gen2/default.nix
    ../../hardware/ssd.nix
    ../../hardware/lowres.nix

    ##############################################################################
    ##  choos setup for graphics  ################################################
    ##############################################################################
    (import ../../hardware/gtx1650.nix).rawNvidiaConf
  ];

  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';
} // (
  # config for libinput
  lib.mkIf (config.services.xserver.libinput.enable) {
    services.xserver.libinput.accelSpeed = "0.15";
  }
)
