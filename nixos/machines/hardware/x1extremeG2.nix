# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

#
#
# Hardware:
#   Thinkpad X1 Extreme Generation 2
#
#

{
  imports = [
    ./notebook-generic.nix
    ./ssd.nix
    ./wwan.nix
    ./nixos-hardware/lenovo/thinkpad/x1-extreme/gen2/default.nix

    { # config for libinput
      config = lib.mkIf (config.services.xserver.libinput.enable) {
        services.xserver.libinput.accelSpeed = "0.15";
      };
    }
  ];

  nix.buildCores = 8;

  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';
}
