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
    ../hardware/notebook-generic.nix
    ../hardware/nixos-hardware/lenovo/thinkpad/x1-extreme/gen2/default.nix
    ../hardware/lowres.nix

    ##############################################################################
    ##  choose setup for graphics  ###############################################
    ##############################################################################
    (import ../hardware/gtx1650.nix).rawNvidiaConf
    (lib.mkIf (config.services.xserver.libinput.enable) {
      services.xserver.libinput.touchpad.accelSpeed = "0.15";
    })
  ];

  config = {
    boot.extraModprobeConfig = ''
      options snd slots=snd-hda-intel
    '';
    services.tlp.settings = {
      USB_BLACKLIST = "0bda:8153"; # see: https://askubuntu.com/a/1044128
      USB_BLACKLIST_PHONE = 1;
    };
  };
}
