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
    ../hardware/lowres.nix

    (lib.mkIf (config.services.xserver.libinput.enable) {
      services.xserver.libinput.touchpad.accelSpeed = "0.15";
    })

    ##############################################################################
    ##  choose setup for graphics  ###############################################
    ##############################################################################
    # (import ../hardware/gtx1650.nix).primeRenderOffload
    # (import ../hardware/gtx1650.nix).bumblebeeConf
    (import ../hardware/gtx1650.nix).rawIntelConf
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
