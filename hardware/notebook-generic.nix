# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  powerManagement.enable = true;

  environment.systemPackages = with pkgs; [
    acpi
    acpid
    xorg.xbacklight
    powertop
  ];

  # networking.wireless.enable = true;
  hardware.bluetooth.enable = true;

  services = {
    logind.lidSwitch = "suspend";
    logind.lidSwitchDocked = "suspend";
    logind.extraConfig = "HandlePowerKey=suspend";
  };

  imports = [
    { # config for libinput
      config = lib.mkIf (config.services.xserver.libinput.enable) {
        services.xserver.libinput = {
          naturalScrolling = true;
          disableWhileTyping = true;
          tappingDragLock = false;
        };
      };
    }
    { # config for synaptics (unused?)
      config = lib.mkIf (config.services.xserver.synaptics.enable) {
        services.xserver.synaptics = {
          twoFingerScroll = true;
          vertEdgeScroll = false;
        };
      };
    }
  ];
}
