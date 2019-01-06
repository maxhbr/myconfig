# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  powerManagement.enable = true;

  environment.systemPackages = with pkgs; [
    acpi acpid
    xorg.xbacklight
    powertop
  ];

  # networking.wireless.enable = true;
  hardware.bluetooth.enable = true;

  services = {
    logind.lidSwitch = "suspend";
    logind.lidSwitchDocked = "suspend";
    logind.extraConfig = "HandlePowerKey=suspend";
    xserver.libinput = {
      naturalScrolling = true;
      disableWhileTyping = true;
      tappingDragLock = false;
    };
    # xserver.synaptics = {
    #   enable = true;
    #   twoFingerScroll = true;
    #   vertEdgeScroll = false;
    # };
  };
}
