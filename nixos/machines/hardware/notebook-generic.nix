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
    # logind.extraConfig = "HandleLidSwitch=ignore\nHandlePowerKey=suspend";
    logind.extraConfig = "HandleLidSwitch=suspend\nHandlePowerKey=suspend";
    xserver.synaptics = {
      enable = true;
      twoFingerScroll = true;
      vertEdgeScroll = false;
    };
  };
}
