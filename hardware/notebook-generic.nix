# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{

  config = {
    powerManagement.enable = true;

    environment.systemPackages = with pkgs; [
      acpi
      acpid
      xorg.xbacklight
      powertop
    ];

    hardware.bluetooth.enable = true;

    programs.light.enable = true;

    services = {
      logind.lidSwitch = "suspend";
      logind.lidSwitchDocked = "suspend";
      logind.extraConfig = "HandlePowerKey=suspend";
    };
  };
}
