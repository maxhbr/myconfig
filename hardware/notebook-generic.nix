# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:

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

    services.logind.settings.Login = {
      HandlePowerKey = lib.mkDefault "suspend";
      HandleLidSwitch = lib.mkDefault "suspend";
      HandleLidSwitchDocked = lib.mkDefault "suspend";
      RuntimeDirectorySize = "8G";
    };

    services.hardware.bolt.enable = true;

    # disable USB wakeup
    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="usb", ATTR{power/wakeup}="disabled"
    '';
  };
}
