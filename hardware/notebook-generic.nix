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
      xbacklight
      powertop
    ];

    hardware.bluetooth.enable = true;

    myconfig.desktop.brightnessctl.enable = true;

    services.logind.settings.Login = {
      HandlePowerKey = lib.mkDefault "suspend";
      HandleLidSwitch = lib.mkDefault "suspend";
      HandleLidSwitchDocked = lib.mkDefault "suspend";
      RuntimeDirectorySize = "8G";
    };

    services.hardware.bolt.enable = true;

    services.upower = {
      enable = true;
      usePercentageForPolicy = true;

      percentageLow = 15;
      percentageCritical = 10;
      percentageAction = 5;

      criticalPowerAction = "Hibernate";
    };

    # Überwachung ab Systemstart, unabhängig von der Desktop-Sitzung.
    systemd.services.upower.wantedBy = [ "multi-user.target" ];

    # disable USB wakeup
    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="usb", ATTR{power/wakeup}="disabled"
    '';
  };
}
