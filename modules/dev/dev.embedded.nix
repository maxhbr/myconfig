# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.dev.embedded;
  user = myconfig.user;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          platformio-core

          # picocom
          # minicom
          tio
        ];
      }
    ];
    users.groups.plugdev = { };
    users.users."${user}".extraGroups = lib.mkAfter [
      "dialout"
      "uucp"
      "plugdev"
    ];
    nixpkgs.config = {
      allowUnfreePredicate =
        pkg:
        builtins.elem (lib.getName pkg) [
          "segger-jlink"
          "STM32CubeProg"
        ];
      segger-jlink.acceptLicense = true;
    };
    services.udev.packages = with pkgs; [
      platformio-core
      openocd
      stlink
      dfu-util
      teensy-udev-rules
      segger-jlink-headless
    ];
    # Extra “belt-and-suspenders” rules for very common probes:
    services.udev.extraRules = ''
      # CMSIS-DAP (hidraw access)
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0d28", TAG+="uaccess"

      # STMicroelectronics ST-LINK/V2 (various IDs; keep MODE conservative)
      SUBSYSTEM=="usb", ATTR{idVendor}=="0483", ATTR{idProduct}=="3748", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTR{idVendor}=="0483", ATTR{idProduct}=="374b", TAG+="uaccess"
    '';
  };
}
