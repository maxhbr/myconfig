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
    # Build tooling only: the sandboxes see no serial or USB devices.
    myconfig.ai.dev.sandboxTools.extraPackages = with pkgs; [ platformio-core ];
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          platformio-core

          # picocom
          # minicom
          tio

          # Host-side probe clients: mysbx's private `/dev` keeps every
          # sandbox away from the ttyACM/hidraw nodes, so `pyocd
          # gdbserver`, `west flash` and friends run on the host — as
          # this user or the `agnt-embedded` agent user.
          pyocd
          openocd
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
