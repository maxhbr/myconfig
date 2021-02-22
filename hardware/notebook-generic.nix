# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  imports = [
    { # config for libinput
      config = lib.mkIf (config.services.xserver.libinput.enable) {
        services.xserver.libinput.touchpad = {
          disableWhileTyping = true;
          tappingDragLock = false;
          naturalScrolling = true;
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
    {
      programs.light.enable = true;
      services.actkbd = {
        enable = true;
        bindings = [
          {
            keys = [ 224 ];
            events = [ "key" ];
            command = "${pkgs.light}/bin/light -U 10";
          }
          {
            keys = [ 225 ];
            events = [ "key" ];
            command = "${pkgs.light}/bin/light -A 10";
          }
        ];
      };
    }
  ];

  config = {
    powerManagement.enable = true;

    environment.systemPackages = with pkgs; [
      acpi
      acpid
      xorg.xbacklight
      powertop
    ];

    hardware.bluetooth.enable = true;

    services = {
      logind.lidSwitch = "suspend";
      logind.lidSwitchDocked = "suspend";
      logind.extraConfig = "HandlePowerKey=suspend";
    };
  };
}
