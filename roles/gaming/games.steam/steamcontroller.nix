# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pkgs.steamcontroller-udev-rules
  ];

  # steam controller
  nixpkgs.config.packageOverrides = pkgs: {
    steamcontroller-udev-rules = pkgs.writeTextFile {
      name = "steamcontroller-udev-rules";
      text = ''
        # This rule is needed for basic functionality of the controller in
        # Steam and keyboard/mouse emulation
        SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"
        #USB devices
        SUBSYSTEM=="usb", ATTRS{idVendor}=="c251", ATTRS{idProduct}=="2202", MODE="0666"
        SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2202", MODE="0666"
        SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="1101", MODE="0666"
        SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="1051", MODE="0666"
        SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="1142", MODE="0666"
        SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="1042", MODE="0666"

        # This rule is necessary for gamepad emulation; make sure you
        # replace 'pgriffais' with the username of the user that runs Steam
        KERNEL=="uinput", MODE="0660", GROUP="wheel", OPTIONS+="static_node=uinput"
        # systemd option not yet tested
        #KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", TAG+="udev-acl"
        #KERNEL=="uinput", SUBSYSTEM=="misc", MODE="0660", GROUP="input"

        # HTC Vive HID Sensor naming and permissioning
        KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="2c87", MODE="0666"
        # Oculus HID Sensor naming and permissioning
        KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="2833", MODE="0666"
        # Valve HID devices over USB hidraw
        KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0666"
        ## Valve HID devices over bluetooth hidraw
        #KERNEL=="hidraw*", KERNELS=="*28DE:*", MODE="0666"
      '';
      destination = "/etc/udev/rules.d/99-steamcontroller.rules";
    };
  };
  services.udev.packages = [ pkgs.steamcontroller-udev-rules ];
}
