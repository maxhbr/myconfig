# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  imports = [ ../../hardware/eGPU.nix ../../hardware/RX5500XT.nix ];
  config = {
    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          selectedSessions = [
            # "hyprland"
            "niri"
            "niri-plain"
            "labwc"
            "river"
            "plasma6"
            # "qtile"
          ];
        };
      };
    };

    services.hardware.bolt.enable = true;

    networking.hostId = "12345678";
    boot.kernelPackages = lib.mkForce pkgs.linuxPackages;

    boot.initrd.supportedFilesystems = [ "luks" ];
  };
}
