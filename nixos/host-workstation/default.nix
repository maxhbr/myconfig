# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports =
    [ ./hardware-configuration.nix
      ../hardware/efi.nix
      # other profiles
      ../headless.nix
      ../dev.nix
      ../modules/imagework
      ../gaming.nix
      ../modules/desktop.X.xfce.nix
      ../modules/desktop.X.vnc.nix
      ../modules/desktop.X.rdp.nix
    ] ++ importall ./imports;

  config =
    { networking.hostName = "workstation";
      networking.hostId = "864d73f4";

      boot.initrd.supportedFilesystems = [ "luks" ];
      boot.initrd.luks.devices.crypted =
        { device = "/dev/disk/by-uuid/46fc7672-6bcc-4245-8d73-65c81cda0c58";
          preLVM = true;
          allowDiscards = true;
        };

      services.xserver.videoDrivers = [ "amdgpu" ];

      # services.xserver.windowManager.default = pkgs.lib.mkForce "xfce";

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = lib.mkForce "20.09"; # Did you read the comment?
    };
}
