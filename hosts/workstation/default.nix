# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
{
  imports =
    [ ./hardware-configuration.nix
      ../../hardware/efi.nix
      ../../hardware/nixos-hardware/common/cpu/amd
      ../../hardware/nixos-hardware/common/pc/ssd
      { boot.initrd.supportedFilesystems = [ "luks" ];
        boot.initrd.luks.devices.crypted =
          { device = "/dev/disk/by-uuid/46fc7672-6bcc-4245-8d73-65c81cda0c58";
            keyFile = "/dev/disk/by-id/usb-JetFlash_Transcend_16GB_753K3Z31LDXXOPIT-0:0";
            keyFileSize = 4096;
            preLVM = true;
            allowDiscards = true;
            fallbackToPassword = true;
          };

        fileSystems."/mnt/lubuntu" =
          { device = "/dev/disk/by-uuid/e3d8b271-deb4-4d4d-b58e-72137b667b24";
            fsType = "ext4";
          };
        fileSystems."/mnt/2tb-1" =
          { device = "/dev/disk/by-uuid/51d362d8-5b73-4b92-84c3-9ff260062da6";
            fsType = "ext4";
          };
      }
      { hardware.enableRedistributableFirmware = true;
        services.xserver.videoDrivers = [ "amdgpu" ];
      }
      ./nfs.nix
      # other profiles
      ../../roles/headless.nix
      ../../roles/dev.nix
      ../../roles/imagework
      ../../roles/gaming.nix
      ../../modules/desktop.X.xfce.nix
      ../../modules/desktop.X.vnc.nix
      ../../modules/desktop.X.rdp.nix
      ../../modules/games.doom3.nix
      ../../modules/games.wine
      ../../modules/games.lutris.nix
      ../../modules/benchmarking.nix
    ];

  config =
    { networking.hostName = "workstation";
      networking.hostId = "864d73f4";

      services.logind.extraConfig = ''
          HandlePowerKey=suspend
        '';

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = lib.mkForce "20.09"; # Did you read the comment?
    };
}
