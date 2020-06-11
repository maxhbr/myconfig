# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
{
  imports =
    [ ./hardware-configuration.nix
      ../../hardware/efi.nix
      ../../hardware/btrfs.nix
      ../../hardware/nixos-hardware/common/cpu/amd
      ../../hardware/nixos-hardware/common/pc/ssd
      { boot.initrd.supportedFilesystems = [ "luks" "btrfs" ];
        # boot.initrd.luks.devices.crypted =
        #   { device = "/dev/disk/by-uuid/1ffeb5d7-29b7-41ee-9fe3-3c528be136db";
        #     # keyFile = "/dev/disk/by-id/usb-JetFlash_Transcend_16GB_753K3Z31LDXXOPIT-0:0";
        #     # keyFileSize = 4096;
        #     # preLVM = true;
        #     # allowDiscards = true;
        #     # fallbackToPassword = true;
        #   };

        fileSystems."/mnt/2tb-1" =
          { device = "/dev/disk/by-uuid/51d362d8-5b73-4b92-84c3-9ff260062da6";
            fsType = "ext4";
          };
      }
      { hardware.enableRedistributableFirmware = true;
        hardware.cpu.amd.updateMicrocode = true;
        services.xserver.videoDrivers = [ "amdgpu" ];
      }
      ./service.nfs.nix
      ../../modules/service.monitoring.nix
      ../../roles/headless.nix
      # other profiles
      ../../roles/desktop.nix
      ../../roles/dev.nix
      ../../roles/imagework
      ../../roles/gaming
      # ../../modules/desktop.X.xfce.nix
      # ../../modules/desktop.X.vnc.nix
      # ../../modules/desktop.X.rdp.nix
      ../../modules/benchmarking.nix
    ];

  config =
    { networking.hostName = "workstation";
      networking.hostId = "864d73f4";

      services.logind.extraConfig = ''
          HandlePowerKey=suspend
        '';

      boot.kernelPackages = lib.mkForce pkgs.nixos-2003-small.linuxPackages_latest;

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = lib.mkForce "20.09"; # Did you read the comment?
    };
}
