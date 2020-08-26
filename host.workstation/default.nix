# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
{
  imports =
    [ ./hardware-configuration.nix
      ../hardware/efi.nix
      ../hardware/btrfs.nix
      ../hardware/nixos-hardware/common/cpu/amd
      ../hardware/nixos-hardware/common/pc/ssd
      ../hardware/hdd-spinndown.nix
      ./4x500-hdds.raid.nix
      ../role.headless
      # other profiles
      ../role.desktop
      ../role.dev/virtualization.docker
      ../role.dev/programs.license-compliance-toolbox.nix
      ../role.dev/dev.haskell
      ../role.imagework
      ../role.gaming
      # ../role.desktop/desktop.X.xfce.nix
      ../role.desktop/desktop.X.vnc.nix
      ../role.desktop/desktop.X.rdp.nix
      { # for quickfix (due to usage of 20.03)
        nixpkgs.config.allowBroken = true;
      }
    ] ++
    (with (import ../lib.nix);
      [ (setupAsWireguardClient "10.199.199.5")
      ]
    );

  config =
    { networking.hostName = "workstation";
      networking.hostId = "864d73f4";

      networking.firewall.allowedTCPPorts = [ 12345 6567 ];
      networking.firewall.allowedUDPPorts = [ 12345 6567 ];

      services.logind.extraConfig = ''
          HandlePowerKey=suspend
        '';

      hardware.enableRedistributableFirmware = true;
      hardware.cpu.amd.updateMicrocode = true;
      services.xserver.videoDrivers = [ "amdgpu" ];

      boot.initrd.supportedFilesystems = [ "luks" "btrfs" ];
      boot.binfmt.emulatedSystems = [ "aarch64-linux" "armv6l-linux" ];

      fileSystems."/mnt/2tb-1" =
        { device = "/dev/disk/by-uuid/51d362d8-5b73-4b92-84c3-9ff260062da6";
          fsType = "ext4";
        };

      services.snapper =
        { snapshotInterval = "hourly";
          cleanupInterval = "1d";
          filters = null;
          configs =
            { home =
                { subvolume = "/home";
                  extraConfig = ''
                    ALLOW_USERS="mhuber"
                  '';
                };
            };
        };

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
