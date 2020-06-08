# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
      { boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];

        # ########################################################################
        # /mnt/data
        # see:
        # - https://github.com/NixOS/nixpkgs/issues/75540
        # - https://gist.github.com/MaxXor/ba1665f47d56c24018a943bb114640d7
        systemd.packages = [ pkgs.systemd-cryptsetup-generator ];
        environment.etc.crypttab.text = ''
          data0 UUID=f435b742-75da-4a0b-894f-06f329afebb8 /etc/cryptkey luks,noearly
          data1 UUID=327a652d-79b0-4740-950c-684d5b56e66d /etc/cryptkey luks,noearly
          data2 UUID=da356132-803e-439b-886d-91d70048b7f9 /etc/cryptkey luks,noearly
          data3 UUID=c5e77337-21ad-4338-987a-710201dd5636 /etc/cryptkey luks,noearly
        '';
        fileSystems."/mnt/data0" =
          { device = "/dev/mapper/data0";
            fsType = "btrfs";
            options = [ "defaults" "noatime" "compress=zstd" "subvol=@sub" "nofail" "x-systemd.requires-mounts-for=/mnt/.data3" ];
          };
        fileSystems."/mnt/.data1" =
          { device = "/dev/mapper/data1";
            fsType = "btrfs";
            options = [ "defaults" "noatime" "compress=zstd" "subvol=@sub" "nofail" ];
          };
        fileSystems."/mnt/.data2" =
          { device = "/dev/mapper/data2";
            fsType = "btrfs";
            options = [ "defaults" "noatime" "compress=zstd" "subvol=@sub" "nofail" "x-systemd.requires-mounts-for=/mnt/.data1" ];
          };
        fileSystems."/mnt/.data3" =
          { device = "/dev/mapper/data3";
            fsType = "btrfs";
            options = [ "defaults" "noatime" "compress=zstd" "subvol=@sub" "nofail" "x-systemd.requires-mounts-for=/mnt/.data2" ];
          };

        services.btrfs.autoScrub =
          { enable = true;
            # fileSystems = [ "/" "/mnt/v0" ];
          };

        ########################################################################
        # USB HDDS:
        fileSystems."/mnt/tng-backup" =
          { device = "/dev/disk/by-uuid/480aaf1f-aae0-469e-911e-13f961b46ec3";
            fsType = "ext4";
            options = ["auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime"];
          };
        fileSystems."/mnt/foto-backup" =
          { device = "/dev/disk/by-uuid/a11523b0-e4f5-4af6-8551-2b43989c4781";
            fsType = "ext4";
            options = ["auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime"];
          };
      }
    ./service.nfs.nix
    ../../roles/headless.nix
    ../../modules/service.monitoring.nix
    ../../modules/virtualization.docker
    # hardware:
    ../../hardware/efi.nix
    # configuration
  ];
  config = {
    networking.hostName = "nas";
    networking.hostId = "29d93341";

    services.logind.extraConfig = ''
        HandlePowerKey=suspend
      '';

  };
}
