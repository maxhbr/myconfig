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
          data1 UUID=6eba13f1-b805-4cc1-92d6-014e4a37e854 /etc/cryptkey luks,noearly
          data2 UUID=28103c1e-b1df-4d28-b99f-1f3c3e474962 /etc/cryptkey luks,noearly
          data3 UUID=d26bfce9-0df4-4563-8af7-6f0cc744a3c9 /etc/cryptkey luks,noearly
          data4 UUID=dbf35925-d912-4a76-ad05-9e90ef5c21f0 /etc/cryptkey luks,noearly
        '';
        fileSystems."/mnt/data" =
          { device = "/dev/mapper/data1";
            fsType = "btrfs";
            options = [ "defaults" "noatime" "compress=zstd" "subvol=@sub" "nofail"  ];
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
