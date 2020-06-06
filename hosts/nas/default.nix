# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
      { boot.initrd.supportedFilesystems = [ "btrfs" ];
        fileSystems."/mnt/data" =
          { device = "/dev/disk/by-uuid/a42b662d-acb4-49bc-9051-e2a71606f589";
            fsType = "btrfs";
            options = [ "subvol=@sub" ];
          };
      }
    ../../roles/headless.nix
    # hardware:
    ../../hardware/efi.nix
    # configuration
  ];
  config = {
    networking.hostName = "nas";
    networking.hostId = "29d93341";
  };
}
