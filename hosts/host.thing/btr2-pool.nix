# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  myconfig,
  ...
}:
{
  boot.initrd.luks.devices."btr2_pool" = {
    device = "/dev/disk/by-uuid/4162ab7a-c579-4152-9e50-98a819b0d0af";
    allowDiscards = true;
  };

  fileSystems."/home/${myconfig.user}/models" = {
    device = "/dev/disk/by-uuid/54e93b27-641e-484f-b8d5-6487b73bcf94";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "subvol=@models"
      "nofail"
      "discard"
      "noatime"
    ];
  };

  fileSystems."/models" = {
    device = "/dev/disk/by-uuid/54e93b27-641e-484f-b8d5-6487b73bcf94";
    fsType = "btrfs";
    options = [
      "ro"
      "compress=zstd"
      "subvol=@models"
      "nofail"
      "discard"
      "noatime"
    ];
  };
}
