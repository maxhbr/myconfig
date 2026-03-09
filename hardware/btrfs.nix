# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  myconfig,
  ...
}:
{
  # imports = [{
  #   # environment.systemPackages = with pkgs; [ snapper ];
  #   services.snapper = {
  #     snapshotInterval = "hourly";
  #     cleanupInterval = "1d";
  #     filters = null;
  #     configs = {
  #       home = {
  #         subvolume = "/home";
  #         extraConfig = ''
  #         ALLOW_USERS="${myconfig.user}"
  #       '';
  #       };
  #     };
  #   };
  # }];
  swapDevices = [
    {
      device = "/.swapfile/swapfile";
      priority = 0;
      size = 20480;
    }
  ];
  # services.btrfs.autoScrub.enable = true;

  # One more practical point: on btrfs, auto-optimise-store is often less compelling anyway, because btrfs already has CoW/reflink-friendly semantics and separate deduplication mechanisms; even a Nix issue discussing defaults notes that filesystem-level deduplication on filesystems like btrfs can supersede auto-optimise-store.
  nix.settings.auto-optimise-store = false;
}
