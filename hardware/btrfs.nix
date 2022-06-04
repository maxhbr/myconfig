# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, myconfig, ... }: {
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
  swapDevices = [{
    device = "/.swapfile/swapfile";
    priority = 0;
    size = 20480;
  }];
  # services.btrfs.autoScrub.enable = true;

}
