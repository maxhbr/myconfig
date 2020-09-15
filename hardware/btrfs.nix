# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }: {
  swapDevices = [{
    device = "/.swapfile/swapfile";
    priority = 0;
    size = 20480;
  }];
  environment.systemPackages = with pkgs; [ snapper ];
  # services.btrfs.autoScrub.enable = true;
}
