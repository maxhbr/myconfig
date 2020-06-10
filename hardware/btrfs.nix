# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:
{ ervices.btrfs.autoScrub.enable = true;
  swapDevices = [{
    device = "/.swapfile/swapfile";
    priority = 0;
    size = 2048;
  }];
}
