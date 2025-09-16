# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  boot.loader.grub = {
    enable = true;
    device = "/dev/sda";
    memtest86.enable = true;
    # splashImage = ../../static/grub-splashImage.png;
  };
}
