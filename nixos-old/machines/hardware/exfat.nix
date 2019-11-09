# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    exfat
    # linuxPackages.exfat-nofuse
  ];
}
