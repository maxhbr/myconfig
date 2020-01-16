# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
  ];

  config = {
    boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  };
}
