# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./hardware/grub.nix
    # modules
    ../modules/service/openssh.nix
  ];
}
