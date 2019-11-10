# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/grub.nix

    ./vm.nix

    ## software:
    # ../profiles/dev.nix
    ../profiles/virtualization/docker.nix
  ];
}
