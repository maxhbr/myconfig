# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  home-manager.sharedModules = [
    {
      home.packages = with pkgs; [ efibootmgr efivar ];
    }
  ];
}
