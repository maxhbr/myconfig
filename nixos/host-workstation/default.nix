# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../hardware/grub.nix
    ../hardware/quadroFX4800.nix
    # other profiles
    ../desktop
    ../desktop/modules/X.xfce.nix
    #../profiles/desktop/xfce.nix
    #../profiles/desktop/games
    #../profiles/virtualization
    #../profiles/wine.nix
    #../profiles/service/openssh.nix
    #../profiles/service/syncthing.nix
    #../profiles/service/vsftp.nix
  ];

  config = {
    networking.hostName = "workstation";
    services.xserver.displayManager.slim.autoLogin = true;
    boot.kernelPackages = lib.mkForce pkgs.unstable.linuxPackages_4_14;
    services.xserver.windowManager.default = pkgs.lib.mkForce "xfce";
  };
}
