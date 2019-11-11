# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ./hardware/grub.nix
    ./hardware/quadroFX4800.nix
    ./hardware/steamcontroller.nix
    ./hardware/pulseaudio.nix
    # modules
    ../modules/desktop/xmonad.nix
    ../modules/desktop/xfce.nix
    ../modules/desktop/games
    ../modules/virtualization
    ../modules/wine.nix
    ../modules/service/openssh.nix
    ../modules/service/syncthing.nix
    ../modules/service/vsftp.nix
  ];

  config = {
    services.xserver.displayManager.slim.autoLogin = true;
    # services.xserver.windowManager.default = pkgs.lib.mkForce "xfce";
  };
}
