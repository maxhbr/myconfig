# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ../modules/hardware/grub.nix
    ../modules/hardware/quadroFX4800.nix
    ../modules/hardware/steamcontroller.nix
    ../modules/hardware/pulseaudio.nix
    # other modules
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
