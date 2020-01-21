# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ./grub.nix
    ./quadroFX4800.nix
    ./steamcontroller.nix
    ./pulseaudio.nix
    # other profiles
    ../profiles/desktop/xmonad.nix
    ../profiles/desktop/xfce.nix
    ../profiles/desktop/games
    ../profiles/virtualization
    ../profiles/wine.nix
    ../profiles/service/openssh.nix
    ../profiles/service/syncthing.nix
    ../profiles/service/vsftp.nix
  ];

  config = {
    services.xserver.displayManager.slim.autoLogin = true;
    # services.xserver.windowManager.default = pkgs.lib.mkForce "xfce";
  };
}
