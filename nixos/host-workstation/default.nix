# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports = [
    ./hardware-configuration.nix
    ../hardware/grub.nix
    ../hardware/quadroFX4800.nix
    # other profiles
    ../headless.nix
    ../desktop.nix
    ../modules/X.xfce.nix
  ] ++ importall ./imports;

  config = {
    networking.hostName = "workstation";
    services.xserver.displayManager.slim.autoLogin = true;
    boot.kernelPackages = lib.mkForce pkgs.unstable.linuxPackages_4_14;
    # services.xserver.windowManager.default = pkgs.lib.mkForce "xfce";
  };
}
