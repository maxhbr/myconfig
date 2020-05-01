# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports = [
    ./hardware-configuration.nix
    ../hardware/efi.nix
    # other profiles
    ../headless.nix
    ../dev.nix
    ../gaming.nix
    ../modules/X.xfce.nix
  ] ++ importall ./imports;

  config = {
    networking.hostName = "workstation";
    networking.hostId = ;
    services.xserver.displayManager.slim.autoLogin = true;
    # services.xserver.windowManager.default = pkgs.lib.mkForce "xfce";
  };
}
