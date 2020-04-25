# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports = [
    ./hardware-configuration.nix
    ./nixPath.nix
    ../headless.nix
    # hardware:
    ../hardware/grub.nix
    # configuration
    ../modules/service.wireguard-server
  ] ++ importall ./imports;
  config = {
    networking.hostName = "vserver";
  };
}
