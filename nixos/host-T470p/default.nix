# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports = [
    ./hardware-configuration.nix
    ../dev.nix
    # hardware:
    ../hardware/efi.nix
    # modules
    ../modules/service.postgresql.nix
  ] ++ importall ./imports;

  config = {
    networking.hostName = "T470p";
    networking.hostId = "8865d9be";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/98375f3f-0366-421f-9ac6-fda1b5a2d5fe";
      preLVM = true;
      allowDiscards = true;
    };
  };
}
