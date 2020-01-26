# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./minimal.nix
    # hardware:
    ./hardware/x1extremeG2.nix
    ./hardware/efi.nix
    # configuration
    ../profiles/desktop
    ../profiles/mail
    ../profiles/dev
    ../profiles/work
    ## fun
    ../profiles/imagework
    ../profiles/misc/smarthome.nix
    ../profiles/gaming
  ];

  config = {
    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices = [{
      device = "/dev/disk/by-uuid/2118a468-c2c3-4304-b7d3-32f8e19da49f";
      name = "crypted";
      preLVM = true;
      allowDiscards = true;
    }];

    # option definitions
    boot.kernel.sysctl = {
      "vm.swappiness" = 1;
    };
  };
}
