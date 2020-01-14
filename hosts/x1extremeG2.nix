# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    # hardware:
    ../modules/hardware/x1extremeG2.nix
    ../modules/hardware/efi.nix
    ../modules/hardware/exfat.nix
    ../modules/hardware/steamcontroller.nix
    ../modules/hardware/pulseaudio.nix
    # core
    ../modules/emacs
    ../modules/desktop.X.xmonad
    ../modules/desktop.Wayland.sway
    # ../modules/desktop.X.xfce.nix
    ../modules/mail.mu4e
    ../modules/mail.mutt
    ../modules/virtualization.docker
    ../modules/virtualization.vbox
    ../modules/virtualization.qemu.nix
    ../modules/service.openssh.nix
    # ../modules/service.syncthing.nix
    ../modules/work
    ../modules/dev.haskell
    ## fun
    ../modules/desktop.games.steam
    ../modules/desktop.games.powder
    ../modules/imagework.nix
    ../modules/smarthome.nix
  ];

  config = {
    boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
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
