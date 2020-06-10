# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{ imports =
    [ ../lib
      ../modules/core.nix
      ../modules/user.mhuber.nix
      ../modules/service.openssh.nix
      /mnt/etc/nixos/configuration.nix
      ../hardware/btrfs.nix
    ];

  config = {
    networking.hostName = "myconfig";
    networking.hostId = "12345678";
  };
}
