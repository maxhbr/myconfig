# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{ imports =
    [ ../lib
      ../modules/core.nix
      ../modules/user.mhuber.nix
      ../modules/service.openssh.nix
      /mnt/etc/nixos/connfiguration.nix
    ];

  config = {
    networking.hostName = "myconfig";
    networking.hostId = "123456";
  };
}
