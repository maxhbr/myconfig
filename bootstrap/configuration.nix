# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{ imports =
    [ ../lib
      ../roles/core.nix
      ../modules/service.openssh.nix
      /mnt/etc/nixos/configuration.nix
    ];

  config = {
    networking.hostName = "bootstrapped";
    networking.hostId = "12345678";
  };
}
