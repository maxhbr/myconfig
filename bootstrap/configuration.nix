# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{ imports =
    [ ../lib
      ../modules/core.nix
      ../modules/vim
      ../modules/zsh
      ../modules/tmux
      ../modules/user.mhuber.nix
      ../modules/nixos.networking
      ../modules/nixos.nix.nix
      ../modules/service.openssh.nix
      /mnt/etc/nixos/configuration.nix
    ];

  config = {
    networking.hostName = "bootstrapped";
    networking.hostId = "12345678";
  };
}
