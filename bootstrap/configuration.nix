# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{ imports =
    [ ../roles/dev.nix
    ];

  config = {
    networking.hostName = "myconfig";
    networking.hostId = "123456";
  };
}
