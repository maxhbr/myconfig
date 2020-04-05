# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../dev.nix
  ];

  config = {
    networking.hostName = "minimal";
  };
}
