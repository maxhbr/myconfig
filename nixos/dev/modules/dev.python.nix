# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./dev.core.nix
  ];
  config = {
    environment.systemPackages = with pkgs; [
      python python3
    ];
  };
}
