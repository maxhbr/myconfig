# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./dev.core
  ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        python python3
      ];
    };
  };
}
