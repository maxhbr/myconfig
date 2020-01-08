# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        tmux
      ];
      home.file = {
        ".tmux.conf".source = ./tmux.conf;
      };
    };
  };
}
