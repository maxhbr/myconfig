# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    home-manager.users.mhuber = {
      home.file = {
        "bin" = {
          source = ./bin;
          recursive = true;
        };
      };
    };
  };
}
