# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "10d2c4e7e49d4c9ff0782a2d72ceaf117aacbee9";
    ref = "release-19.09";
  };
in
{
  imports = [
    "${home-manager}/nixos"
  ];

  home-manager.users.mhuber = {
  };
}

