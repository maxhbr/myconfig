# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

let
  jsonFile = ./. + "home-manager.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    inherit (json) rev ref;
  };
in
{
  imports = [
    "${home-manager}/nixos"
  ];
}

