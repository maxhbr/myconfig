# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  jsonFile = ./. + "/maxhbr-wallpapers.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  my-wallpapers-source = pkgs.fetchzip json;
in {
  config = {
    nixpkgs.overlays = [(self: super: {
      my-wallpapers = super.callPackage my-wallpapers-source {};
    })];
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        my-wallpapers
      ];
    };
  };
}
