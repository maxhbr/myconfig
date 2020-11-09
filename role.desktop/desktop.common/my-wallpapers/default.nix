# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  jsonFile = ./. + "/maxhbr-wallpapers.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  my-wallpapers-source =
    pkgs.fetchFromGitHub { inherit (json) owner repo rev sha256; };
in {
  config = {
    nixpkgs.overlays = [
      (self: super: {
        my-wallpapers = super.callPackage my-wallpapers-source { };
      })
    ];
    home-manager.users."${user}" = {
      home.packages = with pkgs; [ my-wallpapers my-wallpapers-source ];
    };
  };
}
