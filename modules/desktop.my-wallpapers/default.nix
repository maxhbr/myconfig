# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  user = config.myconfig.user;
  jsonFile = ./. + "/maxhbr-wallpapers.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  my-wallpapers-source =
    pkgs.fetchFromGitHub { inherit (json) owner repo rev sha256; };
in {
  imports = [
    {
      nixpkgs.overlays = [
        (self: super: {
          my-wallpapers = super.callPackage my-wallpapers-source { };
        })
      ];
    }
  ];
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [ my-wallpapers my-wallpapers-source ];
      services.random-background = {
        enable = true;
        imageDirectory = "${pkgs.my-wallpapers}/share/1440";
        display = "scale";
        interval = "10min";
      };
    };

    services = {
      xserver = {
        displayManager = {
          lightdm = {
            background = "${pkgs.my-wallpapers}/share/romben3.png";
          };
          # sessionCommands = ''
          #   ${pkgs.my-wallpapers}/bin/myRandomBackground &disown
          # '';
        };
      };
    };
  });
}
