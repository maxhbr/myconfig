# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  imports = [{
    nixpkgs.overlays = [
      (self: super: {
        my-wallpapers = let
          jsonFile = ./. + "/maxhbr-wallpapers.json";
          json = builtins.fromJSON (builtins.readFile jsonFile);
          my-wallpapers-source =
            pkgs.fetchFromGitHub { inherit (json) owner repo rev sha256; };
        in super.callPackage my-wallpapers-source { };
      })
    ];
  }];
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [ my-wallpapers ];
      services.random-background = {
        enable = true;
        imageDirectory = "${pkgs.my-wallpapers}/share";
        display = "scale";
        interval = "10min";
      };
    }];

    services.xserver.displayManager.lightdm.background =
      "${pkgs.my-wallpapers}/share/romben3.png";
  });
}
