# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  winePkgs = with pkgs.unstable; [ wine winetricks playonlinux ];
  wowWinePkgs = let
    wineCfg = {
      wineBuild = "wineWow";
      gstreamerSupport = false;
    };
    wowWine = pkgs.unstable.wine.override wineCfg;
    wowWinetricks = (pkgs.unstable.winetricks.override { wine = wowWine; });
    wowPlayonlinux = (pkgs.unstable.playonlinux.override { wine = wowWine; });
  in [ wowWine wowWinetricks wowPlayonlinux ];
  # cosmoteer = ( helper.wrap
  #    { name   = "cosmoteer";
  #      paths  = [ wget wowWine wowWinetricks ];
  #      script = builtins.readFile ./bin/cosmoteer.sh;
  #    }
  # );

in {
  config = {
    nixpkgs.config.permittedInsecurePackages = [
      "p7zip-16.02" # in winetricks
    ];

    home-manager.users.mhuber = { home.packages = wowWinePkgs; };
    hardware.opengl.driSupport32Bit = true;
  };
}
