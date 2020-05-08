# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  wineCfg = {
    wineBuild = "wineWow";
    gstreamerSupport = false;
  };
  wowWine = pkgs.wine.override wineCfg;
  wowWinetricks = (pkgs.winetricks.override {wine = wowWine;});
  wowPlayonlinux = (pkgs.playonlinux.override {wine = wowWine;});

  winePkgs = with pkgs; [ wine winetricks playonlinux ];
  wowWinePkgs = [ wowWine wowWinetricks wowPlayonlinux ];
  # cosmoteer = ( helper.wrap
  #    { name   = "cosmoteer";
  #      paths  = [ wget wowWine wowWinetricks ];
  #      script = builtins.readFile ./bin/cosmoteer.sh;
  #    }
  # );

in {
  config = {
    home-manager.users.mhuber = {
      home.packages = winePkgs;
    };
    hardware.opengl.driSupport32Bit = true;
  };
}
