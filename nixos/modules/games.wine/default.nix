# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  wineCfg = {
    wineBuild = "wineWow";
    gstreamerSupport = false;
  };
  mywine = pkgs.wine.override wineCfg;
  mywinetricks = (pkgs.winetricks.override {wine = mywine;});

  cosmoteer = with pkgs; wrap {
    name   = "cosmoteer";
    paths  = [ wget mywine mywinetricks ];
    script = builtins.readFile ./bin/cosmoteer.sh;
 };
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        mywine mywinetricks
        cosmoteer
      ];
    };
    hardware.opengl.driSupport32Bit = true;
  };
}
