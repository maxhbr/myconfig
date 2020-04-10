# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
{
  config = {
    ## try to fix:
    ##   no gsettings schemas are installed on the system
    # nixpkgs.overlays =
    #   [(final: prev: {
    #      rambox = prev.rambox.overrideAttrs
    #        ( o:{
    #          buildInputs = with prev; [ wrapGAppsHook gtk3 hicolor-icon-theme ];
    #          dontWrapGApps = false;
    #        });
    #   })];
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        libreoffice
        zoom-us bluejeans-gui
        rambox slack-dark tdesktop
      ];
    };
  };
}
