# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, inputs, ... }:
let
  zoom-old-screenshare-pkgs = import inputs.zoom-old-screenshare {
    inherit (pkgs) system;
    config = pkgs.config;
  };
  zoom-us-overlay = (self: super: {
    zoom-us = super.zoom-us.overrideAttrs (old: {
      # postFixup = old.postFixup + ''
      postFixup = ''
        wrapProgram $out/bin/zoom-us \
          --set QT_DEBUG_PLUGINS 1 \
          --set XDG_CURRENT_DESKTOP gnome \
          --set XDG_SESSION_TYPE ""
      '';
    });
    zoom-old-screenshare = zoom-old-screenshare-pkgs.zoom-us;
  });
  zoom-us = pkgs.zoom-us;
  mk-zoom-auto = name: zoom-pkg:
    let my-script-deps = [ zoom-pkg pkgs.wl-clipboard ];
    in pkgs.runCommandLocal name {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
      install -m755 ${./zoom-auto.sh} -D $out/bin/${name}
      patchShebangs $out/bin/${name}
      wrapProgram "$out/bin/${name}" --prefix PATH : ${
        pkgs.lib.makeBinPath my-script-deps
      }
    '';
  zoom-auto = mk-zoom-auto "zoom-auto" zoom-us;
  zoom-auto-old-screenshare =
    mk-zoom-auto "zoom-auto-old-screenshare" pkgs.zoom-old-screenshare;
  # zoom-auto-wl = mk-zoom-auto "zoom-auto-wl" "${pkgs.cage}/bin/cage -- ${zoom-us}/bin/zoom-us"; # does not work
in {
  config = {
    nixpkgs.overlays = [ zoom-us-overlay ];
    home-manager.sharedModules = [{
      home.packages = [ zoom-us zoom-auto zoom-auto-old-screenshare ];
      xdg.mimeApps = {
        defaultApplications."x-scheme-handler/zoommtg" =
          [ "us.zoom.Zoom.desktop" ];
      };
    }];
  };
}
