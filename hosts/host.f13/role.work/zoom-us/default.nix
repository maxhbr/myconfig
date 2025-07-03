# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  zoom-us-overlay = (
    self: super: {
      zoom-us = super.zoom-us.overrideAttrs (old: {
        # postFixup = old.postFixup + ''
        postFixup = ''
          wrapProgram $out/bin/zoom-us \
            --set QT_DEBUG_PLUGINS 1 \
            --set XDG_CURRENT_DESKTOP gnome \
            --set XDG_SESSION_TYPE ""
        '';
      });
    }
  );
  zoom-us = pkgs.zoom-us;
  mk-zoom-auto =
    name: zoom-pkg:
    let
      my-script-deps = [
        zoom-pkg
        pkgs.wl-clipboard
      ];
    in
    pkgs.runCommandLocal name
      {
        nativeBuildInputs = [ pkgs.makeWrapper ];
      }
      ''
        install -m755 ${./zoom-auto.sh} -D $out/bin/${name}
        patchShebangs $out/bin/${name}
        wrapProgram "$out/bin/${name}" --prefix PATH : ${pkgs.lib.makeBinPath my-script-deps}
      '';
  zoom-auto = mk-zoom-auto "zoom-auto" zoom-us;
  # zoom-auto-wl = mk-zoom-auto "zoom-auto-wl" "${pkgs.cage}/bin/cage -- ${zoom-us}/bin/zoom-us"; # does not work
in
{
  config = {
    nixpkgs.overlays = [ zoom-us-overlay ];
    home-manager.sharedModules = [
      {
        home.packages = [
          zoom-us
          zoom-auto
        ];
        xdg.mimeApps = {
          defaultApplications."x-scheme-handler/zoommtg" = [ "us.zoom.Zoom.desktop" ];
        };
        myconfig.persistence.work-directories = [ ".zoom/data" ];
        myconfig.persistence.work-files = [
          ".config/zoom.conf"
          ".config/zoomus.conf"
        ];
      }
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "zoom-us"
      "zoom-auto"
    ];
  };
}
