# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        my-zoom-us = pkgs.master.zoom-us.overrideAttrs (old: {
          postFixup = let
            os-release = super.writeText "os-release" ''
              PRETTY_NAME="Debian GNU/Linux 10 (buster)"
              NAME="Debian GNU/Linux"
              VERSION_ID="10"
              ID=debian
            '';
          in old.postFixup + ''
            wrapProgram $out/bin/zoom-us --unset XDG_SESSION_TYPE
            wrapProgram $out/bin/zoom --unset XDG_SESSION_TYPE
            echo "${super.bubblewrap}/bin/bwrap --dev-bind / / --ro-bind ${os-release} /etc/os-release $out/bin/.zoom-us-wrapped" > $out/bin/bw-zoom-us
            chmod +x $out/bin/bw-zoom-us
          '';
        });
      })
    ];
    home-manager.sharedModules = [{
      home.packages = with pkgs; [ my-zoom-us ];
      xdg.mimeApps = {
        defaultApplications."x-scheme-handler/zoommtg" =
          [ "us.zoom.Zoom.desktop" ];
      };
    }];
  };
}
