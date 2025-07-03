# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
{
  config = {
    nixpkgs.overlays = [
      (
        self: super:
        let
          wineSelf = with self.nixos-unstable; [
            wine
            winetricks
            playonlinux
          ];
          wineCfg = {
            wineBuild = "wineWow";
            gstreamerSupport = false;
          };
          wowWine = self.nixos-unstable.wine.override wineCfg;
          wowWinetricks = (self.nixos-unstable.winetricks.override { wine = wowWine; });
          wowPlayonlinux = (self.nixos-unstable.playonlinux.override { wine = wowWine; });
          wowLutris = (self.nixos-unstable.lutris.override { wine = wowWine; });
        in
        {
          wine = wowWine;
          winetricks = wowWinetricks;
          playonlinux = wowPlayonlinux;
          lustris = wowLutris;
          # cosmoteer = ( helper.wrap
          #   { name   = "cosmoteer";
          #     paths  = [ wget wowWine wowWinetricks ];
          #     script = builtins.readFile ./bin/cosmoteer.sh;
          #   }
          # );
        }
      )
    ];
    nixpkgs.config.permittedInsecurePackages = [
      "p7zip-16.02" # in winetricks
    ];

    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          wine
          winetricks
          playonlinux
        ];
      }
      # {home.packages = with pkgs; [ lutris ];}
    ];
    hardware.opengl.driSupport32Bit = true;
  };
}
