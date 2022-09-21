# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { cad.enable = mkEnableOption "cad"; };
  config = (lib.mkIf cfg.cad.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        ([ # 2D
          # librecad # 2D
        ] ++ [ # 3D
          # blender
          openscad
          freecad # 3D
          nixos-2105.prusa-slicer
          # povray
          # luxcorerender
          fstl
          meshlab
          gmsh
        ] ++ [ # pcb design
          librepcb
          gerbv
          # kicad-small
          nixos-2205.kicad-with-packages3d
        ]);
    }];
  });
}
