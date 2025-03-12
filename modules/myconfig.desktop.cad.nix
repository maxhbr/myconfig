# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { desktop.cad.enable = mkEnableOption "cad"; };
  config = (lib.mkIf cfg.desktop.cad.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        ([ # 2D
          # librecad # 2D
        ] ++ [ # 3D
          # blender
          openscad-unstable
          freecad # 3D
          povray # raytracing for freecad
          prusa-slicer
          # luxcorerender
          fstl
          meshlab
          gmsh
        ] ++ [ # pcb design
          # librepcb
          # gerbv
          # kicad-small
          # kicad
          jre # for freerouting
        ]);
    }];
  });
}
