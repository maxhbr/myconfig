# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { cad.enable = mkEnableOption "cad"; };
  config = (lib.mkIf cfg.cad.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        # blender
        # librecad # 2D
        openscad
        # freecad # 3D
        prusa-slicer
        povray
        luxcorerender
        meshlab
        gmsh
        # pcb design
        librepcb
      ];
    }];
  });
}
