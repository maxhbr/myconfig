{pkgs,unstable}:
with pkgs; let
  name = "imageworkEnv";
  paths = [
    gimp-with-plugins
    rawtherapee
    geeqie

    blender
    librecad

    inkscape
  ];
in buildEnv { inherit name paths; }
