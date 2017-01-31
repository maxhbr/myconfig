{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gimp-with-plugins
    rawtherapee
    geeqie

    blender
    librecad

    inkscape
  ];
}
