{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gimp
    rawtherapee
    geeqie

    blender
    librecad

    inkscape
  ];
}
