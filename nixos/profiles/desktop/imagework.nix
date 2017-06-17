{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gimp-with-plugins
    rawtherapee
    geeqie
    #krita

    inkscape

    blender
    librecad
  ];
}
