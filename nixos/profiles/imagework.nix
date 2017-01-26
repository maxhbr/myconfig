{ config, pkgs, ... }:

let
  unstable = (import <unstable> {});
in {
  environment.systemPackages = with pkgs; [
    unstable.gimp-with-plugins
    unstable.rawtherapee
    unstable.geeqie

    blender
    librecad

    inkscape
  ];
}
