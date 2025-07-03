{ pkgs, ... }:
let
  dod = pkgs.callPackage ./pkgs/DungeonsofDredmor { };
in
{
  home.packages = [ dod ];
}
