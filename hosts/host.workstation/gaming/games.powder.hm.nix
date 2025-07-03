{ pkgs, config, ... }:
let
  powder = pkgs.callPackage ./pkgs/powder { };
in
{
  home.packages = [ powder ];
}
