# see: https://wiki.ubuntuusers.de/Spiele/Doom_3/
{ pkgs, lib, ... }:
lib.mkIf (pkgs ? dhewm3Files) {
  home.packages = with pkgs; [ dhewm3 ];
  home.file = {
    ".local/share/dhewm3" = {
      source = pkgs.dhewm3Files; # defined in overlay
      recursive = true;
    };
  };
}
