{ pkgs, lib, ... }:
lib.mkIf (pkgs ? openrct2Files) {
  home.packages = with pkgs; [ openrct2 ];
  home.file = {
    ".local/share/openrct2" = {
      source = pkgs.openrct2Files; # defined in overlay
      recursive = true;
    };
  };
}
