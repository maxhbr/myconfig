# see: https://wiki.ubuntuusers.de/Spiele/Doom_3/
{ pkgs, config, lib, ... }:
let user = config.myconfig.user;
in {
  config = lib.mkIf (pkgs ? dhewm3Files) {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [ dhewm3 ];
      home.file = {
        ".local/share/dhewm3" = {
          source = pkgs.dhewm3Files; # defined in overlay
          recursive = true;
        };
      };
    };
  };
}
