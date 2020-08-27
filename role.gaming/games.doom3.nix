{ pkgs, lib, ... }:
# see: https://wiki.ubuntuusers.de/Spiele/Doom_3/
{ config = lib.mkIf (lib.isDerivation pkgs.dhewm3Files) {
    home-manager.users.mhuber =
      { home.packages = with pkgs; [ dhewm3 ];
        home.file =
          { ".local/share/dhewm3" =
              { source = pkgs.dhewm3Files; # defined in overlay
                recursive = true;
              };
          };
      };
  };
}
