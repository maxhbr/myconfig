{ pkgs, lib, ... }: {
  config = lib.mkIf (pkgs ? openrct2Files) {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ openrct2 ];
      home.file = {
        ".local/share/openrct2" = {
          source = pkgs.openrct2Files; # defined in overlay
          recursive = true;
        };
      };
    };
  };
}
