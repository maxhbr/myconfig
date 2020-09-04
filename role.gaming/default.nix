{ pkgs, ... }: {
  imports = [
    ../role.desktop
    ./games.wine
    ./games.doom3.nix
    ./games.lutris.nix
    ./games.ktane.nix
    ./games.openrct2.nix
    ./games.powder.nix
    ./games.steam
  ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs;
        [
          openra
          # xonotic
        ];
      home.file = {
        "bin" = {
          source = ./bin;
          recursive = true;
        };
      };
    };
  };
}
