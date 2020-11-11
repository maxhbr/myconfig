{ pkgs, config, ... }:
let user = config.myconfig.user;
in {
  imports = [
    ../role.desktop
    ./games.wine
    ./games.doom3.nix
    ./games.ktane.nix
    ./games.starsector.nix
    ./games.openrct2.nix
    ./games.powder.nix
    ./games.steam
  ];
  config = {
    home-manager.users."${user}" = {
      imports = [
        ./games.lutris.nix
      ];
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
