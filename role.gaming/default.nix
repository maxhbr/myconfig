{ pkgs, config, ... }:
let user = config.myconfig.user;
in {
  imports = [
    ../role.desktop
    ./games.wine
    ./games.steam
  ];
  config = {
    home-manager.users."${user}" = {
      imports = [
        # ./games.dod.hm.nix
        ./games.doom3.hm.nix
        ./games.ktane.hm.nix
        ./games.lutris.hm.nix
        ./games.openrct2.hm.nix
        ./games.powder.hm.nix
        # ./games.retroarch.hm.nix
        ./games.starsector.hm.nix
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
