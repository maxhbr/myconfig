{pkgs, ...}: {
  imports =
    [ ../role.gaming.minimal
      ./games.wine
      ./games.doom3.nix
      ./games.lutris.nix
      ./games.ktane.nix
      ./games.openrct2.nix
    ];
  config =
    { home-manager.users.mhuber =
        { home.packages = with pkgs;
            [ openra
              # xonotic
            ];
        };
    };
}
