{pkgs, ...}: {
  imports =
    [ ./default.minimal.nix
      ./games.wine
      ./games.doom3.nix
      ./games.lutris.nix
    ];
  config =
    { home-manager.users.mhuber =
        { home.packages = with pkgs;
            [ openra
              openrct2
              xonotic
            ];
        };
    };
}
