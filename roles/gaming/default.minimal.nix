{...}: {
  imports = [
    ./games.powder.nix
    ./games.steam
    # ./games.ktane.nix
  ];
  config =
    { home-manager.users.mhuber =
        { home.file =
            { "bin" =
                { source = ./bin;
                  recursive = true;
                };
            };
        };
    };
}
