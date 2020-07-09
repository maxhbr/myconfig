{...}: {
  imports = [
    ./games.powder.nix
    ./games.steam
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
