{ pkgs, config, lib, myconfig, inputs, ... }:
{
  imports = [
    ./clean_home.nix
  ];
  config = {
    myconfig.persistence.impermanence.enable = true;
    home-manager.sharedModules = [
      ({ config, lib, ... }: {
        config = {
          myconfig.persistence.directories =
            [ ".config/Joplin" ".config/joplin-desktop" ];
        };
      })
      ({ config, lib, ... }: {
        config = lib.mkIf config.programs.chromium.enable {
          myconfig.persistence.directories = [ ".config/chromium" ];
        };
      })
      ({ config, lib, ... }: {
        config = {
          myconfig.persistence.work-directories = [
            "TNG"
          ];
        };
      })
    ];
  };
}