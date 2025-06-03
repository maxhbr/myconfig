{ config, lib, ... }: {
  imports = [
    (lib.mkIf config.programs.steam.enable {
      myconfig.persistence.cache-directories = [ ".local/share/Steam" ];
    })
    (lib.mkIf config.programs.evolution.enable {
      myconfig.persistence.directories =
        [ ".local/share/evolution" ".config/evolution" ];
      myconfig.persistence.cache-directories = [ ".cache/evolution" ];
    })
    (lib.mkIf config.services.syncthing.enable {
      myconfig.persistence.directories = let
        folders = lib.mapAttrsToList (name: folder:
            if lib.hasPrefix "/home/mhuber/" name then
              lib.removePrefix "/home/mhuber/" name
            else
              folder.path) config.services.syncthing.settings.folders;
        in folders ++ ["syncthing"];
    })
  ];
  config = {
    home-manager.sharedModules = [
      ({ config, lib, ... }: {
        config = lib.mkIf config.programs.firefox.enable {
          myconfig.persistence.directories = [ ".mozilla" ];
        };
      })
      ({ config, lib, ... }: {
        config = { myconfig.persistence.directories = [ ".config/Signal" ]; };
      })
      ({ config, lib, ... }: {
        config = {
          myconfig.persistence.directories =
            [ ".config/Joplin" ".config/joplin-desktop" ];
        };
      })
      ({ config, lib, ... }: {
        config = {
          myconfig.persistence.directories = [ "Maildir/alfa" "Maildir/gmail" "Maildir/mail" ];
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
            "Maildir/tng"
            ".config/teams-for-linux"
            ".zoom/data"
            ".config/Slack"
          ];
          myconfig.persistence.work-files =
            [ ".config/zoom.conf" ".config/zoomus.conf" ];
          myconfig.persistence.cache-directories =
            [ ".config/Cursor" ".cursor" ];
        };
      })

    ];
  };
}
