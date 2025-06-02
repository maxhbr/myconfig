{ pkgs, config, lib, myconfig, inputs, ... }: let {
  imports = [
    (lib.mkIf config.programs.steam.enable {
      myconfig.persistence.cache-directories = [ ".local/share/Steam" ];
    })
    (lib.mkIf config.programs.evolution.enable {
      myconfig.persistence.directories =
        [ ".local/share/evolution" ".config/evolution" ];
      myconfig.persistence.cache-directories = [ ".cache/evolution" ];
    })
  ];
  config = {
    home-manager.sharedModules = [
      ({ config, lib, ... }:
        lib.mkIf config.programs.firefox.enable {
          myconfig.persistence.directories = [ ".mozilla" ];
        })
      ({ config, lib, ... }: {
        myconfig.persistence.directories =
          [ ".config/Signal" ".config/Joplin" ".config/joplin-desktop" ];
      })
      ({ config, lib, ... }:
        lib.mkIf config.programs.chromium.enable {
          myconfig.persistence.directories = [ ".config/chromium" ];
        })
      ({ config, lib, ... }:
        lib.mkIf config.programs.teams.enable {
          myconfig.persistence.work-directories = [
            "TNG"
            "Maildir/tng"
            ".config/teams-for-linux"
            ".zoom/data"
            ".config/Slack"
          ];
          myconfig.persistence.work-files =
            [ ".config/zoom.conf" ".config/zoomus.conf" ];
          config.myconfig.persistence.cache-directories =
            [ ".config/Cursor" ".cursor" ];
        })
    ];
  };
}
