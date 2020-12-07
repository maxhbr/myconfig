{pkgs, config, lib, ...}:
let
  user = config.myconfig.user;
in {
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.users."${user}" = {
      programs.firefox.enable = true;
      programs.chromium.enable = true;

      home.packages = with pkgs;
        [
          # misc
          libnotify # xfce.xfce4notifyd # notify-osd

          # gui applications
          mupdf
          zathura
          llpp
          xarchiver
          feh
          imagemagick
          mplayer
          qutebrowser
          tdesktop
          # spellchecking
          aspell
          aspellDicts.de
          aspellDicts.en
        ] ++ lib.optional config.networking.networkmanager.enable
        networkmanager_dmenu;
      xdg.mimeApps = {
        enable = true;
        defaultApplications."application/pdf" = [ "mupdf.desktop" ];
        defaultApplications."image/jpeg" = [ "sxiv.desktop" ];
        defaultApplications."image/png" = [ "sxiv.desktop" ];
        defaultApplications."x-scheme-handler/slack" = [ "slack.desktop" ];
        defaultApplications."x-scheme-handler/zoommtg" =
          [ "us.zoom.Zoom.desktop" ];
      };
    };
    environment = {
      shellAliases = {
        file-roller = "${pkgs.xarchiver}/bin/xarchiver";
      };
    };
  });
}
