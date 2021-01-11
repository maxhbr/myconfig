{ pkgs, config, lib, ... }:
let
  user = config.myconfig.user;
  myInvert = with pkgs;
    writeScriptBin "myInvert" ''
      #!${stdenv.shell}
      ${systemd}/bin/systemctl --user stop redshift
      ${xrandr-invert-colors}/bin/xrandr-invert-colors
    '';
in {
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.users."${user}" = {
      programs.firefox.enable = lib.mkDefault true;
      programs.zathura.enable = lib.mkDefault true;

      home.packages = with pkgs;
        [
          arandr
          xlibs.xmodmap
          xlibs.xset
          xlibs.setxkbmap
          xorg.xkill
          xorg.xmessage
          xclip
          xdotool
          xrandr-invert-colors
          myInvert

          # misc
          libnotify # xfce.xfce4notifyd # notify-osd

          # gui applications
          mupdf
          llpp
          xarchiver
          feh
          imagemagick
          mplayer
          qutebrowser
          google-chrome # for netflix and stadia
          tdesktop
          # spellchecking
          aspell
          aspellDicts.de
          aspellDicts.en
        ] ++ lib.optional config.networking.networkmanager.enable
        networkmanager_dmenu;
      xresources.extraConfig = ''
        *utf8: 1

        !! Xft
        Xft.autohint: 0
        Xft.lcdfilter: lcddefault
        Xft.hintstyle: hintfull
        Xft.hinting: 1
        Xft.antialias: 1
        Xft.rgba: rgb

        !! Xterm
        xterm*loginShell: true
      '';
      xdg.mimeApps = {
        enable = true;
        defaultApplications."image/jpeg" = [ "sxiv.desktop" ];
        defaultApplications."image/png" = [ "sxiv.desktop" ];
        defaultApplications."x-scheme-handler/slack" = [ "slack.desktop" ];
        defaultApplications."x-scheme-handler/zoommtg" =
          [ "us.zoom.Zoom.desktop" ];
      };
    };
    environment = {
      shellAliases = { file-roller = "${pkgs.xarchiver}/bin/xarchiver"; };

      interactiveShellInit = ''
        xclipToX() {
          ${pkgs.xclip}/bin/xclip <(${pkgs.xclip}/bin/xclip -selection c -o)
        }

        xclipToCtrl() {
          ${pkgs.xclip}/bin/xclip -selection c <(${pkgs.xclip}/bin/xclip -o)
        }
      '';
    };

    services = {
      xserver = {
        autorun = true;
        enableCtrlAltBackspace = true;

        displayManager = {
          lightdm = {
            enable = true;
            background = "${pkgs.my-wallpapers}/share/romben3.png";
          };
          sessionCommands = ''
            ${pkgs.my-wallpapers}/bin/myRandomBackground &disown
          '';
        };
      };

      cron = {
        enable = true;
        systemCronJobs = [
          "*/10 * * * *  ${user} ${pkgs.my-wallpapers}/bin/myRandomBackground >> /tmp/cronout 2>&1"
        ];
      };

      redshift = { enable = true; };
    };
  });
}
