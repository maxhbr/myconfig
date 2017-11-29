{ config, lib, pkgs, ... }:
{
  options = {
    myconfig.roles.desktop = {
      enable = lib.mkEnableOption "Desktop environment";
    };
    myconfig.roles.xmonad = {
      enable = lib.mkEnableOption "Xmonad Desktop environment";
    };
    myconfig.roles.xfce = {
      enable = lib.mkEnableOption "Xfce desktop environment";
    };
    myconfig.roles.vnc = {
      enable = lib.mkEnableOption "VNC desktop environment";
    };
  };

  imports = [
################################################################################
    { # desktop
      config = lib.mkIf config.myconfig.roles.desktop.enable {
        environment.systemPackages = with pkgs; [
          arandr xrandr-invert-colors
          xlibs.xmodmap xlibs.xset xlibs.setxkbmap
          xclip
          imagemagick
        # gui applications
          mupdf zathura llpp
          feh scrot
          xarchiver # gnome3.file-roller
          mplayer
          xdotool
        # gui applications
          chromium firefox-unwrapped qutebrowser
          google-chrome # for streaming and music
          browserpass
        # spellchecking
          aspell aspellDicts.de aspellDicts.en
        # misc
          xf86_input_wacom
        ];

        services = {
          xserver = {
            enable = true;
            autorun = true;
            layout = "de";
            xkbVariant = "neo";
            xkbOptions = "altwin:swap_alt_win";
            enableCtrlAltBackspace = true;

            displayManager = {
              slim = {
                enable = true;
                defaultUser = "mhuber";
                theme = ../static/slim-theme;
              };
              sessionCommands = ''
                ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name ${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ/cursors/left_ptr 32
                if test -e $HOME/.Xresources; then
                  ${pkgs.xorg.xrdb}/bin/xrdb --merge $HOME/.Xresources
                fi
                if test -e $HOME/.background-image; then
                  ${pkgs.feh}/bin/feh --bg-scale "$HOME/.background-image"
                fi
              '';
            };

          };

          redshift = {
            enable = true;
            latitude = "48.2";
            longitude = "10.8";
            # temperature.day = 5500;
            # temperature.night = 3500;
          };
        };

        fonts = {
          enableFontDir = true;
          enableGhostscriptFonts = true;
          fonts = with pkgs; [
            dejavu_fonts
            corefonts
            inconsolata
          ];
        };
      };
    }
################################################################################
    { # xmonad
      config = lib.mkIf (config.myconfig.roles.desktop.enable && config.myconfig.roles.xmonad.enable) {
        # myconfig.roles.desktop.enable = true;
        environment.systemPackages = let
            find-cursor = pkgs.callPackage ../../nix/pkgs/find-cursor {};
          in (with pkgs; [
              unstable.dmenu unstable.dzen2
              unclutter
              xss-lock
              libnotify # xfce.xfce4notifyd # notify-osd
              wmctrl
              find-cursor
            ]) ++ (with pkgs.haskellPackages; [
              xmobar
            ]) ++ (with pkgs.unstable.haskellPackages; [
              xmonad yeganesh
            ]);

        system.activationScripts.cleanupXmonadState =
        ''
          rm /home/mhuber/.xmonad/xmonad.state || true
        '';

        services.xserver = {
          windowManager = {
            xmonad = {
              enable = true;
              enableContribAndExtras = true;
            };
            default = "xmonad";
          };

          desktopManager = {
            xterm.enable = false;
            default = "none";
          };
        };
      };
    }
################################################################################
    { # xfce
      config = lib.mkIf (config.myconfig.roles.desktop.enable && config.myconfig.roles.xfce.enable) {
        services.xserver.desktopManager.xfce.enable = true;
      };
    }
################################################################################
    { # vnc
      config =lib.mkIf (config.myconfig.roles.desktop.enable && config.myconfig.roles.vnc.enable) {
        environment.systemPackages = with pkgs; [
          x11vnc
        ];
        networking.firewall.allowedUDPPorts = [ 5900 ];
        networking.firewall.allowedTCPPorts = [ 5900 ];
      };
    }
    {
      environment.systemPackages = with pkgs; [
        hplipWithPlugin
      ];
      services.urxvtd = {
        enable = true;
        # users = [ "mhuber" ];
        # urxvtPackage = pkgs.rxvt_unicode_with-plugins;
      };
    }
    {
      environment.systemPackages = with pkgs; [
        rxvt_unicode_with-plugins rxvt_unicode.terminfo
      ];
      services.printing = {
        enable = true;
        drivers = [ pkgs.gutenprint pkgs.hplip ];
      };
    }
  ];
}
