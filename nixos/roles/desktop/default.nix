{ config, lib, pkgs, ... }:
let
  unstable = (import <unstable> {});
in {
  options = {
    myconfig.roles.desktop = {
      enable = lib.mkEnableOption "Desktop environment";
    };
  };

  imports = [
    ./xmonad.nix
    ./xfce.nix
    ./vnc.nix
  ];

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
    # spellchecking
      aspell aspellDicts.de aspellDicts.en
    # misc
      xf86_input_wacom
    # rxvt
      rxvt_unicode_with-plugins rxvt_unicode.terminfo
    # printing
      hplipWithPlugin
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
            theme = ../../static/slim-theme;
          };
          sessionCommands = ''
            ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
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


      printing = {
        enable = true;
        drivers = [ pkgs.gutenprint pkgs.hplip ];
      };

      urxvtd = {
        enable = true;
        # users = [ "mhuber" ];
        # urxvtPackage = pkgs.rxvt_unicode_with-plugins;
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
