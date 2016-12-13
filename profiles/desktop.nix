{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
  # terminal
    rxvt_unicode_with-plugins rxvt_unicode.terminfo
  # gui related
    slock dmenu unclutter
    arandr
    xlibs.xmodmap xlibs.xset xlibs.setxkbmap
    xclip
  # gui applications
    feh scrot
    chromium firefox-unwrapped luakit
    mupdf zathura llpp
    mplayer
    gnome3.file-roller
  # misc
    xf86_input_wacom
  ] ++ (with pkgs.haskellPackages; [
    xmonad xmobar yeganesh
  ]);

  services = {
    xserver = {
      enable = true;
      autorun = true;
      layout = "de"; # TODO: neo
      xkbOptions = "neo";
      enableCtrlAltBackspace = true;

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
        # i3.enable = true;
        default = "xmonad";
      };

      desktopManager = {
        xterm.enable = false;
        default = "none";
      };

      displayManager = {
        slim = {
          enable = true;
          defaultUser = "mhuber";
        };
        sessionCommands = ''
          ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
          ${pkgs.xlibs.setxkbmap}/bin/setxkbmap de neo
          if test -e $HOME/.Xresources; then
            ${pkgs.xorg.xrdb}/bin/xrdb --merge $HOME/.Xresources
          fi
          if test -e $HOME/.background-image; then
            ${pkgs.feh}/bin/feh --bg-center $HOME/.background-image
          fi
          ${pkgs.rxvt_unicode_with-plugins}/bin/urxvtd -q -f -o &
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
  };
}
