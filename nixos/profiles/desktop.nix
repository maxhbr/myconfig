{ config, pkgs, ... }:

{
  imports = [
    ../pkgs/urxvtd.nix
  ];

  environment.systemPackages = with pkgs; [
  # terminal
    rxvt_unicode_with-plugins rxvt_unicode.terminfo
  # gui related
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
  ];

  services = {
    xserver = {
      enable = true;
      autorun = true;
      layout = "de";
      xkbVariant = "neo";
      xkbOptions = "altwin:swap_alt_win";
      enableCtrlAltBackspace = true;

      displayManager.sessionCommands = ''
        ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
        # ${pkgs.xlibs.setxkbmap}/bin/setxkbmap de neo
        if test -e $HOME/.Xresources; then
          ${pkgs.xorg.xrdb}/bin/xrdb --merge $HOME/.Xresources
        fi
        if test -e $HOME/.desktop-backgronud.png; then
          ${pkgs.feh}/bin/feh --bg-scale "$HOME/.desktop-backgronud.png"
        fi
        # ${pkgs.rxvt_unicode_with-plugins}/bin/urxvtd -q -f -o &
      '';
    };

    redshift = {
      enable = true;
      latitude = "48.2";
      longitude = "10.8";
      # temperature.day = 5500;
      # temperature.night = 3500;
    };

    urxvtd = {
      enable = true;
      users = [ "mhuber" ];
      urxvtPackage = pkgs.rxvt_unicode_with-plugins;
    };
  };
}
