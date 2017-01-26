{ config, pkgs, ... }:

let
  unstable = (import <unstable> {});
in {
  imports = [
    ../pkgs/services/urxvtd.nix
    ./hosts.nix
  ];

  environment.systemPackages = with pkgs; [
  # terminal
    unstable.rxvt_unicode_with-plugins unstable.rxvt_unicode.terminfo
  # gui related
    arandr
    xlibs.xmodmap xlibs.xset xlibs.setxkbmap
    xclip
  # gui applications
    feh scrot
    unstable.chromium unstable.firefox-unwrapped luakit
    unstable.mupdf unstable.zathura unstable.llpp
    unstable.mplayer
    gnome3.file-roller
  # misc
    xf86_input_wacom
    xdotool
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    freetype_subpixel = pkgs.freetype.override {
      useEncumberedCode = true;
      useInfinality = false;
    };
  };

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
        if test -e $HOME/.Xresources; then
          ${pkgs.xorg.xrdb}/bin/xrdb --merge $HOME/.Xresources
        fi
        if test -e $HOME/.desktop-backgronud.png; then
          ${pkgs.feh}/bin/feh --bg-scale "$HOME/.background-image"
        fi
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

  security.setuidPrograms = [ "slock" ];
}
