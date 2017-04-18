{ config, pkgs, ... }:

{
  imports = [
    ./xmonad.nix
    ./urxvt.nix
    ./hosts.nix
    ./printing.nix
  ];

  environment.systemPackages = with pkgs; [
    desktopEnv
  # gui applications
    chromium firefox-unwrapped qutebrowser
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
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      corefonts
      inconsolata
      iosevka
    ];
  };
}
