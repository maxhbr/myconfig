# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./extrahosts
  ];

  config = {
    userPackages = with pkgs; [
      myconfig.background
    ];



    environment = {
      variables = {
        BROWSER = "${pkgs.chromium}/bin/chromium-browser";
      };
      systemPackages = with pkgs; [
        arandr
        xlibs.xmodmap xlibs.xset xlibs.setxkbmap
        xclip
        xarchiver
      # misc
        xf86_input_wacom
        libnotify # xfce.xfce4notifyd # notify-osd
        vanilla-dmz

        networkmanager_dmenu

        # gui applications
        mupdf zathura llpp
        feh imagemagick # scrot
        mplayer
        # gui applications
        chromium unstable.firefox qutebrowser
        google-chrome # for streaming and music
        # browserpass
        # spellchecking
        aspell aspellDicts.de aspellDicts.en
      ];
      interactiveShellInit = ''
        alias file-roller='${pkgs.xarchiver}/bin/xarchiver'
      '';
    };

    programs.light.enable = true;
    services.actkbd = {
      enable = true;
      bindings = [
        { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 10"; }
        { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 10"; }
      ];
    };

    services.printing = {
      enable = true;
      drivers = with pkgs; [ gutenprint hplip ];
      # add hp-printer with:
      # $ nix run nixpkgs.hplipWithPlugin -c sudo hp-setup
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
