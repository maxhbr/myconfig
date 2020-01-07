# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./extrahosts
  ];

  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        my-backgrounds

        # misc
        libnotify # xfce.xfce4notifyd # notify-osd

        networkmanager_dmenu

        # gui applications
        mupdf zathura llpp
        xarchiver
        feh imagemagick # scrot
        mplayer
        chromium unstable.firefox qutebrowser
        google-chrome # for streaming and music
        # spellchecking
        aspell aspellDicts.de aspellDicts.en
      ];
    };

    environment = {
      variables = {
        BROWSER = "${pkgs.chromium}/bin/chromium-browser";
      };
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
