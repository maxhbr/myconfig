# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./extrahosts
  ];

  config = {

    environment = {
      systemPackages = with pkgs; [
        arandr
        xlibs.xmodmap xlibs.xset xlibs.setxkbmap
        xclip
      # misc
        xf86_input_wacom
        libnotify # xfce.xfce4notifyd # notify-osd
        vanilla-dmz
      ];
      interactiveShellInit = ''
        alias file-roller='${pkgs.xarchiver}/bin/xarchiver'
      '';
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
