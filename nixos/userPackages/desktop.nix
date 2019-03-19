# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs }:
{
  inherit (pkgs)
    arandr
    xclip
  # gui applications
    mupdf zathura llpp
    feh imagemagick # scrot
    xarchiver # gnome3.file-roller
    mplayer
  # gui applications
    chromium firefox-unwrapped qutebrowser
    google-chrome # for streaming and music
    # browserpass
  # spellchecking
    aspell;
  inherit (pkgs.aspellDicts)
    de en;
  inherit (pkgs.xlibs)
    xmodmap xset setxkbmap;
}
