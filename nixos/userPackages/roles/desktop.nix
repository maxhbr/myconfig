#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
pkgs:
with pkgs; [
# gui applications
  mupdf zathura llpp
  feh imagemagick # scrot
  xarchiver # gnome3.file-roller
  mplayer
# gui applications
  chromium unstable.firefox-unwrapped qutebrowser
  google-chrome # for streaming and music
  # browserpass
# spellchecking
  aspell aspellDicts.de aspellDicts.en
]
