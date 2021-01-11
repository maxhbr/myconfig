# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  aliases = { cat = ''${pkgs.bat}/bin/bat --theme="Monokai Extended Light"''; };
in {
  programs.bat = {
    enable = true;
    config = {
      pager = "less -FR";
      theme = "Monokai Extended Light";
    };
  };
  programs.bash.shellAliases = aliases;
  programs.zsh.shellAliases = aliases;
  programs.fish.shellAliases = aliases;
}
