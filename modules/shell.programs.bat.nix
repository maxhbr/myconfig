# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  config = {
    home-manager.sharedModules = [
      (let
        aliases = {
          cat = ''${pkgs.bat}/bin/bat --theme="Monokai Extended Light"'';
        };
      in {
        programs.bat = {
          enable = true;
          extraPackages = with pkgs.bat-extras; [ batdiff batman batgrep batwatch ];
          config = {
            pager = "less -FR";
            theme = "Monokai Extended Light";
          };
        };
        programs.bash.shellAliases = aliases;
        programs.zsh.shellAliases = aliases;
        programs.fish.shellAliases = aliases;
      })
    ];
  };
}
