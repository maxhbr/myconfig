# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, myconfig, ... }: {
  config = {
    home-manager.sharedModules = [
      ({pkgs, lib, config, ...}:{
        config = lib.mkIf config.programs.bat.enable (let
        theme = "Monokai Extended Light";
        aliases = {
          cat = ''${pkgs.bat}/bin/bat --theme="${theme}"'';
        };
      in 
        {
        programs.bat = {
          extraPackages = with pkgs.bat-extras; [
            # batdiff # fails to compile
            # batman # broken
            # batgrep # broken
            # batwatch
          ];
          config = {
            pager = "less -FR";
            inherit theme;
          };
        };
        programs.bash.shellAliases = aliases;
        programs.zsh.shellAliases = aliases;
        programs.fish.shellAliases = aliases;
      });
    })
    {
      programs.bat.enable = true;
    }
    ];
  };
}
