# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
{
  config = {
    home-manager.imports = [{
      home.file = {
        ".vim" = {
          source = ./vim;
          recursive = true;
        };
        # ".vimrc".source = ./vimrc;
      };
      programs.vim = {
        enable = true;
        extraConfig = builtins.readFile ./vimrc;
        settings = { number = true; };
        plugins = with pkgs.vimPlugins; [ ];
      };
    }];
  };
}
