# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ oh-my-zsh ];
      home.file = {
        ".zshrc".source = ./zshrc;
        ".zprofile".source = ./zprofile;
        ".profile".source = ./zprofile;
        ".zshrc.pre-oh-my-zsh".source = ./zshrc.pre-oh-my-zsh;
        ".aliasrc".source = ./aliasrc;
      };
    };
    environment = {
      shells = [
        "${pkgs.zsh}/bin/zsh"
        "/run/current-system/sw/bin/zsh"
      ];
    };

    programs.zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      ohMyZsh = {
        enable = true;
        plugins = ["git"];
      };
      promptInit = ""; # Clear this to avoid a conflict with oh-my-zsh
    };
  };
}
