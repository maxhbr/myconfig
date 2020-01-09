# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

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
        ".zsh-nix-shell".source = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "master";
          sha256 = "0l41ac5b7p8yyjvpfp438kw7zl9dblrpd7icjg1v3ig3xy87zv0n"; # TODO: autoupdate
        };

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
