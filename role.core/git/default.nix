# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

# TODO: package scripts
{
  config = {
    environment = {
      shellAliases = {
        g = "git";
        gs = "git s";
        t = "tig";
        tu = "tig HEAD @{upstream}";
      };
    };
    home-manager.users.mhuber = {
      home.packages = with pkgs;
        [ github-cli ]
        ++ (with pkgs.gitAndTools; [ tig pkgs.git-lfs git-fame git-gone ]);
      home.file = { ".gitconfig".source = ./gitconfig; };
      programs.git = {
        package = pkgs.gitAndTools.gitFull;
        enable = true;
      };
    };
  };
}
