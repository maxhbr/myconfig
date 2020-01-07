# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        git git-lfs
        gitAndTools.tig
        pass-git-helper
      ];
      home.file = {
        ".gitconfig".source = ./gitconfig;
      };
      programs.git = {
        package = pkgs.gitAndTools.gitFull;
        enable = true;
      };
    };
  };
}
