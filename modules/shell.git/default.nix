# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# TODO: package scripts
#
{ config, pkgs, ... }:
let user = config.myconfig.user;
in {
  config = {
    environment = {
      shellAliases = {
        g = "git";
        gs = "git s";
        t = "tig";
        tu = "tig HEAD @{upstream}";
      };
    };
    home-manager.users."${user}" = {
      home.packages = with pkgs;
        [ github-cli ] ++ (with pkgs.gitAndTools; [
          tig
          pkgs.git-lfs
          git-fame
          git-gone
          git-absorb
        ]);
      home.file = { ".gitconfig".source = ./gitconfig; };
      programs.git = {
        package = pkgs.gitAndTools.gitFull;
        enable = true;
      };
    };
  };
}
