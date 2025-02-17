# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# TODO: package scripts
#
{ config, pkgs, ... }: {
  config = {
    environment = {
      shellAliases = {
        g = "git";
        pushb = "git pushb";
        pushfb = "git pushfb";
        gs = "git s";
        t = "tig";
        tu = "tig HEAD @{upstream}";
      };
    };
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [ github-cli ] ++ (with pkgs.gitAndTools; [
          tig
          pkgs.git-lfs
          git-fame
          git-gone
          git-absorb
          git-crypt
          git-secrets
        ]);
      home.file = { ".gitconfig".source = ./gitconfig; };
      programs.git = {
        package = pkgs.gitAndTools.gitFull;
        enable = true;
        signing = {
          key = null;
          signByDefault = true;
          format = if config.programs.gnupg.agent.enable then "openpgp" else "ssh";
        };
      };
      programs.fish = {
        shellAbbrs = { g = "git"; };
        functions = {
          gitignore = "curl -sL https://www.gitignore.io/api/$argv";
        };
      };
    }];
  };
}
