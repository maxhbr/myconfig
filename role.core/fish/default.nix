# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [ ../shell.common ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ fasd ];
      programs.fish = {
        enable = true;
        shellAliases = { };
        shellAbbrs = { };
        functions = {
          gitignore = "curl -sL https://www.gitignore.io/api/$argv";
        };
        shellInit = "";
        loginShellInit = "";
        interactiveShellInit = ''
          set -U fish_greeting
        '';
        promptInit = ''
          set -l nix_shell_info (
            if test -n "$IN_NIX_SHELL"
              echo -n "<nix-shell> "
            end
          )
        '';
        plugins = [
          { # sashimi prompt from https://github.com/isacikgoz/sashimi
            name = "sashimi";
            src = pkgs.fetchFromGitHub {
              owner = "isacikgoz";
              repo = "sashimi";
              rev = "1f85f6f33be9079cff9a8798ac6f91a319ba3e40";
              sha256 = "04968zdxby1ggsafv8gf1qnzz56p8y4xd25xd2rhbkpnbyghbh83";
            };
          }
          {
            name = "fasd";
            src = pkgs.fetchFromGitHub {
              owner = "oh-my-fish";
              repo = "plugin-fasd";
              rev = "38a5b6b6011106092009549e52249c6d6f501fba";
              sha256 = "06v37hqy5yrv5a6ssd1p3cjd9y3hnp19d3ab7dag56fs1qmgyhbs";
            };
          }
          {
            name = "foreign-env";
            src = pkgs.fetchFromGitHub {
              owner = "oh-my-fish";
              repo = "plugin-foreign-env";
              rev = "dddd9213272a0ab848d474d0cbde12ad034e65bc";
              sha256 = "00xqlyl3lffc5l0viin1nyp819wf81fncqyz87jx8ljjdhilmgbs";
            };
          }
          {
            name = "tmux";
            src = pkgs.fetchFromGitHub {
              owner = "oh-my-fish";
              repo = "plugin-tmux";
              rev = "7d43f4e0959649fb2367c1525da8a0bd9075b723";
              sha256 = "03a5lrbgd4rsxqw3qir6hfjxg20qdzgy2095w76s1p2pwqhkr48j";
            };
          }
          {
            name = "z";
            src = pkgs.fetchFromGitHub {
              owner = "jethrokuan";
              repo = "z";
              rev = "78861a85fc4da704cd7d669c1133355c89a4c667";
              sha256 = "1ffjihdjbj3359hjhg2qw2gfx5h7rljlz811ma0a318nkdcg1asx";
            };
          }
        ];
      };
    };
    programs.fish = { enable = true; };
    environment = {
      shells = [ "${pkgs.fish}/bin/fish" "/run/current-system/sw/bin/fish" ];
    };
  };
}
