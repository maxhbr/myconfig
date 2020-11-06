# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [ ../shell.common
              { # nix related config
                home-manager.users.mhuber = {
                  programs.fish = {
                    shellAbbrs = {
                      nixse = "nix search";
                      why-depends-nixos = "nix why-depends /run/current-system";
                      nixTest = "NIXPKGS_ALLOW_UNFREE=1 nix-shell '<nixpkgs-unstable>' --run fish -p";
                    };
                    functions = {
                      # nixRun = ''
                      # if [ "$#" -eq "2" ]; then
                      #   NIXPKGS_ALLOW_UNFREE=1 nix-shell '<nixpkgs-unstable>' -p "$1" --command "$2"
                      # else
                      #   NIXPKGS_ALLOW_UNFREE=1 nix-shell '<nixpkgs-unstable>' -p "$1" --command "$1"
                      # fi
                      # '';
                    };
                  };
                };
              }
              { # for git
                home-manager.users.mhuber = {
                  programs.fish = {
                    shellAbbrs = {
                      g = "git";
                    };
                    functions = {
                      gitignore = "curl -sL https://www.gitignore.io/api/$argv";
                    };
                  };
                };
              }
            ];
  config = {
    programs.fish = { enable = true; };
    environment = {
      shells = [ "${pkgs.fish}/bin/fish" "/run/current-system/sw/bin/fish" ];
    };
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ fasd fzf ];
      programs.fish = {
        enable = true;
        shellAliases = { };
        shellAbbrs = {
          rm = "rm -I";
          cp = "cp -i";
          mv = "mv -vi";
        };
        functions = {
          __fish_command_not_found_handler = {
            body = "__fish_default_command_not_found_handler $argv[1]";
            onEvent = "fish_command_not_found";
          };
          __list_dir_handler = {
            body = ''
# echo content after cd / z / or any pwd change
set linesInLs (ls -1GF | ${pkgs.coreutils}/bin/wc -l)
set linesInTerminal (${pkgs.ncurses}/bin/tput lines)
  if [  -lt  ]; then
    ls -GF
  fi
'';
            onVariable = "PWD";
          };
          whichf = "readlink -f (which $@)";
          cdtemp = "cd (mktemp -d); pwd";
          cptemp = ''
  set f (readlink -f $1)
  cd (mktemp -d)
  cp -r $f ./
  pwd
'';
          mvtemp = ''
  set f (readlink -f $1)
  cd (mktemp -d)
  mv $f ./
  pwd
'';
          ff = "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type f -iname '*'$argv'*' -ls 2>/dev/null";
          ffd = "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -type d -iname '*'$argv'*' -ls 2>/dev/null";
          ffa = "find . -not -iwholename '*.svn*' -not -iwholename '*.git*' -iname '*'$argv'*' -ls 2>/dev/null";
          ffg = "find . -type f -print0 | xargs -0 grep -lI $argv";
        };
        shellInit = "";
        loginShellInit = "";
        interactiveShellInit = ''
set -U fish_greeting

# see: https://fishshell.com/docs/current/#command-line-editor
function hybrid_bindings --description "Vi-style bindings that inherit emacs-style bindings in all modes"
    for mode in default insert visual
        fish_default_key_bindings -M $mode
    end
    fish_vi_key_bindings --no-erase
end
set -g fish_key_bindings hybrid_bindings
        '';
        promptInit = ''
          set -l nix_shell_info (
            if test -n "$IN_NIX_SHELL"
              echo -n "<nix-shell> "
            end
          )
        '';
        plugins = [
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
          {
            name = "fzf";
            src = pkgs.fetchFromGitHub {
              owner = "jethrokuan";
              repo = "fzf";
              rev = "c2895ba0a83543f6aa29086989d0b735c7943067";
              sha256 = "1ydx0gv4mdnik8d4ymyckz9zkmiikq4xi0h4agca3sng87skwmw8";
            };
          }
          { name = "done";
            src = builtins.fetchGit { url = "https://github.com/franciscolourenco/done"; };
          }
          { name = "fish-async-prompt";
            src = builtins.fetchGit { url = "https://github.com/acomagu/fish-async-prompt"; };
          }
          { name = "fish-ssh-agent";
            src = builtins.fetchGit { url = "https://github.com/danhper/fish-ssh-agent"; };
          }
        ];
      };
      home.file = {
        ".config/fish/functions/fish_prompt.fish".source = let
          src = pkgs.fetchFromGitHub {
            owner = "isacikgoz";
            repo = "sashimi";
            rev = "1f85f6f33be9079cff9a8798ac6f91a319ba3e40";
            sha256 = "04968zdxby1ggsafv8gf1qnzz56p8y4xd25xd2rhbkpnbyghbh83";
          };
        in src + "/fish_prompt.fish";
        ".config/fish/functions/bax.fish".source = let
            src = builtins.fetchGit { url = "https://github.com/jorgebucaran/bax.fish"; };
          in src + "/bax.fish";
      };
    };
  };
}
