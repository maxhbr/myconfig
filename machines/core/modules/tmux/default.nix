# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, pkgs, ... }:
let
  inherit (builtins) readFile concatStringsSep;

  inherit (lib) removePrefix;

  pluginConf = plugins:
    concatStringsSep "\n\n" (map (plugin:
      let name = removePrefix "tmuxplugin-" plugin.name;
      in "run-shell ${plugin}/share/tmux-plugins/${name}/${name}.tmux")
      plugins);

  plugins = with pkgs.tmuxPlugins; [
    copycat
    open
    resurrect
    yank
    vim-tmux-navigator
  ];
in {
  environment = {
    shellAliases = {
      tx = "tmux new-session -A -s $USER";
      tmx = "tmux attach-session || tmux";
      tmux = "tmux -2";
    };
    interactiveShellInit = ''
      ntmx(){
        # name sessions automatically, depending on the current dir
        n=$(echo $(basename "$(pwd)") | sed "s/\./_/g")
        tmux attach-session -t "$n" || tmux new -s "$n"
      }
      if test -z $TMUX && [[ $TERM != "screen" ]]; then
        if [ -n "$BASH_VERSION" ]; then
          if [[ $- == *i* ]]; then
            bind '"\C-n":" ntmx\C-m"'
          fi
        elif [ -n "$ZSH_VERSION" ]; then
          bindkey -s '^n' '^qntmx\n'
        fi
      fi
    '';
  };
  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    escapeTime = 10;
    historyLimit = 5000;
    keyMode = "vi";
    shortcut = "a";
    terminal = "tmux-256color";
    baseIndex = 1;
    extraTmuxConf = ''
      ${readFile ./tmuxline.conf}
      ${readFile ./tmux.conf}
      ${pluginConf plugins}
    '';
  };
}
