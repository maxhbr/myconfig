# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, pkgs, config, myconfig, ... }:
let
  user = myconfig.user;
  inherit (builtins) readFile concatStringsSep;

  inherit (lib) removePrefix;

  pluginConf =
    plugins:
    concatStringsSep "\n\n" (
      map (
        plugin:
        let
          name = removePrefix "tmuxplugin-" plugin.name;
        in
        "run-shell ${plugin}/share/tmux-plugins/${name}/${name}.tmux"
      ) plugins
    );

  plugins = with pkgs.tmuxPlugins; [
    copycat
    open
    # resurrect
    yank
    vim-tmux-navigator
  ];

in
{
  nixpkgs.overlays = [ (final: prev: { tmux = final.master.tmux; }) ];
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
    newSession = true;
    escapeTime = 10;
    historyLimit = 5000;
    keyMode = "vi";
    shortcut = "a";
    terminal = "tmux-256color";
    baseIndex = 1;
    extraConfig = ''
      ${readFile ./tmuxline.conf}
      ${readFile ./tmux.conf}
      ${pluginConf plugins}
    '';
  };
}
