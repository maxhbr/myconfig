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
  environment.shellAliases = {
    tx = "tmux new-session -A -s $USER";
    tmx = "tmux attach-session || tmux";
  };
  # TODO: ntmx with keybindings

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
