# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  pkgs,
  config,
  myconfig,
  ...
}:
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

  tmux-switch-client = pkgs.writeShellApplication {
    name = "tmux-switch-client.sh";
    runtimeInputs = with pkgs; [
      tmux
      gawk
      coreutils
    ];
    text = lib.fileContents ./tmux-switch-client.sh;
  };

  host-tmux-session-script = let
      hostname = config.networking.hostName;
      host-tmux-session = "host-tmux-session";
      nvtopCmd = if config.myconfig.hardware.gpu.variant != [ ] then ''
        tmux send-keys -t ${host-tmux-session}:1 "nvtop -i" C-m
        tmux split-window -v -t ${host-tmux-session}
      '' else "echo 'No nvtop'";
    in pkgs.writeShellScriptBin host-tmux-session ''
    # if session is not yet created, create it
    if ! tmux has-session -t ${host-tmux-session}; then
      tmux new-session -d -s ${host-tmux-session}
      tmux send-keys -t ${host-tmux-session}:1 "btop" C-m
      tmux split-window -h -t ${host-tmux-session}
      ${nvtopCmd}
      tmux send-keys -t ${host-tmux-session}:1 "journalctl -f" C-m
      tmux split-window -v -t ${host-tmux-session}
    fi
    if [[ -n "$TMUX" ]]; then
      tmux switch-client -t ${host-tmux-session}
    else
      exec tmux attach-session -t ${host-tmux-session}
    fi
  '';
in
{
  home-manager.sharedModules = [
    {
      home.packages = [ tmux-switch-client host-tmux-session-script ];
    }
  ];
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
