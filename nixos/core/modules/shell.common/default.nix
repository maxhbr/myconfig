# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  imports = [
    ../tmux
  ];
  config = {
    home-manager.users.mhuber = {
      programs.bat = {
        enable = true;
        config = {
          pager = "less -FR";
          theme = "Monokai Extended Light";
        };
      };
      home.file = {
        ".aliasrc".source = ./aliasrc;
        ".bashrc" = {
          text = ''
#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

###############################################################################
[[ -f ~/.aliasrc ]] && source ~/.aliasrc
[[ -f ~/.aliasrc-private ]] && source ~/.aliasrc-private
[[ -d $HOME/bin ]] && {
    export PATH=$HOME/bin:$PATH
    [[ -d $HOME/bin/stolen ]] && export PATH=$PATH:$HOME/bin/stolen
    [[ -d $HOME/bin/docker ]] && export PATH=$PATH:$HOME/bin/docker
}
[[ -d $HOME/.perl/bin ]] && export PATH=$HOME/.perl/bin:$PATH
[[ -d $HOME/.cabal/bin ]] && export PATH=$HOME/.cabal/bin:$PATH
[[ -d $HOME/.local/bin ]] && export PATH=$HOME/.local/bin:$PATH

###############################################################################
export PROMPT_COMMAND='history -a'
          '';
        };
        ".config/htop/htoprc" = {
          text = ''
fields=0 48 17 18 38 39 40 2 46 47 111 49 1
sort_key=46
sort_direction=1
hide_threads=0
hide_kernel_threads=1
hide_userland_threads=0
shadow_other_users=0
show_thread_names=0
show_program_path=0
highlight_base_name=1
highlight_megabytes=1
highlight_threads=1
tree_view=0
header_margin=1
detailed_cpu_time=1
cpu_count_from_zero=0
update_process_names=0
account_guest_in_cpu_meter=0
color_scheme=6
delay=15
left_meters=AllCPUs2 CPU LoadAverage Tasks
left_meter_modes=1 1 2 2
right_meters=Uptime Battery Hostname Blank Blank Memory Swap
right_meter_modes=2 2 2 2 2 1 1
          '';
        };
        ".agignore" = {
          text = ''
            /.git/
          '';
        };
        ".config/ranger" = {
          source = ./ranger;
          recursive = true;
        };
      };
    };
    environment = {
      systemPackages = with pkgs; [
        ag
        ranger
        tmux
        elinks w3m
        manpages
        file

        # admin:
        htop iftop iptraf-ng iotop bmon s-tui
        pwgen # unstable.mkpasswd
        usbutils
        tcpdump
        pmount fuse

        (writeScriptBin "myspeedtest.sh" (builtins.readFile ./bin/myspeedtest.sh))
      ];
      interactiveShellInit = ''
        eval $(${pkgs.thefuck}/bin/thefuck --alias)
        hgrep() { history | egrep "$@"; }
      '';
      shellAliases = {
        cat = "${pkgs.bat}/bin/bat --theme=\"Monokai Extended Light\"";
        ps = "${pkgs.procs}/bin/procs";
      };
    };
    programs.thefuck.enable = true;

    security = {
      sudo.extraConfig = ''
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl suspend
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl reboot
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl poweroff
      '';
      wrappers = {
        pmount.source  = "${pkgs.pmount}/bin/pmount";
        pumount.source  = "${pkgs.pmount}/bin/pumount";
      };
    };
  };
}
