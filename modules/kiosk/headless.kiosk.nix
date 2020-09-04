# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# see also:
#  - https://gist.github.com/datakurre/cd29a985351e6b8c9bbc04532e5f9df0
{ pkgs, ... }:
let
  attachKiosk = with pkgs;
    writeScriptBin "attachKiosk" ''
      ${pkgs.tmux}/bin/tmux -S /tmp/kioskSocket attach-session -t kiosk
    '';
  setupKiosk = with pkgs;
    writeScriptBin "setupKiosk" ''
      set -x
      if test -z $TMUX && [[ $TERM != "screen" ]]; then
        if ${pkgs.tmux}/bin/tmux -S /tmp/kioskSocket has-session -t kiosk; then
          exec ${attachKiosk}/bin/attachKiosk
        else
          exec ${pkgs.tmux}/bin/tmux -S /tmp/kioskSocket -2 new-session -s kiosk \; \
            send-keys '${pkgs.htop}/bin/htop' C-m \; \
            split-window -v \; \
            send-keys 'dmesg -w' C-m \; 
          # exec ${pkgs.tmux}/bin/tmux -S /tmp/kioskSocket -2 new-session -s kiosk ${pkgs.htop}/bin/htop
        fi
      fi
      if [[ -e /tmp/kioskSocket ]]; then
        chmod g+rwx /tmp/kioskSocket
      fi
      exec ${pkgs.stdenv.shell} "$@"
    '';
  setupKioskAliasrc = with pkgs;
    writeText "setupKioskAliasrc" ''
      [[ $- != *i* ]] && return
      if [ "$PS1" != "" ]; then
        if test -z $TMUX && [[ $TERM != "screen" ]]; then
          if ${pkgs.tmux}/bin/tmux -S /tmp/kioskSocket has-session -t kiosk; then
            exec ${attachKiosk}/bin/attachKiosk
          else
            exec ${pkgs.tmux}/bin/tmux -S /tmp/kioskSocket -2 new-session -s kiosk ${pkgs.htop}/bin/htop
          fi
        else
          if [[ -e /tmp/kioskSocket ]]; then
            chmod g+rwx /tmp/kioskSocket
          fi
        fi
      fi
    '';
in {
  imports = [ ./common.nix ];
  config = {
    services.mingetty = {
      autologinUser = "kiosk";
      greetingLine = "Lets play Kiosk";
      helpLine = "";
    };

    environment.systemPackages = with pkgs; [ setupKiosk attachKiosk ];

    # users.extraUsers.kiosk.shell = "${setupKiosk}/bin/setupKiosk";
    # systemd.services.write-kiosk-bashrc =
    #   { serviceConfig.Type = "oneshot";
    #     script = ''
    #       cat ${setupKioskAliasrc} > /home/kiosk/.bashrc
    #     '';
    #     wantedBy = ["getty@tty1.service"];
    #   };
  };
}
