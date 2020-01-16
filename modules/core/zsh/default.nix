# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  imports = [
    ../shell.common
  ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ oh-my-zsh ];
      home.file = {
        ".zshrc".source = ./zshrc;
        ".zprofile".source = ./zprofile;
        ".profile".source = ./zprofile;
        ".zshrc.pre-oh-my-zsh".source = ./zshrc.pre-oh-my-zsh;
        ".zsh-nix-shell".source = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "master";
          sha256 = "0l41ac5b7p8yyjvpfp438kw7zl9dblrpd7icjg1v3ig3xy87zv0n"; # TODO: autoupdate
        };
      };
    };
    environment = {
      shells = [
        "${pkgs.zsh}/bin/zsh"
        "/run/current-system/sw/bin/zsh"
      ];
    };

    programs.zsh = {
      enable = true;
      histSize = 100000;
      syntaxHighlighting.enable = true;
      ohMyZsh = {
        enable = true;
        plugins = ["git"];
      };
      promptInit = ""; # Clear this to avoid a conflict with oh-my-zsh
    };
    systemd = {
      timers.zsh-history-backup-timer = {
        wantedBy = [ "timers.target" ];
        partOf = [ "zsh-history-backup-timer.service" ];
        timerConfig.OnCalendar = "hourly";
      };
      services.zsh-history-backup-timer = {
        serviceConfig.Type = "oneshot";
        script = ''
          historyfile=/home/mhuber/.zsh_history
          backupdir="$historyfile"_backups
          mkdir -p $backupdir
          backup=$backupdir/.zsh_history_$(date '+%Y-%V')
          if [[ ! -f $backup ]]; then
            echo "Time: $(date)." >> $backupdir/zsh-history-backup-timer.log
            cp  /home/mhuber/.zsh_history $backup
          fi
        '';
      };
    };
  };
}
