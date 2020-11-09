# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  jsonFile = ./. + "/chisui-zsh-nix-shell.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
in {
  imports = [ ../shell.common ];
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [ oh-my-zsh ];
      home.file = {
        ".zshrc".source = ./zshrc;
        ".zprofile".source = ./zprofile;
        ".profile".source = ./zprofile;
        ".zshrc.pre-oh-my-zsh".source = ./zshrc.pre-oh-my-zsh;
        ".zsh-nix-shell".source =
          pkgs.fetchFromGitHub { inherit (json) owner repo rev sha256; };
      };
    };
    environment = {
      shells = [ "${pkgs.zsh}/bin/zsh" "/run/current-system/sw/bin/zsh" ];
    };

    programs.zsh = {
      enable = true;
      histSize = 100000;
      syntaxHighlighting.enable = true;
      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
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
          historyfile=/home/${user}/.zsh_history
          backupdir="$historyfile"_backups
          backup=$backupdir/$(date '+%Y-%V').zsh_history.gz
          if [[ ! -f $backup ]]; then
            mkdir -p $backupdir
            echo "Time: $(date)." >> $backupdir/zsh-history-backup-timer.log
            ${pkgs.gzip}/bin/gzip -k $historyfile
            mv $historyfile.gz $backup
            chown ${user}:${user} $backup
          fi
        '';
      };
    };
  };
}
