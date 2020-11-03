# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [ ../shell.common ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ fasd ];
      programs.fish = {
        enable = true;
        shellAliases = {};
        shellAbbrs = {};
        functions = {
          gitignore = "curl -sL https://www.gitignore.io/api/$argv";
        };
        shellInit = "";
        loginShellInit = "";
        interactiveShellInit = "";
        promptInit = "";
        plugins = [
          {
            name = "z";
            src = pkgs.fetchFromGitHub {
              owner = "jethrokuan";
              repo = "z";
              rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
              sha256 = "0c5i7sdrsp0q3vbziqzdyqn4fmp235ax4mn4zslrswvn8g3fvdyh";
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
            name="foreign-env";
            src = pkgs.fetchFromGitHub {
              owner = "oh-my-fish";
              repo = "plugin-foreign-env";
              rev = "dddd9213272a0ab848d474d0cbde12ad034e65bc";
              sha256 = "00xqlyl3lffc5l0viin1nyp819wf81fncqyz87jx8ljjdhilmgbs";
            };
          }
        ];
      };
    };
    programs.fish = {
      enable = true;
    };
    # systemd = {
    #   timers.fish-history-backup-timer = {
    #     wantedBy = [ "timers.target" ];
    #     partOf = [ "fish-history-backup-timer.service" ];
    #     timerConfig.OnCalendar = "hourly";
    #   };
    #   services.fish-history-backup-timer = {
    #     serviceConfig.Type = "oneshot";
    #     script = ''
    #       historyfile=/home/mhuber/.local/share/fish/fish_history
    #       backupdir="$historyfile"_backups
    #       backup=$backupdir/$(date '+%Y-%V').fish_history.gz
    #       if [[ ! -f $backup ]]; then
    #         mkdir -p $backupdir
    #         echo "Time: $(date)." >> $backupdir/fish-history-backup-timer.log
    #         ${pkgs.gzip}/bin/gzip -k $historyfile
    #         mv $historyfile.gz $backup
    #         chown mhuber:mhuber $backup
    #       fi
    #     '';
    #   };
    # };
  };
}
