{ config, lib, pkgs, ... }:
let user = config.myconfig.user;
in {
  config = {
    systemd = {
      timers.fish-history-backup-timer = {
        wantedBy = [ "timers.target" ];
        partOf = [ "fish-history-backup-timer.service" ];
        timerConfig.OnCalendar = "hourly";
      };
      services.fish-history-backup-timer = {
        serviceConfig.Type = "oneshot";
        script = ''
          historyfile=/home/${user}/.local/share/fish/fish_history
          backupdir="$historyfile"_backups
          backup=$backupdir/$(date '+%Y-%V').fish_history.gz
          if [[ ! -f $backup ]]; then
            mkdir -p $backupdir
            echo "Time: $(date)." >> $backupdir/fish-history-backup-timer.log
            ${pkgs.gzip}/bin/gzip -k $historyfile
            mv $historyfile.gz $backup
            chown ${user}:${user} $backup
          fi
        '';
      };
    };
  };
}
