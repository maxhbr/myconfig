{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.myconfig.services.dmesgMonitor;
in {
  options.myconfig.services.dmesgMonitor = {
    enable = mkEnableOption "System log error monitor service";
    
    errorSubstrings = mkOption {
      type = types.listOf types.str;
      description = ''
        List of error substrings to look for in dmesg/journalctl output. The system will reboot
        if any of these substrings are found.
      '';
    };

    checkInterval = mkOption {
      type = types.int;
      default = 60;
      description = "Interval in seconds between checks";
    };

    initialDelay = mkOption {
      type = types.int;
      default = 600;
      description = "Initial delay in seconds after boot before starting checks";
    };

    lookbackWindow = mkOption {
      type = types.int;
      default = 300;
      description = "Number of seconds to look back in the logs";
    };

    publicPort = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = "Optional public port to expose for monitoring";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.lookbackWindow < (cfg.initialDelay - 60);
        message = "lookbackWindow must be smaller than initialDelay to ensure we only look at logs after the service starts monitoring";
      }
    ];

    systemd.services.dmesg-monitor = {
      description = "System Log Error Monitor";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      
      environment = {
        GREP_SEARCH = builtins.concatStringsSep "|" cfg.errorSubstrings;
        INITIAL_DELAY = toString cfg.initialDelay;
        CHECK_INTERVAL = toString cfg.checkInterval;
        LOOKBACK_WINDOW = toString cfg.lookbackWindow;
      };

      script = ''
        #!/usr/bin/env bash

        check_logs() {
          ${pkgs.systemd}/bin/journalctl -k -b0 --since "-$LOOKBACK_WINDOW seconds" | grep -qE "$GREP_SEARCH"
        }

        wait_for_initial_delay() {
          echo "Waiting for initial delay of $INITIAL_DELAY seconds..."
          sleep $INITIAL_DELAY
          echo "...waited"
        }

        watch_logs() { 
          while true; do
            if check_logs; then
              systemctl reboot
              exit 0
            fi
            sleep $CHECK_INTERVAL
          done
        }

        wait_for_initial_delay
        watch_logs
      '';

      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "10s";
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };

    systemd.timers.dmesg-monitor = {
      description = "Timer for System Log Error Monitor";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "${toString cfg.initialDelay}s";
        OnUnitActiveSec = "${toString cfg.checkInterval}s";
        Unit = "dmesg-monitor.service";
      };
    };

    networking.firewall.allowedTCPPorts = lib.optional (cfg.publicPort != null) cfg.publicPort;
  };
}