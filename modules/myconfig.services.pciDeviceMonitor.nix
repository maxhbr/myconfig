{ config, lib, pkgs, ... }:
with lib;

let cfg = config.myconfig.services.pciDeviceMonitor;
in {
  options.myconfig.services.pciDeviceMonitor = {
    enable = mkEnableOption "PCI device presence monitor service";

    searchSubstring = mkOption {
      type = types.str;
      description = ''
        The substring to look for in the lspci output. The system will reboot
        if this substring is not found after the specified number of retries.
      '';
    };

    retries = mkOption {
      type = types.int;
      default = 3;
      description = "Number of consecutive failed checks before rebooting";
    };

    checkInterval = mkOption {
      type = types.int;
      default = 60;
      description = "Interval in seconds between checks";
    };

    initialDelay = mkOption {
      type = types.int;
      default = 600;
      description =
        "Initial delay in seconds after boot before starting checks";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.pci-device-monitor = {
      description = "PCI Device Presence Monitor";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];

      environment = {
        SEARCH_SUBSTRING = cfg.searchSubstring;
        INITIAL_DELAY = toString cfg.initialDelay;
        MAX_RETRIES = toString cfg.retries;
        CHECK_INTERVAL = toString cfg.checkInterval;
      };

      script = ''
        #!/usr/bin/env bash

        check_lspci() {
          ${pkgs.pciutils}/bin/lspci | grep -q "$SEARCH_SUBSTRING"
        }

        wait_for_device() {
          while true; do
            echo "Checking for $SEARCH_SUBSTRING, if it has appeared at least once..."
            if check_lspci; then
              echo "Device found, starting monitoring"
              return 0
            fi
            echo "Waiting for $INITIAL_DELAY seconds before checking again..."
            sleep $INITIAL_DELAY
          done
        }

        watch_device() { 
          echo "Watching for $SEARCH_SUBSTRING"
          local CURRENT_RETRY=0
          while true; do
            if check_lspci; then
              # Device is present, reset retry counter
              CURRENT_RETRY=0
              echo "Device still present"
            else
              # Device not found, increment retry counter
              ((CURRENT_RETRY++))
              echo "Device missing (attempt $CURRENT_RETRY/$MAX_RETRIES)"

              if [ $CURRENT_RETRY -ge $MAX_RETRIES ]; then
                echo "Device missing for $MAX_RETRIES consecutive checks. Rebooting..."
                systemctl reboot
                exit 0
              fi
            fi

            sleep $CHECK_INTERVAL
          done
        }

        wait_for_device
        watch_device
      '';

      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "10s";
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };

    systemd.timers.pci-device-monitor = {
      description = "Timer for PCI Device Monitor";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "${toString cfg.initialDelay}s";
        OnUnitActiveSec = "${toString cfg.checkInterval}s";
        Unit = "pci-device-monitor.service";
      };
    };
  };
}
