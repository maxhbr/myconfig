{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.myconfig;

  disableXhciWakeScript = pkgs.writeShellApplication {
    name = "disable-xhci-wake";
    runtimeInputs = [ pkgs.gnugrep pkgs.coreutils ];
    text = ''
      if [ ! -r /proc/acpi/wakeup ]; then
        echo "/proc/acpi/wakeup not available; nothing to do." >&2
        exit 0
      fi

      for dev in XHC0 XHC1 XHC3 XHC4; do
        line=$(grep "^$dev" /proc/acpi/wakeup || true)
        [ -n "$line" ] || continue

        if printf '%s\n' "$line" | grep -q '\*enabled'; then
          echo "Disabling ACPI wake on $dev" >&2
          echo "$dev" > /proc/acpi/wakeup || true
        else
          echo "ACPI device $dev already disabled for wake." >&2
        fi
      done
    '';
  };
in {
  options.myconfig.hardware.disableXhciWake = {
    enable = mkEnableOption ''
      Disable waking the system from suspend using XHCI USB controllers
      (XHC0, XHC1, XHC3, XHC4 as listed in /proc/acpi/wakeup).
    '';
  };

  config = mkIf cfg.hardware.disableXhciWake.enable {
    systemd.services.disable-xhci-wake = {
      description = "Disable wake from all XHCI USB controllers";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "sysinit.target" ];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${disableXhciWakeScript}/bin/disable-xhci-wake";
      };
    };
  };
}
