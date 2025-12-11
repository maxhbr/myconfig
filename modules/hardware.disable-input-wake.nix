{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.myconfig;

  disableInputWakeScript = pkgs.writeShellScript "disable-input-wake" ''
    set -eu

    # Some shells (dash) do not have shopt, so guard it.
    if command -v shopt >/dev/null 2>&1; then
      shopt -s nullglob
    fi

    for e in /sys/class/input/event*; do
      [ -e "$e" ] || continue

      name="unknown"
      if [ -r "$e/device/name" ]; then
        name=$(cat "$e/device/name")
      fi

      # Keep wake enabled on power and lid related inputs.
      case "$name" in
        *Power*Button*|*Power*Key*|*Sleep*Button*|*Lid*)
          echo "Skipping input '$name' ($e) – keep wake enabled" >&2
          continue
          ;;
      esac

      wake="$e/device/power/wakeup"
      if [ -r "$wake" ]; then
        cur=$(cat "$wake")
        if [ "$cur" = "enabled" ]; then
          echo "Disabling wake on input '$name' ($e)" >&2
          echo disabled > "$wake"
        else
          echo "Wake already disabled on input '$name' ($e)" >&2
        fi
      fi
    done
  '';
in {
  options.myconfig.hardware.disableInputWake = {
    enable = mkEnableOption ''
      Disable waking the system from suspend using generic input devices
      (keyboards, mice, etc.) while keeping wake enabled for power button
      and lid switch input devices.
    '';
  };

  config = mkIf cfg.hardware.disableInputWake.enable {
    systemd.services.disable-input-wake = {
      description = "Disable wake from non-power input devices";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "sysinit.target" ];

      serviceConfig = {
        Type      = "oneshot";
        ExecStart = disableInputWakeScript;
      };
    };
  };
}
