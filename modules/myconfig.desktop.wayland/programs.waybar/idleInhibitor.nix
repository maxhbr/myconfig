# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Externally controllable idle inhibitor for wayland sessions: the systemd
# user service `idle-inhibitor.service` holds a logind "idle" inhibitor,
# which swayidle watches (logind BlockInhibited). `set-idle-inhibitor` is
# the single source of truth; after a state change it pokes waybar
# (SIGRTMIN+8) so the custom/idle_inhibitor module reflects it.
# Optionally, Thunderbolt connect events drive the state — patterns in
# `myconfig.desktop.wayland.waybar.idleInhibitor.thunderboltDevices` are
# matched against sysfs attributes of authorized devices.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.desktop.wayland.waybar.idleInhibitor;
  user = myconfig.user;
  tbDevicePatterns = cfg.thunderboltDevices;
  tbIdleInhibitorEnabled = tbDevicePatterns != [ ];
  tbDeviceRegex = lib.concatStringsSep "|" tbDevicePatterns;

  # marks in $XDG_RUNTIME_DIR coordinating manual and device-driven state:
  # - idle-inhibitor-tb.conn: a matching device is connected (sync's view
  #   of the hardware)
  # - idle-inhibitor-tb.lock: the inhibitor was activated by a matching
  #   device (undocking releases it)
  # - idle-inhibitor-tb.manual: the user overrode the device-driven state;
  #   Thunderbolt events leave the inhibitor alone until a fresh dock
  tbLock = "idle-inhibitor-tb.lock";
  tbConnLock = "idle-inhibitor-tb.conn";
  manualLock = "idle-inhibitor-tb.manual";
  runtimeDir = "\${XDG_RUNTIME_DIR:-/run/user/$(id -u)}";

  idleInhibitorUnit = "idle-inhibitor.service";
  idleInhibitorStatus = pkgs.writeShellApplication {
    name = "idle-inhibitor-status";
    text = ''
      set -euo pipefail
      if ${pkgs.systemd}/bin/systemctl --user is-active --quiet ${idleInhibitorUnit}; then
        echo '{"text":"\uf06e","class":"activated","tooltip":"idle inhibitor on (screen will not lock)"}'
      else
        echo '{"text":"\uf070","class":"deactivated","tooltip":"idle inhibitor off"}'
      fi
    '';
  };
  # Sync idle inhibitor state with connected Thunderbolt devices.
  # Matches sysfs `device_name`, `vendor_name` and `unique_id` of
  # authorized (non-host) devices under /sys/bus/thunderbolt/devices
  # against the configured patterns (extended regex, case-insensitive).
  # udev cannot match sysfs attributes on "remove" events (the node is
  # already gone), so the rule always runs a rescan script instead.
  syncTbIdleInhibitor = pkgs.writeShellApplication {
    name = "sync-tb-idle-inhibitor";
    text = ''
      set -euo pipefail
      runtime_dir="${runtimeDir}"

      # devices authorize asynchronously after the add uevent; retry
      # briefly so a rescan-at-add does not miss a freshly docked device
      matched=false
      for _ in 1 2 3 4 5; do
        for d in /sys/bus/thunderbolt/devices/*; do
          [ -d "$d" ] || continue
          [ -f "$d/authorized" ] || continue # skip the host controller (domain device)
          [ "$(cat "$d/authorized")" = "1" ] || continue
          devname=$(cat "$d/device_name" 2>/dev/null || true)
          vendor=$(cat "$d/vendor_name" 2>/dev/null || true)
          uid=$(cat "$d/unique_id" 2>/dev/null || true)
          if printf '%s\n%s\n%s\n' "$devname" "$vendor" "$uid" \
            | ${pkgs.gnugrep}/bin/grep -q -E -i '${lib.escape [ "'" ] tbDeviceRegex}'; then
            matched=true
            break
          fi
        done
        if $matched; then
          break
        fi
        sleep 1
      done

      if $matched; then
        fresh_dock=false
        if [ ! -f "$runtime_dir/${tbConnLock}" ]; then
          fresh_dock=true
          touch "$runtime_dir/${tbConnLock}"
        fi
        # respect a manual override while the same dock stays connected;
        # a fresh dock clears it and lets the device take over again
        if $fresh_dock; then
          rm -f "$runtime_dir/${manualLock}"
        elif [ -f "$runtime_dir/${manualLock}" ]; then
          exit 0
        fi
        ${setIdleInhibitor}/bin/set-idle-inhibitor --device-driven on
        touch "$runtime_dir/${tbLock}"
      else
        rm -f "$runtime_dir/${tbConnLock}"
        if [ -f "$runtime_dir/${tbLock}" ]; then
          rm -f "$runtime_dir/${tbLock}"
          ${setIdleInhibitor}/bin/set-idle-inhibitor --device-driven off
        fi
      fi
    '';
  };
  # NOTE: udev runs as root and reaches the session user's systemd
  # manager via `systemctl --machine=<user>@.host --user` — see
  # services.udev.extraRules below. The scripts above are user-scoped.
  setIdleInhibitor = pkgs.writeShellApplication {
    name = "set-idle-inhibitor";
    text = ''
      set -euo pipefail
      usage() {
        echo "usage: set-idle-inhibitor [--device-driven] on|off|toggle|status" >&2
        exit 2
      }
      systemctl=${pkgs.systemd}/bin/systemctl
      is_active() {
        "$systemctl" --user is-active --quiet ${idleInhibitorUnit}
      }
      refresh_waybar() {
        # waybar custom modules default to signal = 8 (SIGRTMIN+8)
        ${pkgs.psmisc}/bin/pkill -SIGRTMIN+8 waybar 2> /dev/null || true
      }
      device_driven=false
      cmd=""
      while [[ $# -gt 0 ]]; do
        case "$1" in
          --device-driven)
            device_driven=true
            shift
            ;;
          on | off | toggle | status)
            [ -z "$cmd" ] || usage
            cmd="$1"
            shift
            ;;
          *) usage ;;
        esac
      done
      [ -n "$cmd" ] || usage
      case "$cmd" in
        on)
          "$systemctl" --user start ${idleInhibitorUnit}
          refresh_waybar
          ;;
        off)
          "$systemctl" --user stop ${idleInhibitorUnit}
          refresh_waybar
          ;;
        toggle)
          if is_active; then
            "$systemctl" --user stop ${idleInhibitorUnit}
          else
            "$systemctl" --user start ${idleInhibitorUnit}
          fi
          refresh_waybar
          ;;
        status)
          if is_active; then
            echo "on"
          else
            echo "off"
          fi
          ;;
      esac
      # A manual invocation replaces any device-driven state: it clears
      # the device-driven mark (a later undocking must not turn a manual
      # activation off) and sets the manual-override mark (Thunderbolt
      # events leave the state alone until a fresh dock clears it).
      runtime_dir="${runtimeDir}"
      if ! $device_driven && [ "$cmd" != "status" ]; then
        rm -f "$runtime_dir/${tbLock}" 2> /dev/null || true
        touch "$runtime_dir/${manualLock}"
      fi
    '';
  };
in
{
  options.myconfig.desktop.wayland.waybar.idleInhibitor = with lib; {
    thunderboltDevices = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = literalExpression ''
        [
          "Caldigit.*"
          "TS4"
        ]
      '';
      description = mdDoc ''
        Extended regex patterns (case-insensitive) matched against the
        `device_name`, `vendor_name` and `unique_id` of authorized
        Thunderbolt devices in sysfs. While any device matches, the
        idle inhibitor is turned on; when the last matching device
        disconnects, a device-driven activation is turned off again.
        A manual `set-idle-inhibitor on` or `off` is not overridden by
        Thunderbolt events while the same dock stays connected; a fresh
        dock hands control back to the device.
      '';
    };
  };

  config = lib.mkIf config.myconfig.desktop.wayland.enable {
    home-manager.sharedModules = [
      (
        { config, ... }:
        lib.mkIf config.programs.waybar.enable {
          home.packages = [
            setIdleInhibitor
            idleInhibitorStatus
          ]
          ++ lib.optionals tbIdleInhibitorEnabled [ syncTbIdleInhibitor ];
          systemd.user.services.idle-inhibitor = {
            Unit = {
              Description = "Hold a logind idle inhibitor (screen locking is prevented while active)";
              PartOf = [ "graphical-session.target" ];
              After = [ "graphical-session.target" ];
            };
            Service = {
              ExecStart = "${pkgs.systemd}/bin/systemd-inhibit --what=idle --who=${user} --why=set-idle-inhibitor ${pkgs.coreutils}/bin/sleep infinity";
            };
          };
          # One-shot Thunderbolt sync at session start (covers
          # already-docked machines); udev re-triggers it on every
          # thunderbolt add/remove event.
          systemd.user.services.idle-inhibitor-tb-sync = lib.mkIf tbIdleInhibitorEnabled {
            Unit = {
              Description = "Sync idle inhibitor with connected Thunderbolt devices";
              After = [ "graphical-session.target" ];
              PartOf = [ "graphical-session.target" ];
            };
            Install = {
              WantedBy = [ "graphical-session.target" ];
            };
            Service = {
              Type = "oneshot";
              ExecStart = "${syncTbIdleInhibitor}/bin/sync-tb-idle-inhibitor";
            };
          };
          programs.waybar.settings.mainBar."custom/idle_inhibitor" = {
            format = "{}";
            exec = "${idleInhibitorStatus}/bin/idle-inhibitor-status";
            return-type = "json";
            on-click = "${setIdleInhibitor}/bin/set-idle-inhibitor toggle";
            interval = 10;
            tooltip = true;
            rotate = 90;
          };
        }
      )
    ];
    # Deliver thunderbolt connect/disconnect events to the session user's
    # tb-sync service. udev runs as root; --machine=<user>@.host reaches the
    # user's systemd manager (fails harmlessly while the user is not
    # logged in — the session-start oneshot covers that case). On remove
    # events the sysfs node is already gone, so the service rescans sysfs
    # instead of trusting the event payload.
    services.udev.extraRules = lib.mkIf tbIdleInhibitorEnabled ''
      ACTION=="add|remove|change", SUBSYSTEM=="thunderbolt", RUN+="${pkgs.systemd}/bin/systemctl --machine=${user}@.host --user start idle-inhibitor-tb-sync.service"
    '';
  };
}
