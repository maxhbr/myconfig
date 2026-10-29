# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Externally controllable idle inhibitor for wayland sessions.
#
# waybar's own idle_inhibitor module (0.15.0) has no IPC, so it cannot be
# driven from a script. Instead a systemd user service holds a logind "idle"
# inhibitor, which swayidle watches (logind BlockInhibited) — while it runs,
# swayidle suspends its lock timeouts. `set-idle-inhibitor` is the single
# source of truth; after a state change it pokes waybar (SIGRTMIN+8) so the
# custom/idle_inhibitor module reflects it.
#
# Optionally the state is driven by Thunderbolt connect events: patterns in
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

  idleInhibitorUnit = "idle-inhibitor.service";
  idleInhibitorStatus = pkgs.writeShellScriptBin "idle-inhibitor-status" ''
    set -euo pipefail
    if ${pkgs.systemd}/bin/systemctl --user is-active --quiet ${idleInhibitorUnit}; then
      echo '{"text":"\uf06e","class":"activated","tooltip":"idle inhibitor on (screen will not lock)"}'
    else
      echo '{"text":"\uf070","class":"deactivated","tooltip":"idle inhibitor off"}'
    fi
  '';
  # Sync idle inhibitor state with connected Thunderbolt devices.
  # Matches sysfs `device_name`, `vendor_name` and `unique_id` of
  # authorized (non-host) devices under /sys/bus/thunderbolt/devices
  # against the configured patterns (extended regex, case-insensitive).
  # udev cannot match sysfs attributes on "remove" events (the node is
  # already gone), so the rule always runs a rescan script instead.
  # A no-op while no patterns are configured.
  syncTbIdleInhibitor = pkgs.writeShellScriptBin "sync-tb-idle-inhibitor" ''
    set -euo pipefail
    ${lib.optionalString (!tbIdleInhibitorEnabled) "exit 0"}
    matched=false
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
      ${setIdleInhibitor}/bin/set-idle-inhibitor --device-driven on
      touch "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/idle-inhibitor-tb.lock"
    else
      if [ -f "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/idle-inhibitor-tb.lock" ]; then
        rm -f "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/idle-inhibitor-tb.lock"
        ${setIdleInhibitor}/bin/set-idle-inhibitor --device-driven off
      fi
    fi
  '';
  # NOTE: udev runs as root and reaches the session user's systemd
  # manager via `systemctl --machine=<user>@.host --user` — see
  # services.udev.extraRules below. The script above is user-scoped.
  setIdleInhibitor = pkgs.writeShellScriptBin "set-idle-inhibitor" ''
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
        on|off|toggle|status)
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
    # A manual invocation clears the device-driven mark, so a later
    # Thunderbolt disconnect only deactivates the inhibitor if the
    # current activation came from a matching device.
    if ! $device_driven && [ "$cmd" != "status" ]; then
      rm -f "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/idle-inhibitor-tb.lock" 2> /dev/null || true
    fi
  '';
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
        A manual `set-idle-inhibitor on` is not overridden by
        disconnects.
      '';
    };
  };

  config = {
    home-manager.sharedModules = [
      (
        { config, ... }:
        lib.mkIf config.programs.waybar.enable {
          home.packages = [
            setIdleInhibitor
            idleInhibitorStatus
            syncTbIdleInhibitor
          ];
          systemd.user.services.idle-inhibitor = {
            Unit = {
              Description = "Hold a logind idle inhibitor (screen locking is prevented while active)";
              PartOf = [ "graphical-session.target" ];
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
    services.udev.extraRules = lib.mkIf (tbDevicePatterns != [ ]) ''
      ACTION=="add|remove|change", SUBSYSTEM=="thunderbolt", RUN+="${pkgs.systemd}/bin/systemctl --machine=${user}@.host --user start idle-inhibitor-tb-sync.service"
    '';
  };
}
