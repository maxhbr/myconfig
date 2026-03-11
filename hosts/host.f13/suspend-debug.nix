# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Diagnostic tooling for s2idle / S0ix suspend issues on AMD platforms.
# Provides a script to check whether the platform properly enters S0ix
# and a systemd service that logs s0ix stats before and after each suspend.
{ pkgs, ... }:
let
  suspend-debug = pkgs.writeShellScriptBin "suspend-debug" ''
    set -euo pipefail
    echo "=== Suspend / S0ix Diagnostics ==="
    echo ""

    echo "--- /sys/power/mem_sleep ---"
    cat /sys/power/mem_sleep
    echo ""

    echo "--- /sys/power/state ---"
    cat /sys/power/state
    echo ""

    echo "--- Kernel command line ---"
    cat /proc/cmdline
    echo ""

    if [ -e /sys/kernel/debug/amd_pmc/s0ix_stats ]; then
      echo "--- AMD PMC S0ix stats ---"
      cat /sys/kernel/debug/amd_pmc/s0ix_stats
      echo ""
    else
      echo "WARNING: /sys/kernel/debug/amd_pmc/s0ix_stats not found."
      echo "  Ensure debugfs is mounted and amd_pmc module is loaded."
      echo ""
    fi

    if [ -e /sys/kernel/debug/amd_pmc/smu_fw_version ]; then
      echo "--- AMD PMC SMU firmware version ---"
      cat /sys/kernel/debug/amd_pmc/smu_fw_version
      echo ""
    fi

    echo "--- ACPI wakeup sources ---"
    cat /proc/acpi/wakeup
    echo ""

    echo "--- USB wakeup status ---"
    for dev in /sys/bus/usb/devices/*/power/wakeup; do
      if [ -e "$dev" ]; then
        printf "%s: %s\n" "$dev" "$(cat "$dev")"
      fi
    done
    echo ""

    echo "--- Recent suspend/resume journal entries ---"
    journalctl -b --no-pager -g "suspend|PM:|s2idle|sleep" --lines=30 2>/dev/null || true
  '';
in
{
  environment.systemPackages = [ suspend-debug ];

  # Log s0ix stats around suspend cycles for post-mortem analysis
  systemd.services.suspend-debug-log = {
    description = "Log S0ix stats before suspend and after resume";
    before = [ "sleep.target" ];
    wantedBy = [ "sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
    };
    script = ''
      if [ -e /sys/kernel/debug/amd_pmc/s0ix_stats ]; then
        echo "=== S0ix stats at $(date --iso-8601=seconds) (pre-suspend) ==="
        cat /sys/kernel/debug/amd_pmc/s0ix_stats
      fi
    '';
    postStop = ''
      if [ -e /sys/kernel/debug/amd_pmc/s0ix_stats ]; then
        echo "=== S0ix stats at $(date --iso-8601=seconds) (post-resume) ==="
        cat /sys/kernel/debug/amd_pmc/s0ix_stats
      fi
    '';
  };
}
