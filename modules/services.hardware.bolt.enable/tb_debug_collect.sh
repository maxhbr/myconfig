#!/usr/bin/env bash
# Collect evidence for debugging a non-working ThinkPad Thunderbolt dock.
# Run ON THE HOST with the dock connected, ideally right after a cold
# boot with the dock plugged in. Output goes to a timestamped log file
# in ~/_tb_debug/. Requires root for some sections; they degrade
# gracefully if unavailable.
set -uo pipefail

out="${HOME}/_tb_debug/tb-evidence-$(date +%Y%m%d-%H%M%S).log"
mkdir -p "$(dirname "$out")"

section() {
    printf '\n===== %s =====\n' "$1" >>"$out"
    echo "-- collecting: $1" >&2
}

run() {
    # run() <label> <cmd...>  — never abort the collector on failure
    section "$1"
    shift
    "$@" >>"$out" 2>&1 || echo "(command failed: $*)" >>"$out"
}

run_log() {
    # run_log <label> <file> <cmd...>  — dump cmd output, fall back to reading file
    local label=$1 file=$2
    shift 2
    section "$label"
    if "$@" >>"$out" 2>&1; then
        return
    fi
    if [[ -r $file ]]; then
        echo "(command failed, dumping $file)" >>"$out"
        cat "$file" >>"$out" 2>&1
    else
        echo "(command failed and $file not readable)" >>"$out"
    fi
}

{
    echo "Thunderbolt dock debug evidence"
    echo "collected: $(date -Is)"
    echo "host:      $(uname -n)"
    echo "kernel:    $(uname -r)"
} >"$out"

run "system" uname -a
run "os-release" cat /etc/os-release

# --- bolt -------------------------------------------------------------
if command -v boltctl >/dev/null 2>&1; then
    run "boltctl list" boltctl list
    run "boltctl domains" boltctl domains
    section "boltctl info (per device)"
    # UUIDs come from sysfs; parsing them out of `boltctl list` picks up
    # the tree-drawing glyphs ('|-', '`-', 'o') instead.
    while IFS= read -r uuid; do
        [[ -n $uuid ]] || continue
        boltctl info "$uuid" >>"$out" 2>&1 || true
    done < <(cat /sys/bus/thunderbolt/devices/*/unique_id 2>/dev/null)
    run "boltctl power" boltctl power
else
    section "boltctl"
    echo "boltctl not found in PATH" >>"$out"
fi

# --- sysfs thunderbolt ------------------------------------------------
section "sysfs thunderbolt devices"
if [[ -d /sys/bus/thunderbolt/devices ]]; then
    ls -l /sys/bus/thunderbolt/devices/ >>"$out" 2>&1 || true
    for d in /sys/bus/thunderbolt/devices/*/; do
        echo "-- $d" >>"$out"
        for f in device_name vendor_name device_type authorized unique_id \
            nvm_version generation speed is_boot_supported security power_state; do
            if [[ -r ${d}${f} ]]; then
                printf '%s: %s\n' "$f" "$(cat "${d}${f}" 2>/dev/null)" >>"$out"
            fi
        done
    done
else
    echo "no /sys/bus/thunderbolt/devices" >>"$out"
fi

# --- PCI / USB enumeration -------------------------------------------
run_log "lspci thunderbolt/usb4" /sys/bus/pci/devices \
    lspci -nn -vv -k -d ::0c04
run "lspci all (filtered)" sh -c 'lspci -nn | grep -iE "thunderbolt|usb4|xdomain|network|vga|display" || true'
section "pci devices (sysfs fallback, works without lspci)"
if [[ -d /sys/bus/pci/devices ]]; then
    for d in /sys/bus/pci/devices/*; do
        printf '%s: vendor=%s device=%s class=%s driver=%s\n' \
            "${d##*/}" \
            "$(cat "$d/vendor" 2>/dev/null)" \
            "$(cat "$d/device" 2>/dev/null)" \
            "$(cat "$d/class" 2>/dev/null)" \
            "$(basename "$(readlink "$d/driver" 2>/dev/null)" || echo none)" >>"$out"
    done
fi
run "lsusb (tree)" lsusb -t
run "lsusb (verbose-ish)" sh -c 'lsusb | sort || true'
run "ip link" ip link
# shellcheck disable=SC2016
run_log "network drivers" /proc/net/dev \
    sh -c 'ls /sys/class/net; echo; for d in /sys/class/net/*; do echo "-- $d -> $(readlink "$d")"; done'

# --- kernel log --------------------------------------------------------
# thunderbolt / usb4 / tunneling, current and previous boot
for boot in 0 -1; do
    section "kernel log boot ${boot} (thunderbolt/usb4/xdomain)"
    journalctl -b "$boot" -k --no-pager 2>>"$out" |
        grep -iE 'thunderbolt|bolt|usb4|tbt|xdomain|icl|dlpc|dpcd|dma' |
        cat >>"$out" 2>&1 || echo "(no matching lines or journal unreadable)" >>"$out"
done
# -u bolt targets the unit directly; grepping the full boot journal
# takes minutes on a long-uptime host.
run "journal boltd" journalctl -b -u bolt --no-pager
# display / DRM path (dock DP bridge vs. working Dell display)
run "kernel log (drm/gpu)" sh -c 'journalctl -b -k --no-pager | grep -iE "drm|i915|amdgpu|nouveau" || true'
# USB side of the dock
run "kernel log (usb/xhci)" sh -c 'journalctl -b -k --no-pager | grep -iE "usb|xhci|hid" || true'
# previous boot kernel log (cold-boot plug-in often only visible there)
run "kernel log previous boot (filtered)" sh -c 'journalctl -b -1 -k --no-pager | grep -iE "thunderbolt|usb4|tbt|usb|xhci|drm" | tail -n 400 || true'
# full tail of current kernel log, for context around plug-in
run "kernel log tail" sh -c 'journalctl -b -k --no-pager | tail -n 200'

# --- DRM connector state -----------------------------------------------
# The decisive signal: does the GPU see the dock's DP tunnel as connected?
section "drm connectors"
if [[ -d /sys/class/drm ]]; then
    for c in /sys/class/drm/card*-*; do
        [[ -d $c ]] || continue
        printf '%s: status=%s enabled=%s dpms=%s\n' \
            "${c##*/}" \
            "$(cat "$c/status" 2>/dev/null)" \
            "$(cat "$c/enabled" 2>/dev/null)" \
            "$(cat "$c/dpms" 2>/dev/null)" >>"$out"
    done
    section "drm MST topology (aux devices)"
    for d in /sys/kernel/debug/dri/*/dp_mst_topology; do
        [[ -r $d ]] && {
            echo "-- $d" >>"$out"
            cat "$d" >>"$out" 2>&1
        }
    done
    ls /sys/class/drm >>"$out" 2>&1
else
    echo "no /sys/class/drm" >>"$out"
fi

# --- thunderbolt debugfs: DP/PCIe tunnels ----------------------------
# DP tunnels are only visible here; nothing in /sys/bus/thunderbolt
# exposes them.
section "thunderbolt debugfs (tunnels)"
if [[ -d /sys/kernel/debug/thunderbolt ]]; then
    find /sys/kernel/debug/thunderbolt -type f -print -exec cat {} \; >>"$out" 2>&1 || true
else
    echo "no /sys/kernel/debug/thunderbolt (need root + debugfs mounted)" >>"$out"
fi

# --- fwupd: dock firmware versions + updates -------------------------
# TBT3 docks are on LVFS; fwupd is the Linux-native update path.
section "fwupd devices"
if command -v fwupdmgr >/dev/null 2>&1; then
    fwupdmgr get-devices --show-all-devices >>"$out" 2>&1 || true
    section "fwupd updatable"
    fwupdmgr get-updates >>"$out" 2>&1 || true
    section "fwupd remotes"
    fwupdmgr get-remotes >>"$out" 2>&1 || true
else
    echo "fwupdmgr not found in PATH" >>"$out"
fi

# --- thunderbolt security (needs root) --------------------------------
if [[ ${EUID} -ne 0 ]]; then
    section "root-only sections"
    echo "not running as root; re-running with sudo for privileged sections" >>"$out"
    section "dmesg tail (via sudo, may prompt)"
    sudo -n dmesg 2>>"$out" | tail -n 200 >>"$out" 2>&1 || true
else
    run "dmesg tail" sh -c 'dmesg | tail -n 200'
    # shellcheck disable=SC2016
    run "thunderbolt nvm files" sh -c 'ls -l /var/lib/thunderbolt/ 2>/dev/null; for f in /var/lib/thunderbolt/*; do echo "-- $f"; cat "$f"; done 2>/dev/null'
    run "dmidecode (system + baseboard)" dmidecode -t system -t baseboard
fi

echo >>"$out"
echo "===== done =====" >>"$out"
# make the evidence readable for the calling user even under sudo
[[ ${EUID} -eq 0 ]] && chmod a+r "$out"
echo "evidence written to: $out" >&2
