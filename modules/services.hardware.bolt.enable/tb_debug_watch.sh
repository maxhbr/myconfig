#!/usr/bin/env bash

# Live-capture Thunderbolt/USB4/DP hotplug events while you plug a dock in.
# Run ON THE HOST with sudo, then (un)plug the dock. Ctrl-C to stop.
# Output goes to a timestamped file in ~/_tb_debug/.
set -uo pipefail

out="${HOME}/_tb_debug/tb-watch-$(date +%Y%m%d-%H%M%S).log"
mkdir -p "$(dirname "$out")"

[[ ${EUID} -eq 0 ]] && chmod a+r "$out" 2>/dev/null || true

echo "watching kernel log for thunderbolt/usb4/drm events -> $out"
echo "plug/unplug the dock now; Ctrl-C to stop"

# also snapshot connector state on start, for reference
{
    echo "===== connectors at start ====="
    for c in /sys/class/drm/card*-*; do
        [[ -e $c/status ]] || continue
        printf '%s: %s\n' "${c##*/}" "$(cat "$c/status")"
    done
} >>"$out"

journalctl -k -f --no-pager 2>&1 |
    grep --line-buffered -aiE 'thunderbolt|usb4|bolt|xdomain|DM_MST|HPD|amdgpu.*drm|drm.*DP' |
    while IFS= read -r line; do
        printf '%s\n' "$line" | tee -a "$out"
    done

{
    echo
    echo "===== connectors at end ====="
    for c in /sys/class/drm/card*-*; do
        [[ -e $c/status ]] || continue
        printf '%s: %s\n' "${c##*/}" "$(cat "$c/status")"
    done
} >>"$out"

[[ ${EUID} -eq 0 ]] && chmod a+r "$out" 2>/dev/null || true
echo "watch log written to: $out" >&2
