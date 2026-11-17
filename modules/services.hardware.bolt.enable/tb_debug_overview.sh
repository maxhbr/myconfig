#!/usr/bin/env bash

# Fast read-only overview of the current Thunderbolt / USB4 / Type-C state.
# Answers, in a few lines each:
#   1. Which TB devices are connected, and are they authorized (tunneling)?
#   2. Which devices failed TB and fell back to another mode
#      (USB / DisplayPort alt mode instead of a TB tunnel)?
#   3. Is the battery charging, over which port and which partner device?
#
# Known quirk of this host (Framework 13, AMD): UCSI is broken
# (ucsi_acpi "unknown error 256"), so /sys/class/typec stays empty and
# port roles / alt modes are not available. The script falls back to
# sysfs thunderbolt + power_supply + kernel log in that case.
#
# No root required for the main sections; they degrade gracefully.
# Run ON THE HOST: tb_debug_overview   (sudo adds DP/PCIe tunnel view)
set -uo pipefail

hr() { printf '%s\n' "------------------------------------------------------------"; }
section() {
    printf '\n== %s ==\n' "$1"
}

# cat a sysfs file if it exists, else print placeholder
sysf() {
    if [[ -r $1 ]]; then
        cat "$1" 2>/dev/null
    else
        echo "-"
    fi
}

printf 'Thunderbolt overview: %s  %s  kernel %s' \
    "$(hostname)" "$(date '+%F %T')" "$(uname -r)"
command -v boltctl >/dev/null 2>&1 && printf '  (bolt available)' || printf '  (boltctl missing)'
printf '\n'

# --- 1. TB/USB4 devices -----------------------------------------------
section "TB/USB4 devices (sysfs)"
# Path scheme: "X-0" = host router, "X-Y:Z.r" (contains a dot) = retimer,
# everything else "X-Y[:...]" = peripheral device behind the host.
tb_periphs=()    # connected peripheral devices (dir names)
tb_periph_auth=0 # ... of which authorized
for d in /sys/bus/thunderbolt/devices/*; do
    [[ -d $d ]] || continue
    dev="${d##*/}"
    name="$(sysf "$d/device_name")"
    case $dev in
        domain*)
            continue
            ;;
        *-0)
            printf '%-8s host router    auth=%-3s gen=%s\n' \
                "$dev" "$(sysf "$d/authorized")" "$(sysf "$d/generation")"
            continue
            ;;
        *.*) # retimer: vendor/device ids live in the kernel log, not sysfs
            printf '%-8s retimer\n' "$dev"
            continue
            ;;
    esac
    auth="$(sysf "$d/authorized")"
    printf '%-8s %-30s auth=%-3s gen=%-12s speed=%-10s nvm=%s\n' \
        "$dev" "$name" "$auth" "$(sysf "$d/generation")" \
        "$(sysf "$d/speed")" "$(sysf "$d/nvm_version")"
    [[ $auth == 0 ]] &&
        printf '         !! NOT AUTHORIZED - PCIe/USB tunneling blocked (security=%s)\n' \
            "$(cat "${d%/*}"/domain*/security 2>/dev/null | head -n1)"
    tb_periphs+=("$dev")
    [[ $auth == 1 ]] && tb_periph_auth=$((tb_periph_auth + 1))
done
if [[ ${#tb_periphs[@]} -eq 0 ]]; then
    echo "no TB/USB4 peripheral devices on the bus (host routers don't count)"
fi

for dom in /sys/bus/thunderbolt/devices/domain*; do
    [[ -d $dom ]] || continue
    printf 'domain %s: security=%s nvm=%s\n' \
        "${dom##*/}" "$(sysf "$dom/security")" "$(sysf "$dom/nvm_version")"
done

# bolt view: names paired with status, connected ones marked.
# bolt knows stored-but-unplugged devices too, so this shows what the
# system has seen vs what is on the bus right now.
if command -v boltctl >/dev/null 2>&1; then
    section "bolt: known devices (=> currently connected)"
    boltctl list 2>/dev/null | awk '
        /^ *[o*] / { name = substr($0, index($0, $2)); conn = ($1 == "*") }
        /status:/  { printf "%s %s: %s\n", (conn ? "=>" : "  "), name, $3 }
    ' || echo "(boltctl list failed)"
else
    section "bolt"
    echo "boltctl not found in PATH"
fi

# --- 2. Fallback detection --------------------------------------------
# Signal hierarchy:
#   TB peripheral on bus + authorized  -> tunneling, dock USB hubs are
#                                        its tunneled xHCI (normal)
#   dock USB devices but NO TB periph  -> TB handshake failed, dock
#                                        fell back to USB/DP-only mode
#   /sys/class/typec empty             -> UCSI broken on this host; alt
#                                        modes not inspectable
section "Type-C ports"
typec_ports=0
for p in /sys/class/typec/port-*; do
    [[ -d $p ]] || continue
    typec_ports=$((typec_ports + 1))
    printf '%s: orientation=%s data=%s power=%s\n' \
        "${p##*/}" "$(sysf "$p/orientation")" "$(sysf "$p/data_role")" \
        "$(sysf "$p/power_role")"
    if [[ -d $p/partner ]]; then
        vid="$(sed -n 's/^USB_VENDOR_ID=//p' "$p/partner/uevent" 2>/dev/null)"
        pid="$(sed -n 's/^USB_PRODUCT_ID=//p' "$p/partner/uevent" 2>/dev/null)"
        printf '  partner:'
        [[ -n $vid ]] && printf ' usb=%s:%s' "$vid" "$pid"
        printf '\n'
        for m in "$p"/partner/alternate_modes.*/mode*; do
            [[ -f $m/active ]] || continue
            printf '  alt mode %-20s active=%s (%s)\n' \
                "${m##*/}" "$(sysf "$m/active")" "$(sysf "$m/description")"
        done
        # a TB partner with no alt modes and no TB sysfs entry = fell back
        if ! compgen -G "$p/partner/alternate_modes.*/mode*/active" >/dev/null; then
            printf '  (no alternate mode negotiated)\n'
        fi
    else
        echo '  partner: none (nothing plugged in)'
    fi
done
if [[ $typec_ports -eq 0 ]]; then
    echo "no typec ports exposed - UCSI controller not working (see kernel log below)"
fi

section "Dock USB devices (fallback check)"
dock_usb=0
if command -v lsusb >/dev/null 2>&1; then
    # 17ef=Lenovo, 413c=Dell dock families (WD19/WD22, ThinkPad docks)
    if lsusb 2>/dev/null | grep -iE '17ef|413c|thinkpad|wd1[92]|dock'; then
        dock_usb=1
        echo
        if [[ ${#tb_periphs[@]} -gt 0 ]]; then
            echo "OK: dock USB hubs present AND TB device on bus -> USB runs through the TB PCIe tunnel (normal)"
        else
            echo "!! FALLBACK: dock USB devices present but NO TB device on the bus"
            echo "   the dock is running in USB/DisplayPort-only mode (TB handshake failed)"
        fi
    else
        echo "no dock-looking USB devices"
    fi
else
    echo "lsusb not available"
fi

# --- 3. Charging ------------------------------------------------------
section "Power supplies"
ac_online=""
for bat in /sys/class/power_supply/*; do
    [[ -d $bat ]] || continue
    btype="$(sysf "$bat/type")"
    case $btype in
        Battery | UPS | Mains) ;;
        *) continue ;;
    esac
    if [[ -r $bat/power_now ]]; then
        watts=$(awk -v f="$(cat "$bat/power_now")" 'BEGIN { printf "%.1f", f / 1000000 }')
    elif [[ -r $bat/voltage_now && -r $bat/current_now ]]; then
        watts=$(awk -v v="$(cat "$bat/voltage_now")" -v c="$(cat "$bat/current_now")" \
            'BEGIN { printf "%.1f", (v / 1000000) * (c / 1000000) }')
    else
        watts="-"
    fi
    printf '%-8s type=%-7s %-13s' "${bat##*/}" "$btype" "$(sysf "$bat/status")"
    if [[ $btype == Battery ]]; then
        printf ' %s%%  %s W' "$(sysf "$bat/capacity")" "$watts"
    else
        printf ' online=%s' "$(sysf "$bat/online")"
        [[ $(sysf "$bat/online") == 1 ]] && ac_online="${bat##*/}"
    fi
    printf '\n'
done

charge_src="on battery"
bat_status="unknown"
bat_cap="-"
for bat in /sys/class/power_supply/*; do
    [[ -d $bat && $(sysf "$bat/type") == Battery ]] || continue
    bat_status="$(sysf "$bat/status")"
    bat_cap="$(sysf "$bat/capacity")"
    break
done
if [[ -n $ac_online ]]; then
    if [[ ${#tb_periphs[@]} -gt 0 ]]; then
        # TB3 docks feed the EC over the TB cable; the EC exposes it as an
        # ACPI mains adapter, so typec power_role never shows "sink" here.
        charge_src="AC adapter $ac_online, fed by TB device: ${tb_periphs[0]##*/}"
    else
        charge_src="AC adapter $ac_online (no TB device connected: barrel charger or plain USB-C)"
    fi
    # AC online but battery not drawing = EC charge-hold near full, still
    # powered from the dock, not from the battery.
    if [[ $bat_status == "Not charging" || $bat_status == "Full" ]]; then
        charge_src="$charge_src (battery holds at $bat_cap%)"
    fi
fi
printf 'power source: %s\n' "$charge_src"

# --- 4. Failure signals from this boot's kernel log --------------------
section "Kernel log, this boot (failures / retimer flapping)"
if journalctl -b -k --no-pager >/dev/null 2>&1; then
    tb_log=$(journalctl -b -k --no-pager 2>/dev/null | grep -iE 'thunderbolt|bolt|usb4|typec|ucsi')
    retimer_up=$(grep -ic 'new retimer found' <<<"$tb_log" || true)
    retimer_down=$(grep -ic 'retimer disconnected' <<<"$tb_log" || true)
    echo "retimer events: $retimer_up connect / $retimer_down disconnect"
    [[ $retimer_down -gt 3 ]] &&
        echo "  !! retimer flap: cable/dock link is unstable"
    hits=$(grep -viE 'registered|probing|using' <<<"$tb_log" |
        grep -iE 'fail|error|timeout|denied|fallback|retry|not responding|deauthoriz|unauthoriz|limiting' |
        tail -n 15)
    if [[ -n $hits ]]; then
        printf '%s\n' "$hits"
    else
        echo "no TB/typec errors logged this boot"
    fi
else
    echo "journal not readable (try sudo or systemd-journal group)"
fi

# DP tunnels: dock displays show up as connected DRM connectors
section "Display connectors"
drm_conn=0
for c in /sys/class/drm/card*-*; do
    [[ -r $c/status ]] || continue
    st="$(cat "$c/status" 2>/dev/null)"
    case ${c##*/} in
        # internal panel / writeback are not tunnel outputs
        *-eDP-* | *-LVDS-* | *-Writeback-*) skip=1 ;;
        *) skip=0 ;;
    esac
    [[ $st == connected ]] && [[ $skip == 0 ]] && drm_conn=$((drm_conn + 1))
    printf '%-18s %s\n' "${c##*/}" "$st"
done
if [[ $drm_conn -gt 0 ]]; then
    echo "($drm_conn external display(s) connected -> DP tunnel of the dock is up)"
else
    echo "(no external display connected via DP)"
fi

section "PCIe/DP tunnels (debugfs, needs root)"
if [[ -d /sys/kernel/debug/thunderbolt ]]; then
    for t in /sys/kernel/debug/thunderbolt/*/; do
        [[ -r $t ]] || continue
        printf '%s:\n' "${t%/}"
        grep -H . "$t"* 2>/dev/null | sed 's/^/  /' || true
    done
else
    echo "debugfs not accessible - rerun with sudo to see DP/PCIe tunnels"
fi

echo
hr
echo "summary:"
echo "  TB peripherals on bus: ${#tb_periphs[@]}, authorized: $tb_periph_auth"
echo "  battery: $bat_status ${bat_cap}%"
echo "  power: $charge_src"
if [[ $dock_usb == 1 && ${#tb_periphs[@]} -eq 0 ]]; then
    echo "  !! dock in USB fallback mode"
fi
exit 0
