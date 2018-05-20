#!/usr/bin/env bash
# based on https://gitea.vbgn.be/vierbergenlars/xmonad/src/branch/master/battery-monitor.sh
set -e
trap 'xmessage -nearmouse "WARNING: battery monitor died!"' EXIT

was_notified=0
while sleep 31; do
    acpi_data=$(acpi -b)
    status=$(echo -n "$acpi_data" | awk -F'[,:%]' '{print $2}')
    capacity=$(echo -n "$acpi_data" | awk -F'[,:%]' '{print $3}')
    echo "Status: $status; Capacity: $capacity; was_notified: $was_notified; acpi_data: $acpi_data"

    case $status in
        "Discharging")
            if [[ "$capacity" -lt 7 ]]; then
                bash -c 'echo "Battery critical, suspending"; $acpi_data' | xmessage -nearmouse -file -&
                sleep 5
                systemctl suspend
                sleep 600 # Wait 10 minutes, so we only hibernate once
            else
                # if [[ "$capacity" -lt 15 && "$was_notified" -lt "$(date -d 'now - 1 minutes' +%s)" ]]; then
                if [[ "$was_notified" -lt "$(date -d 'now - 1 minutes' +%s)" ]]; then
                    xmessage -nearmouse "$acpi_data"&
                    was_notified=$(date +%s)
                fi
            fi
        ;;
        *)
            was_notified=0
        ;;
    esac
done
