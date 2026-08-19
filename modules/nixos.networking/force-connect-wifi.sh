#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Force a WiFi connection to a given SSID.
#
# Usage: force-connect-wifi <ssid>
#        SSID=<ssid> force-connect-wifi
#
# - Exits successfully if the SSID is already the active connection.
# - Rescans and waits until the SSID becomes visible.
# - Prefers an existing saved profile so that no password prompt is needed.
set -euo pipefail

SSID="${SSID:-${1:-}}"
MAX_TRIES="${MAX_TRIES:-10}"
SLEEP_SECONDS="${SLEEP_SECONDS:-3}"

if [[ -z $SSID ]]; then
    echo "force-connect-wifi: ERROR: no SSID given" >&2
    echo "usage: force-connect-wifi <ssid>" >&2
    exit 2
fi

log() {
    echo "force-connect-$SSID: $*" >&2
}

# Is $SSID the currently active WiFi connection?
is_connected() {
    nmcli -t -f ACTIVE,SSID device wifi list --rescan no 2>/dev/null |
        grep -qxF "yes:$SSID"
}

# Is $SSID visible in the current scan results?
is_visible() {
    nmcli -t -f SSID device wifi list --rescan no 2>/dev/null |
        grep -qxF "$SSID"
}

# Name of a saved connection profile for $SSID, if any.
saved_profile() {
    nmcli -t -f NAME,TYPE connection show 2>/dev/null |
        while IFS=':' read -r name type; do
            case "$type" in
                802-11-wireless | wifi) ;;
                *) continue ;;
            esac
            local profile_ssid
            profile_ssid="$(nmcli -t -g 802-11-wireless.ssid connection show "$name" 2>/dev/null || true)"
            if [[ $profile_ssid == "$SSID" || $name == "$SSID" ]]; then
                echo "$name"
                return 0
            fi
        done
}

if is_connected; then
    log "already connected, nothing to do"
    exit 0
fi

if ! is_visible; then
    log "not visible yet, rescanning..."
    for ((try = 1; try <= MAX_TRIES; try++)); do
        nmcli device wifi rescan >/dev/null 2>&1 || true
        if is_visible; then
            log "became visible after $try scan(s)"
            break
        fi
        if ((try == MAX_TRIES)); then
            log "ERROR: SSID '$SSID' did not show up after $MAX_TRIES scans" \
                "(~$((MAX_TRIES * SLEEP_SECONDS))s). Is the radio enabled and in range?"
            exit 1
        fi
        sleep "$SLEEP_SECONDS"
    done
fi

profile="$(saved_profile | head -n1)"
if [[ -n $profile ]]; then
    log "using saved profile '$profile'"
    nmcli connection up id "$profile"
else
    log "no saved profile, connecting to '$SSID' directly"
    nmcli device wifi connect "$SSID"
fi

if is_connected; then
    log "connected"
else
    log "ERROR: connection command finished but '$SSID' is not active"
    exit 1
fi
