#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

wwanNetwork="Vodafone Vorgabe"
wwanDevice="cdc-wdm0"

exec &> >(tee -a /tmp/wwan.sh.log)

startDevice() {
    if [[ "-r" != "$1" ]]; then
        sudo systemctl start ModemManager.service
    else
        sudo systemctl restart ModemManager.service
    fi
}

connectWWAN() {
    nmcli c up "$wwanNetwork" ifname $wwanDevice || true
}

start() {
    lastRun="$(date +"%Y-%m-%d %H:%M:%S")"
    startDevice

    # TODO: internationalization
    until connectWWAN | tee /dev/tty | grep -q 'erfolgreich aktiviert'; do
        journalctl -u  ModemManager.service -b --since="$lastRun"
        sleep 5
        lastRun="$(date +"%Y-%m-%d %H:%M:%S")"
        startDevice -r
    done
}

stop() {
    nmcli c down "$wwanNetwork" || true

    lastRun="$(date +"%Y-%m-%d %H:%M:%S")"
    sudo systemctl stop ModemManager.service || true
    journalctl -u  ModemManager.service -b --since="$lastRun"
}

if [ $# -eq 0 ]; then
    start
else
    ([[ ! -n "$(type -t $1)" ]] || [ "$(type -t $1)" != "function" ] ) && exit 1
    $@
fi
