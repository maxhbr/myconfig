#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

have() { type "$1" &> /dev/null; }

have nix-shell && {
    have speedtest-cli || {
        echo "start via nix-shell"
        exec nix-shell -p speedtest-cli wget --command $0 $@
    }
}

CSV=~/myspeedtest_sh.csv
LOG=~/myspeedtest_sh.log

checkIfConnected() {
    if ! ping -c1 heise.de > /dev/null 2>&1; then
        echo "not connected: ping: $(date)" | tee -a $LOG
        if ! wget -O - heise.de > /dev/null 2>&1; then
            echo "not connected: wget: $(date)" | tee -a $LOG
            return 1
        fi
    fi
}

run() {
    echo "start: $(date)"
    if checkIfConnected; then
        if [[ -f $CSV ]]; then
            speedtest-cli --csv-header
        else
            speedtest-cli --csv-header | tee $CSV
        fi
        speedtest-cli --csv | tee -a $CSV
    else
        echo ",,-- not connected --,$(date --utc +"%FT%T.%6NZ"),,,,,," | tee -a $CSV
    fi
    echo "end: $(date)"
}

if [[ $1 == "--loop" ]]; then
    while run; do
        sleep 300
    done
else
    run
fi
