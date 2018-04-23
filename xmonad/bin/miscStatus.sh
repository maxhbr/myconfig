#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

delimiter=$1
startcol=$2
endcol=$3

pre="$delimiter $startcol"
post="$endcol "

have() { type "$1" &> /dev/null; }

if [ -f /proc/acpi/bbswitch ]; then
    if grep -q ON /proc/acpi/bbswitch; then
        echo -n "${pre}BB${post}"
    fi
fi

if ifconfig tun0 > /dev/null; then
    echo -n "${pre}VPN${post}"
fi

if have VBoxManage; then
    num=$(VBoxManage list runningvms | wc -l)
    if [[ "$num" -gt 0 ]]; then
        echo -n "${delimiter} VBs:${startcol}${num}${post}"
    fi
fi

if have docker; then
    if docker info &> /dev/null; then
        num=$(docker ps -q | wc -l)
        if [[ "$num" -gt 0 ]]; then
            echo -n "${delimiter} Ds:${startcol}${num}${post}"
        fi
    fi
fi

# BTSyncON=$(ps -A | grep -c btsync)
# if ! [[ $BTSyncON == "0" ]]; then
#   echo -n "${pre}BTSync${post}"
# fi
