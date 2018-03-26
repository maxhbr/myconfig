#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

pre=$1
post=$2

if [ -f /proc/acpi/bbswitch ]; then
    if grep -q ON /proc/acpi/bbswitch; then
        echo -n "${pre}BB${post}"
    fi
fi

if ifconfig tun0 > /dev/null; then
    echo -n "${pre}VPN${post}"
fi

