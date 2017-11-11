#!/usr/bin/env bash

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

