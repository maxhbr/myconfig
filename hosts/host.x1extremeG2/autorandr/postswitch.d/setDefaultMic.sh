#!/usr/bin/env bash

set -euo pipefail

echo "looking for mic ..."
if lsusb | grep 0d8c:0005; then
    echo "... found"
    pacmd set-default-source alsa_input.usb-BLUE_MICROPHONE_Blue_Snowball_797_2020_07_31_01554-00.mono-fallback
fi
