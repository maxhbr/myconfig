#!/usr/bin/env bash

set -e

have() { type "$1" &> /dev/null; }

OHAB="/opt/openhab"
if ! id -u openhab; then
    if have nixos-rebuild; then
        echo "use nixos to create the user"
        exit 1
    fi
    sudo useradd -d "$OHAB" -r -s /sbin/nologin openhab
    sudo usermod -a -G openhab "$USER"
fi
sudo mkdir -p "$OHAB/conf"
sudo mkdir -p "$OHAB/userdata"
sudo mkdir -p "$OHAB/addons"
sudo chown -R openhab:openhab "$OHAB"

args=""
if [[ -f /etc/localtime ]]; then
    args="$args -v /etc/localtime:/etc/localtime:ro"
fi
if [[ -f /etc/timezone ]]; then
    args="$args -v /etc/timezone:/etc/timezone:ro"
fi

sudo docker run \
        --name openhab \
        --net=host \
        "$args" \
        -v "$OHAB/conf":/openhab/conf \
        -v "$OHAB/userdata":/openhab/userdata \
        -v "$OHAB/addons":/openhab/addons \
        -d \
        -e USER_ID="$(id -u openhab)" \
        -e GROUP_ID="$(id -g openhab)" \
        -e OPENHAB_HTTP_PORT="8080" \
        -e OPENHAB_HTTPS_PORT="8443" \
        --restart=unless-stopped \
        openhab/openhab:latest

