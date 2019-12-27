#!/usr/bin/env bash

set -e

OHAB="/opt/openhab"
mkdir -p "$OHAB/conf"
mkdir -p "$OHAB/userdata"
mkdir -p "$OHAB/addons"

if ! id -u openhab; then
    sudo useradd -d "$OHAB" -r -s /sbin/nologin openhab
    sudo usermod -a -G openhab "$USER"
fi

sudo docker run \
        --name openhab \
        --net=host \
        -v /etc/localtime:/etc/localtime:ro \
        `#-v /etc/timezone:/etc/timezone:ro` \
        -v "$OHAB/conf":/openhab/conf \
        -v "$OHAB/userdata":/openhab/userdata \
        -v "$OHAB/addons":/openhab/addons \
        -d \
        -e USER_ID="$UID" \
        -e GROUP_ID="$GID" \
        `#-e OPENHAB_HTTP_PORT=""` \
        `#-e OPENHAB_HTTPS_PORT=""` \
        --restart=unless-stopped \
        openhab/openhab:latest
