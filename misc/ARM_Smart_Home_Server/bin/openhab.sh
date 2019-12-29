#!/usr/bin/env bash
# see:
# - https://www.openhab.org/docs/installation/docker.html
# - https://hub.docker.com/r/openhab/openhab/

set -e
OHAB="${OHAB:-/opt/openhab}"
OHAB_USER="${OHAB_USER:-openhab}"

have() { type "$1" &> /dev/null; }
setup() {
    echo "... SETUP"
    if ! id -u "$OHAB_USER"; then
        if have nixos-rebuild; then
            echo "use nixos to create the user"
            exit 1
        fi
        sudo useradd -d "$OHAB" -r -s /sbin/nologin $OHAB_USER
        sudo usermod -a -G $OHAB_USER "$USER"
    fi
    sudo mkdir -p "$OHAB/conf"
    sudo mkdir -p "$OHAB/userdata"
    sudo mkdir -p "$OHAB/addons"
    sudo chown -R $OHAB_USER:$OHAB_USER "$OHAB"
}
update() {
    echo "... UPDATE"
    sudo docker stop openhab && sudo docker rm openhab || true
    sudo docker pull openhab/openhab:latest
}
run() {
    args=""
    if [[ -f /etc/localtime ]]; then
        args="$args -v /etc/localtime:/etc/localtime:ro"
    fi
    if [[ -f /etc/timezone ]]; then
        args="$args -v /etc/timezone:/etc/timezone:ro"
    fi

    echo "... RUN"
    sudo docker run \
         --name openhab \
         --net=host \
         $args \
         -v "$OHAB/conf":/openhab/conf \
         -v "$OHAB/userdata":/openhab/userdata \
         -v "$OHAB/addons":/openhab/addons \
         -d \
         -e USER_ID="$(id -u $OHAB_USER)" \
         -e GROUP_ID="$(id -g $OHAB_USER)" \
         -e OPENHAB_HTTP_PORT="8080" \
         -e OPENHAB_HTTPS_PORT="8443" \
         --restart=unless-stopped \
         openhab/openhab:latest
}

setup
update
run

