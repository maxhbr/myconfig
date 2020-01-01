#!/usr/bin/env bash
# see:
# - https://hub.docker.com/r/grafana/grafana/
# - https://grafana.com/docs/grafana/latest/installation/docker/

set -e
GRAFANA="${GRAFANA:-/opt/grafana}"
GRAFANA_USER="${GRAFANA_USER:-grafana}"

have() { type "$1" &> /dev/null; }
setup() {
    echo "... SETUP"
    if ! id -u "$GRAFANA_USER"; then
        if have nixos-rebuild; then
            echo "use nixos to create the user"
            exit 1
        fi
        sudo useradd -d "$GRAFANA" -r -s /sbin/nologin $GRAFANA_USER
        sudo usermod -a -G $GRAFANA_USER "$USER"
    fi
    sudo mkdir -p "$GRAFANA/data"
    sudo chown -R $GRAFANA_USER:$GRAFANA_USER "$GRAFANA"
}
update() {
    echo "... UPDATE"
    sudo docker stop grafana && sudo docker rm grafana || true
    sudo docker pull grafana/grafana:latest
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
         --name grafana \
         --restart=unless-stopped \
         $args \
         -v "$GRAFANA/data":/var/lib/grafana \
         -d \
         --user "$(id -u $GRAFANA_USER)":"$(id -g $GRAFANA_USER)" \
         -p 3000:3000 \
         grafana/grafana:latest
}

setup
update
run

