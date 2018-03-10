#!/usr/bin/env bash
# written by: maximilian.huber@tngtech.com
set -e

################################################################################
docker="$(docker info &> /dev/null || echo "sudo") docker"

build() {
    $docker build -t signal --rm=true --force-rm=true - <<EOF
FROM ubuntu:xenial
ENV DEBIAN_FRONTEND noninteractive

RUN set -x \
 && apt-get update && apt-get -y install curl apt-transport-https \
 && (curl -s https://updates.signal.org/desktop/apt/keys.asc | apt-key add -) \
 && (echo "deb [arch=amd64] https://updates.signal.org/desktop/apt xenial main" >> /etc/apt/sources.list.d/signal-xenial.list)\
 && apt-get update && apt-get -y install signal-desktop \
 && rm -rf /var/lib/apt/lists/*

RUN useradd -ms /bin/bash signal
USER signal
RUN mkdir -p /home/signal/.config/Signal/
WORKDIR /home/signal/.config/Signal/
VOLUME /home/signal/.config/Signal/

CMD signal-desktop
EOF
}

run() {
    local TMP=$TMP/signal_sh
    mkdir -p $TMP
    XSOCK=$TMP/.X11-unix
    XAUTH=$TMP/.docker.xauth
    xauth nlist :0 \
        | sed -e 's/^..../ffff/' \
        | xauth -f $XAUTH nmerge -

    if [ ! "$($docker ps -aq -f name=signal)" ]; then
        $docker run \
                -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
                -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
                --volume /dev/snd:/dev/snd \
                --volume /tmp/.X11-unix:/tmp/.X11-unix \
                --name=signal \
                signal
    else
        $docker start signal
    fi
}

rm() {
    $docker rm \
            --force signal \
            >/dev/null 2>&1 || true
}

################################################################################
if [[ $# -eq 0 ]] ; then
    run
else
    $@
fi
