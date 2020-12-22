#!/usr/bin/env bash
set -eo pipefail

################################################################################
tag=scancodeworkbench

buildImageIfMissing() {
    if [[ "$(docker images -q $tag 2> /dev/null)" == "" ]]; then
        docker build -t $tag --rm=true --force-rm=true - <<EOF
FROM ubuntu:latest
ENV DEBIAN_FRONTEND noninteractive
WORKDIR /scwb
RUN set -x \
 && apt-get update \
 && apt-get install -y firefox wget libgtk-3-dev gconf-service libasound2 libatk1.0-0 libatk-bridge2.0-0 libc6 libcairo2 libcups2 libdbus-1-3 libexpat1 libfontconfig1 libgcc1 libgconf-2-4 libgdk-pixbuf2.0-0 libglib2.0-0 libgtk-3-0 libnspr4 libpango-1.0-0 libpangocairo-1.0-0 libstdc++6 libx11-6 libx11-xcb1 libxcb1 libxcomposite1 libxcursor1 libxdamage1 libxext6 libxfixes3 libxi6 libxrandr2 libxrender1 libxss1 libxtst6 ca-certificates fonts-liberation libappindicator1 libnss3 lsb-release xdg-utils wget \
 && rm -rf /var/lib/apt/lists/* \
 && wget https://github.com/nexB/scancode-workbench/releases/download/v3.1.1/ScanCode-Workbench-linux-x64-3.1.1.tar.gz \
 && tar -xzf ScanCode-Workbench-linux-x64-3.1.1.tar.gz

CMD ScanCode-Workbench-linux-x64-3.1.1/ScanCode-Workbench
EOF
    fi
}

run() {
    set -x
    local TMP=$TMP/scancodeworkbench_sh
    mkdir -p $TMP
    XSOCK=$TMP/.X11-unix
    XAUTH=$TMP/.docker.xauth
    xauth nlist :0 \
        | sed -e 's/^..../ffff/' \
        | xauth -f $XAUTH nmerge -

    if [ ! "$(docker ps -aq -f name=scancodeworkbench)" ]; then
        docker run \
            --rm \
            `#-u $(id -u $USER):$(id -g $USER)` \
            -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
            -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
            --volume /dev/snd:/dev/snd \
            --volume /tmp/.X11-unix:/tmp/.X11-unix \
            --volume "$(pwd)":/workdir \
            --name=scancodeworkbench \
            $tag
    else
        docker start scancodeworkbench
    fi
}

forceRebuild() {
    docker rm \
            --force scancodeworkbench \
            >/dev/null 2>&1 || true
}

################################################################################
if [[ "$1" == "-fr" ]]; then
    shift
    forceRebuild
fi
buildImageIfMissing
run
