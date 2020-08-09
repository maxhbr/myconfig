#!/usr/bin/env bash
# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -x

docker rm --force zerene >/dev/null 2>&1

set -e
version="T2020-05-22-1330"

docker build -t zerene --rm=true --force-rm=true - <<EOF
FROM openjdk:8-jre

RUN apt-get update && \
    apt-get install -y wget unzip \
                       libxext6 libxrender1 libxtst6 libxi6 libfreetype6
RUN set -x \
 && mkdir /app \
 && cd /app \
 && wget 'https://zerenesystems.com/stacker/downloads/ZS-Linux-Intel-64bit-${version}.zip' \
 && unzip 'ZS-Linux-Intel-64bit-${version}.zip' -d /app/ \
 && rm -rf 'ZS-Linux-Intel-64bit-${version}.zip' \
 && chmod +x /app/ZereneStacker/*.zslinux \
 && chmod +x /app/ZereneStacker/ZereneStacker.bsh \
 && chmod +x /app/ZereneStacker/ZereneStacker \
 && chmod +x /app/ZereneStacker/jre/bin/* \
 && chmod +x /app/ZereneStacker/jre/lib/jspawnhelper

RUN useradd -ms /bin/bash zerene
USER zerene

WORKDIR /workdir

ENV DISPLAY :0
CMD set -x \
 && java \
         -classpath '/app/ZereneStacker/ZereneStacker.jar:/app/ZereneStacker/JREextensions/*' \
         com.zerenesystems.stacker.gui.MainFrame
EOF

XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -

docker run -ti \
       -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
       -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
       --volume /dev/snd:/dev/snd \
       --volume /tmp/.X11-unix:/tmp/.X11-unix \
       --volume "$(pwd):/workdir" \
       --name=zerene \
       zerene $1
