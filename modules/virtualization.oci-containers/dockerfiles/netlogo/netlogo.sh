#!/usr/bin/env bash
# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -x

docker rm --force netlogo >/dev/null 2>&1

set -e

version=5.3.1

docker build -t netlogo --rm=true --force-rm=true - <<EOF
FROM ubuntu:trusty
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y wget
RUN useradd -ms /bin/bash netlogo
USER netlogo
WORKDIR /home/netlogo
RUN wget https://ccl.northwestern.edu/netlogo/${version}/NetLogo-${version}-64.tgz \
 && tar xzf NetLogo-${version}-64.tgz

ENV DISPLAY :0
CMD netlogo-${version}-64/NetLogo
EOF

XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -

docker run -ti \
       -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
       -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
       --volume /dev/snd:/dev/snd \
       --volume /tmp/.X11-unix:/tmp/.X11-unix \
       --name=netlogo \
       netlogo $1
