#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

sudo systemctl start docker

set -x

sudo docker rm --force minizinc >/dev/null 2>&1

set -e

sudo docker build -t minizinc --rm=true --force-rm=true - <<EOF
FROM ubuntu:latest
ENV DEBIAN_FRONTEND noninteractive

ENV _update="apt-get update"
ENV _install="apt-get install -y --no-install-recommends"
ENV _cleanup="eval apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*"
ENV _purge="apt-get purge -y --auto-remove"

RUN \$_update \
 && \$_install wget \
        libgtk2.0-0 libglu1-mesa qt5-default libxslt1-dev libgstreamer-plugins-base0.10-dev qtgstreamer-plugins-qt5 \
 && \$_cleanup

RUN useradd -ms /bin/bash minizinc
USER minizinc
WORKDIR /home/minizinc
RUN set -x \
 && wget --no-check-certificate https://github.com/MiniZinc/MiniZincIDE/releases/download/2.1.2/MiniZincIDE-2.1.2-bundle-linux-x86_64.tgz \
 && tar xzf MiniZincIDE-2.1.2-bundle-linux-x86_64.tgz \
 && rm MiniZincIDE-2.1.2-bundle-linux-x86_64.tgz \
 && mv MiniZincIDE-2.1.2-bundle-linux-x86_64/* ./ \
 && rmdir MiniZincIDE-2.1.2-bundle-linux-x86_64

ENV DISPLAY :0
VOLUME /home/minizinc
VOLUME /home/minizinc/workspace
CMD ./MiniZincIDE.sh
EOF

XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -

sudo docker run -ti \
       -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
       -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
       -v $(realpath ./):/home/minizinc/workspace \
       --volume /tmp/.X11-unix:/tmp/.X11-unix \
       --name=minizinc \
       minizinc $1
