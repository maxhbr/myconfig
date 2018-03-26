#!/usr/bin/env bash
# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -ex
workdir=$(pwd)
cd $(dirname "${BASH_SOURCE[0]}")

XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -

# docker build -t mhuber/tex --rm=true --force-rm=true .
docker run -it --net=host \
       -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
       -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
       --volume /tmp/.X11-unix:/tmp/.X11-unix \
       -v "$workdir:/workdir" \
       mhuber/tex \
       gosu "$(id -u):$(id -g)" \
       latexmk -outdir="/workdir/out" -pdf -pvc
