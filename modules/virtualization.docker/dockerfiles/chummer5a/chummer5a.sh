#!/usr/bin/env bash
# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -x

docker rm --force chummer5a >/dev/null 2>&1

set -e

docker build -t chummer5a --rm=true --force-rm=true - <Dockerfile

XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 \
    | sed -e 's/^..../ffff/' \
    | xauth -f $XAUTH nmerge -

docker run -ti \
       -v "$XSOCK":"$XSOCK" -e DISPLAY="$DISPLAY" \
       -v "$XAUTH":"$XAUTH" -e XAUTHORITY="$XAUTH" \
       -v "$(pwd):/mount" \
       --name=chummer5a \
       chummer5a "$@"
