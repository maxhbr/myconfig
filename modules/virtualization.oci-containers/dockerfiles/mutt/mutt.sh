#!/usr/bin/env bash
# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -ex

docker build -t mhuber/mutt --rm=true --force-rm=true .
docker run -it --net=host mhuber/mutt \
       bash
       # mutt
       # gosu "$(id -u):$(id -g)" \
