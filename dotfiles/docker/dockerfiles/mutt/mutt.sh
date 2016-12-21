#!/usr/bin/env bash

set -ex

docker build -t mhuber/mutt --rm=true --force-rm=true .
docker run -it --net=host mhuber/mutt \
       bash
       # mutt
       # gosu "$(id -u):$(id -g)" \
