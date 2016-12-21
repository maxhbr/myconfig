#!/usr/bin/env bash

set -ex
workdir=$(pwd)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$DIR"

docker build -t mhuber/tex --rm=true --force-rm=true .
docker run -it --net=host -v "$workdir:/workdir" mhuber/tex \
       gosu "$(id -u):$(id -g)" \
       latexmk -outdir="/workdir/out" -pdf -pvc
