#!/usr/bin/env bash

docker build -t obs-backgroundremoval-builder .
mkdir -p ./out
docker run -it -v "$(readlink -f ./out)":/out obs-backgroundremoval-builder
