#!/usr/bin/env bash
# Copyright 2019-2021 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

tag=mytern:latest

export DOCKER_BUILDKIT=1

export format=json
# export format=yaml

helpMsg() {
    cat<<EOF
usage:
  $0 --rm
  $0 -d path/to/Dockerfile [path/To/Out/Folder]
  $0 -i image:tag [path/To/Out/Folder]
  $0 path/To/Project/Folder
  $0 --help
EOF
}

#################################################################################
buildImageIfMissing() {
    if [[ "$(docker images -q $tag 2> /dev/null)" == "" ]]; then
        TERN=$(mktemp -d)
        trap 'rm -rf $TERN' EXIT
        git clone https://github.com/tern-tools/tern.git $TERN

        docker build \
            -f "$TERN/docker/Dockerfile" \
            -t $tag $TERN
    else
        echo "docker image already build"
    fi
}

#################################################################################
computeOutFolder() {
    local workdir="$(readlink -f "$1")"
    echo "${workdir%_ort}_tern"
}

getOutFolder() {
    local out="$(computeOutFolder "$@")"
    mkdir -p "$out"
    echo "$out"
}

runTernOnImage() {
    local imagename="$1"
    shift
    if [[ "$1" ]]; then
        mkdir -p "$1"
        out="$(readlink -f "$1")"
    else
        out="$(pwd)"
    fi

    (set -x;
     docker run \
         --privileged \
         --device /dev/fuse \
         -v /var/run/docker.sock:/var/run/docker.sock \
         -v "${out}":/out \
         -w /out \
         --rm $tag \
         report -f "$format" -i "$imagename" -o "/out/tern_output_${imagename}.$format";
     times
     )
    sudo chown $(id -u $USER):$(id -g $USER) "/out/tern_output_${imagename}.$format"
}

runTernOnDockerfile() {
    local dockerfile="$(readlink -f "$1")"
    local workdir="$(dirname "$dockerfile")"
    local dockerfilename="$(basename "$dockerfile")"
    [[ ! -d "$workdir" ]] && exit 1
    shift
    if [[ "$1" ]]; then
        mkdir -p "$1"
        out="$(readlink -f "$1")"
    else
        out="$(getOutFolder "$workdir")"
    fi

    (set -x;
     docker run \
         --privileged \
         --device /dev/fuse \
         -v /var/run/docker.sock:/var/run/docker.sock \
         -v "$workdir:/workdir" \
         -v "${out}":/out \
         -w /out \
         --rm $tag \
         report -f "$format" -d "/workdir/$dockerfilename" -o "/out/tern_output.$format";
     times
     )
    sudo chown $(id -u $USER):$(id -g $USER) "$out/tern_output.$format"
}

runTernRecursively() {
    local workdir="$(readlink -f "$1")"
    [[ ! -d "$workdir" ]] && exit 1
    shift
    local out="$(getOutFolder "$workdir")"

    cd "$workdir"
    find . -iname 'Dockerfile'  -print0 |
        while IFS= read -r -d '' dockerfile; do
            outFolder="$out/$(echo "$dockerfile" | sed 's#^./##' | sed 's#/#_#g')"
            mkdir -p "$outFolder"
            runTernOnDockerfile $dockerfile "$outFolder"
            sudo chown -R $(id -u $USER):$(id -g $USER) "$outFolder"
        done
}

#################################################################################
# main
#################################################################################
buildImageIfMissing

case $1 in
    "--rm") shift; docker rmi "$tag";;
    "-d") shift; runTernOnDockerfile "$@";;
    "-i") shift; runTernOnImage "$@";;
    "--help") helpMsg ;;
    *) runTernRecursively "$@";;
esac
times
