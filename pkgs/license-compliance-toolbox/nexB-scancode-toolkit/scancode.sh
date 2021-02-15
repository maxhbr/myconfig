#!/usr/bin/env bash
# based on: https://github.com/beevelop/docker-scancode (Apache-2.0)

set -euo pipefail

baseTag=scancode:latest
tag=myscancode:latest

help() {
    cat <<EOF
$ $0 [-fr] [-ex] PATH_TO_WORKDIR [args]
EOF
}

forceRebuild() {
    docker rmi $tag || true
    docker rmi $baseTag || true
}

buildImageIfMissing() {
    if [[ "$(docker images -q $tag 2> /dev/null)" == "" ]]; then
        if [[ "$(docker images -q $baseTag 2> /dev/null)" == "" ]]; then
            SCANCODE=$(mktemp -d)
            trap "rm -rf $SCANCODE" EXIT
            git clone https://github.com/nexB/scancode-toolkit $SCANCODE
            docker build \
                --network=host \
                -t $baseTag $SCANCODE
        else
            echo "docker base image already build, at $(docker inspect -f '{{ .Created }}' $baseTag)"
        fi
        docker build -t $tag -<<EOF
FROM $baseTag
RUN set -x \
 && mkdir -p /tmp/ && chmod 777 /tmp/ \
 && mkdir -p /.cache && chmod 777 /.cache
EOF
    else
        echo "docker image already build, at $(docker inspect -f '{{ .Created }}' $tag)"
    fi
}

getOutFolder() {
    local workdir="$(readlink -f "$1")"
    local out="${workdir%_scancode}_scancode"
    mkdir -p "$out"
    echo "$out"
}

getNumberOfThreads() {
    cores=$(nproc)
    if [[ "$cores" -ge 3 ]]; then
        echo $((cores - 2))
    else
        echo 1
    fi
}

runScancode() {
    local workdir="$(readlink -f "$1")"
    [[ ! -d "$workdir" ]] && exit 1
    bn="$(basename "$workdir")"
    out="$(getOutFolder "$workdir")"
    shift
    (set -x;
     docker run -i \
            --rm \
            -u $(id -u $USER):$(id -g $USER) \
            -v "$workdir":/workdir -v "$out":/out \
            --net=host \
            $tag \
            -n $(getNumberOfThreads) \
            --license --copyright --package --info \
            /workdir \
            --license-text --license-text-diagnostics \
            --json "/out/${bn}.scancode.json" --json-pp "/out/${bn}.scancode.pp.json" --csv "/out/${bn}.scancode.csv" \
            --spdx-rdf "/out/${bn}.scancode.rdf.xml" --spdx-tv "/out/${bn}.scancode.spdx" --html-app "/out/${bn}.scancode.html" \
            --strip-root \
            $@;
     times
    ) 2>&1 | tee -a "$out/${bn}.scancode.log"
    sed -i "s%/out/%./%" "$out/${bn}.scancode.html"
}

runExtractcode() {
    local workdir="$(readlink -f "$1")"
    [[ ! -d "$workdir" ]] && exit 1
    bn="$(basename "$workdir")"
    shift
    (set -x;
     docker run -i \
            --rm \
            -u $(id -u $USER):$(id -g $USER) \
            -v "$workdir":/workdir \
            --net=host \
            $tag \
            /scancode-toolkit/extractcode --verbose \
            /workdir \
            $@;
     times
    )
}

#################################################################################
# main
#################################################################################
if [[ $# -ge 1 && "$1" == "-fr" ]]; then
    shift
    forceRebuild
fi
buildImageIfMissing

if [[  $# -ge 1 ]]; then
    if [[  "$1" == "-ex" ]]; then
        shift
        runExtractcode $@
    else
        runScancode $@
    fi
    times
else
    help
fi
