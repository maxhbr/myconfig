#!/usr/bin/env bash
# based on: https://github.com/beevelop/docker-scancode (Apache-2.0)

set -e

tag=scancode:latest

help() {
    cat <<EOF
$ $0 [-fr] [-ex] PATH_TO_WORKDIR [args]
EOF
}

forceRebuild() {
    docker rmi $tag || true
}

buildImageIfMissing() {
    if [[ "$(docker images -q $tag 2> /dev/null)" == "" ]]; then
        docker build -t $tag - <<EOF
FROM python:3.6

RUN apt-get update && apt-get install bzip2 xz-utils zlib1g libxml2-dev libxslt1-dev

ADD https://github.com/nexB/scancode-toolkit/archive/master.tar.gz .

RUN set -x \
 && mkdir workdir \
 && mkdir /.cache && chmod 777 /.cache \
 && mkdir scancode-toolkit \
 && tar xzvf master.tar.gz -C scancode-toolkit --strip-components=1 \
 && ./scancode-toolkit/scancode --help

WORKDIR workdir

RUN pwd
CMD /scancode-toolkit/scancode --help
EOF
    else
        echo "docker image already build"
    fi
}

getOutFolder() {
    local workdir="$(readlink -f "$1")"
    local out="${workdir%_scancode}_scancode"
    mkdir -p "$out"
    echo "$out"
}

runScancode() {
    local workdir="$(readlink -f "$1")"
    [[ ! -d "$workdir" ]] && exit 1
    bn="$(basename "$workdir")"
    shift
    (set -x;
     docker run -i \
            --rm \
            -u $(id -u $USER):$(id -g $USER) \
            -v "$workdir":/workdir -v "$(getOutFolder "$workdir")":/out \
            -w /workdir \
            --net=host \
            $tag \
            /scancode-toolkit/scancode \
            --license --copyright --package --info \
            /workdir \
            --license-text --license-text-diagnostics \
            --json "/out/${bn}.scancode.json" --json-pp "/out/${bn}.scancode.pp.json" --csv "/out/${bn}.scancode.csv" \
            --spdx-rdf "/out/${bn}.scancode.rdf.xml" --spdx-tv "/out/${bn}.scancode.pp.spdx" --html-app "/out/${bn}.scancode.html" \
            --strip-root \
            $@;
     times
    )
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
            -w /workdir \
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
if [[ "$1" == "-fr" ]]; then
    shift
    forceRebuild
fi
buildImageIfMissing

if [[ "$1" == "-ex" ]]; then
    shift
    runExtractcode $@
else
    runScancode $@
fi

