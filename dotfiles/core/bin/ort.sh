#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

#
# A simple script which generates the ort docker image on demand and wraps calls
# to ort into a docker layer
#
# Based on Code from https://github.com/heremaps/oss-review-toolkit (Licensed under Apache-2.0)
#

set -e

#################################################################################
# function to build ort docker image
#################################################################################
buildImageIfMissing() {
    if [[ "$(docker images -q ort:latest 2> /dev/null)" == "" ]]; then
        ORT=$(mktemp -d)
        trap 'rm -rf $ORT' EXIT
        git clone https://github.com/heremaps/oss-review-toolkit/ $ORT

        docker build -t ort-build:latest - < $ORT/docker/build/Dockerfile

        docker run -i \
               -v "$ORT":/workdir \
               -u $(id -u $USER):$(id -g $USER) \
               -w /workdir \
               --net=host \
               ort-build \
               ./gradlew --no-daemon --stacktrace :cli:installDist :cli:distTar

        ORT_VERSION=$(cat $ORT/model/src/main/resources/VERSION)
        docker build \
               --build-arg ORT_VERSION=$ORT_VERSION \
               -t ort:latest \
               -f $ORT/docker/run/Dockerfile $ORT/cli/build/distributions
        docker tag ort:latest ort:"$ORT_VERSION"
    else
        echo "docker image already build"
    fi
}

#################################################################################
# function to run dockerized ort commands
#################################################################################
runOrt() {
    workdir="$1"
    [[ ! -d "$workdir" ]] && exit 1
    shift
    mkdir -p "$HOME/.ort/cache"
    docker run -i \
           --rm \
           -v /etc/group:/etc/group:ro -v /etc/passwd:/etc/passwd:ro -u $(id -u $USER):$(id -g $USER) \
           -v "$HOME/.ort/":"$HOME/.ort" -v "$HOME/.ort/cache":"$HOME/.cache" \
           -v "$workdir":/workdir \
           -w /workdir \
           --net=host \
           ort --info \
           $@
}

#################################################################################
# actual calls to ort features
#################################################################################
analyzeFolder() {
    local folderToScan="$1"
    [[ ! -d "$folderToScan" ]] && exit 1

    runOrt "$folderToScan" \
           analyze -f JSON -i /workdir -o /workdir/_ort_analyzer
}

scanAnalyzeResult() {
    local analyzeResult="$1"
    [[ ! -f "$analyzeResult" ]] && exit 1

    local analyzeResultFolder="$(readlink -f "$(dirname $analyzeResult)")"
    local analyzeResultFile="$(basename $1)"
    runOrt "$analyzeResultFolder" \
           scan --ort-file "$analyzeResultFile" -o /workdir/_ort_scan
}

reportScanResult() {
    local scanResult="$1"
    [[ ! -f "$scanResult" ]] && exit 1

    local scanResultFolder="$(readlink -f "$(dirname $scanResult)")"
    local scanResultFile="$(basename $1)"
    runOrt "$scanResultFolder" \
           report -f StaticHtml,Notice,Excel,WebApp --ort-file "$scanResultFile" -o /workdir/_ort_report
}

#################################################################################
# main
#################################################################################
buildImageIfMissing
case $1 in
    "--analyze") shift; analyzeFolder "$@" ;;
    "--scan") shift; scanAnalyzeResult "$@" ;;
    "--report") shift; reportScanResult "$@" ;;
    "--ortHelp") shift; ortHelp ;;
    "--help") cat<<EOF
usage:
  $0 --analyze <folder>
  $0 --scan <yaml>
  $0 --report <yaml>
  $0 [args]
  $0 --help

remove the ort:latest image to enforce rebuild of the docker image

EOF
        ;;
    *) runOrt "$(pwd)" "$@" ;;
esac
times
