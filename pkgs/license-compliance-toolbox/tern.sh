#!/usr/bin/env nix-shell
#! nix-shell -i bash -p python3Packages.pip
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

pipEnvDir="$HOME/.license-compliance-toolbox-pip-env"
# export format=json
export format=yaml

helpMsg() {
    cat<<EOF
usage:
  $0 [-d] path/to/Dockerfile [path/To/Out/Folder]
  $0 -i image:tag [path/To/Out/Folder]
  $0 path/To/Project/Folder
  $0 --help
EOF
}

activateTern() {
    pushd "$pipEnvDir"
    source bin/activate
    popd
}

installTernIfMissing() {
    if [[ ! -d "$pipEnvDir" ]]; then
        python3 -m venv $pipEnvDir
        activateTern
        pip install tern scancode-toolkit
    else
        activateTern
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
        outFolder="$(readlink -f "$1")"
        cd $outFolder;
    else
        outFolder="$(pwd)"
    fi
    (set -x;
     tern report \
         -f "$format" \
         -x scancode \
         -i "$imagename" \
         -o "$outFolder/tern_output.$format")
}

runTernOnDockerfile() {
    local dockerfile="$(readlink -f "$1")"
    [[ ! -f "$dockerfile" ]] && exit 1
    shift
    local outFolder
    if [[ "$1" ]]; then
        outFolder="$(readlink -f "$1")"
        cd $outFolder;
    else
        outFolder="$(pwd)"
    fi

    (set -x;
     tern report \
         -f "$format" \
         -x scancode \
         -d "$dockerfile" \
         -o "$outFolder/tern_output.$format")
}

runTernRecursively() {
    local workdir="$(readlink -f "$1")"
    [[ ! -d "$workdir" ]] && exit 1
    shift
    local out="$(getOutFolder "$workdir")"

    (cd "$workdir"
     find . -iname 'Dockerfile'  -print0 |
         while IFS= read -r -d '' dockerfile; do
             outFolder="$out/$(echo "$dockerfile" | sed 's#^./##' | sed 's#/#_#g')"
             mkdir -p "$outFolder"

             runTernOnDockerfile "$dockerfile" "$outFolder"
         done
    )
}

#################################################################################
# main
#################################################################################

installTernIfMissing

case $1 in
    "-d") shift; runTernOnDockerfile "$@";;
    "-i") shift; runTernOnImage "$@";;
    "--help") helpMsg ;;
    *) shift;
       if [[ -f "$1" ]]; then
           runTernOnDockerfile "$@"
       elif [[ -d "$1" ]]; then
           runTernRecursively "$1"
       else
           tern "$@"
       fi
       ;;
esac
times
