#!/usr/bin/env nix-shell
#! nix-shell -i bash -p python36 python36Packages.pip
# based on: https://github.com/beevelop/docker-scancode (Apache-2.0)

set -e

pipEnvDir="$HOME/.license-compliance-toolbox-pip-env"

help() {
    cat <<EOF
$ $0 [-ex] PATH_TO_WORKDIR [args]
EOF
}

installScancodeIfMissing() {
    if [[ ! -d "$pipEnvDir" ]]; then
        python3 -m venv $pipEnvDir
    fi
    pushd "$pipEnvDir"
    source bin/activate
    popd
    if ! command -v "scancode" &> /dev/null; then
        pip install wheel
        pip install scancode-toolkit
    fi
}

#################################################################################

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

    (set -x
     scancode \
         --license-text --license-text-diagnostics \
         --json "${out}/${bn}.scancode.json" --json-pp "${out}/${bn}.scancode.pp.json" --csv "${out}/${bn}.scancode.csv" \
         --spdx-rdf "${out}/${bn}.scancode.rdf.xml" --spdx-tv "${out}/${bn}.scancode.spdx" --html-app "${out}/${bn}.scancode.html" \
         --strip-root \
         "$@" | tee -a "${out}/${bn}.scancode.log")
    # sed -i "s%${out}/%./%" "$out/${bn}.scancode.html"
}

runExtractcode() {
    local workdir="$(readlink -f "$1")"
    [[ ! -d "$workdir" ]] && exit 1
    bn="$(basename "$workdir")"
    shift

    (set -x
     extractcode --verbose "$workdir" "$@")
}

#################################################################################
# main
#################################################################################
installScancodeIfMissing

if [[ "$1" == "-ex" ]]; then
    shift
    runExtractcode "$@"
else
    runScancode "$@"
fi

