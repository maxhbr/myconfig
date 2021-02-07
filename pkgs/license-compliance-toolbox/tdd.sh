#!/usr/bin/env bash
set -euo pipefail

DIR="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"

getOutFolder() {
    local input="$1"
    local workdir="$(readlink -f "$input")"
    local out="${workdir%_ort}_tdd"
    mkdir -p "$out"
    echo "$out"
}

getSourceDir() {
    local input="$1"
    local out="$(getOutFolder "$input")"
    local source="$out/$(basename "$input")"
    if [[ -d "$input" ]]; then
        cp -r "$input" "$source"
        (>&2 "$DIR/scancode.sh" -ex "$source")
    fi
    echo "$source"
}

listBinaryFiles() {
    find "${1:-.}" \
        -type f \
        -not -empty \
        -not -path '*/\.git/*' \
        -not -path '*/\.svn/*' |
        perl -lne 'print if -B'
}

main() {
    local input="$1"
    local out="$(getOutFolder "$input")"
    local sourceDir="$(getSourceDir "$input")"

    listBinaryFiles "$sourceDir" > "$out/binaryFiles.csv"
    "$DIR/scancode.sh" "$sourceDir" || true
    "ort.sh" all "$sourceDir" || true
}

main "$@"
