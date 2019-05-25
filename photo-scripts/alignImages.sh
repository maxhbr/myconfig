#!/usr/bin/env nix-shell
#! nix-shell -i bash -p hugin
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on Macro-scripts package (a complete Open Source workflow for processing macro focus stacking photographs)
# Written by Sergey Mashchenko
# links: https://pulsar124.fandom.com/wiki/Open_Source_workflow_for_macro_focus_stacking
#        https://github.com/pulsar123/Macro-scripts

set -euo pipefail

if [[ "$1" == "--help" ]]; then
    cat<<EOF
  $0 img [img [img ...]]
EOF
    exit 0
fi

if [[ $# -lt 2 ]]; then
    (>&2 echo "less that two arguments, nothing to align")
    exit 1
fi

# if assuming linear color space '-l'
# TODO: look at: https://photo.stackexchange.com/a/83179
OPT="-l"


my_align() {
    local prefix="$1"
    shift
    (set -x
     >&2 align_image_stack "$OPT" --use-given-order -m -a $prefix $@)
    local generatedFiles="$(ls -1q "$prefix"* | wc -l)"
    if [ "$#" -gt "$generatedFiles" ]; then
        (>&2 echo "$(tput bold)failed! expected $# but got only $generatedFiles$(tput sgr0)")
        return 1
    fi
    for name in "$prefix"*; do
        echo "$name"
    done
}

main() {
    files=("$@")
    tmpdir=$(mktemp -d)
    (>&2 echo "tmpdir is: $tmpdir")
    trap "{ rmdir \"$tmpdir\"; }" EXIT

    N=$#
    middle=$(($N / 2 - 1))

    firstFile=$(basename $1)
    firstFile="${firstFile%.*}"
    resultPrefix="${firstFile}_${N}_ALIGN"

    (>&2 echo "#########################################################################"
     >&2 echo "## Start aligning (N=$N, middle=$middle)")

    # Reversing the order of files in the first half:
    for f in "${files[@]:0:$middle+1}"; do
        revfiles=( "$f" "${revfiles[@]}" )
    done
    my_align "$tmpdir/part1_" ${revfiles[@]} | while read name; do
        curN="$(echo $middle "$(echo $(basename $name) | cut -b 7-10)" | awk '{printf "%04d\n", $1-$2}')"
        out="${resultPrefix}${curN}.tif"
        (>&2 echo "$name  -->  $out")
        mv "$name" "$out"
        echo "$out"
    done

    my_align "$tmpdir/part2_" ${files[@]:$middle} | while read name; do
        indexOfGenerated="$(echo $(basename $name) | cut -b 7-10)"
        if [[ "$indexOfGenerated" == "0000" ]]; then
            \rm "$name"
            continue
        fi
        curN="$(echo $middle "$indexOfGenerated" | awk '{printf "%04d\n", $1+$2}')"
        out="${resultPrefix}${curN}.tif"
        (>&2 echo "$name  -->  $out")
        mv "$name" "$out"
        echo "$out"
    done

    numberOfGeneratedFiles=$(find . -maxdepth 1 -name "$resultPrefix"'*' -print | wc -l)

    (>&2 echo "## done (result number=$numberOfGeneratedFiles)"
     >&2 ls "$tmpdir"
     >&2 echo "#########################################################################")

    if [ "$N" -ne "$numberOfGeneratedFiles" ]; then
        (>&2 echo "number of files do not match ...")
        exit 1
    fi
}

main $@
