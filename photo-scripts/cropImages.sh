#!/usr/bin/env nix-shell
#! nix-shell -i bash -p imagemagick

set -e

help() {
    cat<<EOF
  $0 <top-right-bottom-left>       img [img [img ...]]
  $0 <top-bottom> <right-left>     img [img [img ...]]
  $0 <top> <right> <bottom> <left> img [img [img ...]]
EOF
}

top=0
right=0
bottom=0
left=0
re='^[0-9]+$'
if [[ $1 =~ $re ]] ; then
    left=$1
    top=$1
    right=$1
    bottom=$1
    shift
    if [[ $1 =~ $re ]] ; then
        left=$1
        right=$1
        shift
        if [[ $1 =~ $re && $2 =~ $re ]] ; then
            bottom=$1
            left=$2
            shift
            shift
        fi
    fi
else
    help
    exit 1
fi

cropImage() {
    input=$1
    output="${input%.*}_CROPPED.${input##*.}"
    echo "$input --> $output"
    convert "$input" -crop "+${left}+${top}" -crop "-${right}-${bottom}" "$outputgg"
}

for img in "$@"; do
    cropImage "$img"
done
