#!/usr/bin/env nix-shell
#! nix-shell -i bash -p dcraw
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on Macro-scripts package (a complete Open Source workflow for processing macro focus stacking photographs)
# Written by Sergey Mashchenko
# links: https://pulsar124.fandom.com/wiki/Open_Source_workflow_for_macro_focus_stacking
#        https://github.com/pulsar123/Macro-scripts
# manpage: https://www.cybercom.net/~dcoffin/dcraw/dcraw.1.html

# TODO:
#  why produce the generated files the following warning:
# Warning: TIFFDecoder: no TIFFTAG_SAMPLEFORMAT or TIFFTAG_DATATYPE, guessing pixeltype 'UINT16'.

set -e

if [[ "$1" == "--help" ]]; then
    cat<<EOF
  $0 [-wb wb-raw-img|--skip1] [-l|-default] [-q0|-q1|-q2|-q3] raw [raw [raw ...]]
EOF
    exit 0
fi

calculateWB() {
    local wbImg="$1"

    # Fraction of the image to use for white balance calculations (in both dimensions; centered)
    local frac=0.3

    read Nx Ny <<< $(LANG=en_US.UTF-8 dcraw -i -v "$wbImg" |
                         grep "^Image size" |
                         cut -d: -f2 |
                         sed 's/x//g')
    (>&2 echo "Image dimensions: $Nx x $Ny")

    # Center coordinates:
    local x=$(( $Nx / 2 ))
    local y=$(( $Ny / 2 ))
    # Crop dimensions:
    local w=$(echo $x $frac | awk '{print int($1*$2)}')
    local h=$(echo $y $frac | awk '{print int($1*$2)}')
    (>&2 echo "Crop area center and dimensions: x,y=$x,$y; w,h=$w x $h")

    wb=$(LANG=en_US.UTF-8 dcraw -v -A $x $y $w $h -c "$wbImg" 2>&1 >/dev/null |
             grep "^multipliers" |
             cut -d" " -f2-)
    (>&2 echo "Calculated whitebalance is: $wb")
    echo "$wb"
}

if [[ "$1" == "-wb" ]]; then
    shift
    opts="-r $(calculateWB "$1")"
    shift
elif [[ $1 == "--skip1" ]]; then
    shift
    shift
else
    # use the whitebalance from the raw image
    opts="-W"
fi

# choose color space
# sRGB is the default
if [[ "$1" == "-l" ]]; then
    shift
    echo "Using linear color space"
    opts="-4 $opts"
    # Linear 16-bit, same as -6 -W -g 1 1
elif [[ "$1" == "-default" ]]; then
    echo "Using default color space"
    opts="$opts"
else
    echo "Using sRGB color space"
    opts="-6 $opts -g 2.4 12.92"
fi

# choose quality (default is -q3)
if [[ "$1" == "-q0" ]]; then
    shift
    opts="$opts -q 0"
elif [[ "$1" == "-q1" ]]; then
    shift
    opts="$opts -q 1"
elif [[ "$1" == "-q2" ]]; then
    shift
    opts="$opts -q 2"
elif [[ "$1" == "-q3" ]]; then
    shift
    opts="$opts -q 3"
fi

# RAW images conversion to 48-bit TIFFs:
dcraw -v $opts -T $@

