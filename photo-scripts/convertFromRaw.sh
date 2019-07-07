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
  $0 [-l|-default] [-q0|-q1|-q2|-q3] raw [raw [raw ...]]
EOF
    exit 0
fi

# choose color space
# sRGB is the default
if [[ "$1" == "-l" ]]; then
    shift
    echo "Using linear color space"
    opts="-4 -W"
    # Linear 16-bit, same as -6 -W -g 1 1
elif [[ "$1" == "-default" ]]; then
    echo "Using default color space"
    opts="-W"
else
    echo "Using sRGB color space"
    opts="-6 -W -g 2.4 12.92"
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

