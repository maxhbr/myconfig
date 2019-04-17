#!/usr/bin/env nix-shell
#! nix-shell -i bash -p dcraw

# based on Macro-scripts package (a complete Open Source workflow for processing macro focus stacking photographs)
# Written by Sergey Mashchenko
# links: https://pulsar124.fandom.com/wiki/Open_Source_workflow_for_macro_focus_stacking
#        https://github.com/pulsar123/Macro-scripts

set -e

if [[ "$1" == "--help" ]]; then
    cat<<EOF
  $0 [-l] raw [raw [raw ...]]
EOF
    exit 0
fi

if [[ "$1" == "-l" ]]; then
     echo "Using linear color space"
    opts="-4"
else
    echo "Using sRGB color space"
    opts="-6 -W"
fi

# RAW images conversion to 48-bit TIFFs:
dcraw -v $opts -T $@

