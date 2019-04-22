#!/usr/bin/env nix-shell
#! nix-shell -i bash -p enblend-enfuse
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on Macro-scripts package (a complete Open Source workflow for processing macro focus stacking photographs)
# Written by Sergey Mashchenko
# links: https://pulsar124.fandom.com/wiki/Open_Source_workflow_for_macro_focus_stacking
#        https://github.com/pulsar123/Macro-scripts

set -e

################################################################################
##  parse arguments parse arguments  ###########################################
################################################################################

ENFUSE_PROJECTOR_ARG="--gray-projector=l-star"
case $1 in
    --help)
        cat<<EOF
  $0 [--mode1] [--reverse] img [img [img ...]]
  $0 --mode2   [--reverse] img [img [img ...]]
  $0 --mode3   [--reverse] img [img [img ...]]
  $0 --mode4   [--reverse] img [img [img ...]]
EOF
        exit 0
        ;;
    --mode1)
        shift
        ;;
    --mode2)
        shift
        ENFUSE_PROJECTOR_ARG=""
        ;;
    --mode3)
        shift
        ENFUSE_PROJECTOR_ARG="--contrast-window-size=5"
        ;;
    --mode4)
        shift
        ENFUSE_PROJECTOR_ARG="--contrast-edge-scale=0.3"
        ;;
esac
ENFUSE_ARGS="--exposure-weight=0 --saturation-weight=0 --contrast-weight=1 --hard-mask $ENFUSE_PROJECTOR_ARG"

if [[ $1 == "--reverse" ]]; then
    shift
    files=( "$@" )

    min=0
    max=$(( ${#files[@]} -1 ))

    while [[ min -lt max ]]; do
        x="${files[$min]}"
        files[$min]="${files[$max]}"
        files[$max]="$x"
        (( min++, max-- ))
    done
else
    files=( "$@" )
fi

if [[ $# -lt 2 ]]; then
    echo "requires exactly more than two arguments"
    exit 1
fi
trap times EXIT

################################################################################
##  functions  #################################################################
################################################################################

find_next_filename() {
    local name=$1
    local extension=$2
    if [[ ! -e "$name$extension" ]]; then
        echo "$name$extension"
        return 0
    fi
    local count=1
    while [[ -e "${name}_${count}${extension}" ]]; do
        count=$((count+1))
    done
    echo "${name}_${count}${extension}"
}

find_out_filename() {
    local firstFile=$(basename "${files[0]}")
    firstFile="${firstFile%.*}"
    firstFile="${firstFile%_*}"

    find_next_filename "${firstFile}_STACKED" ".tif"
}

create_slab() {
    local i=$1
    local j=$2
    local N1=$3
    local Size=$4
    local Nover=$5

    local k1=$(($i*$Size+$j)) # First file to include in the slab
    local k2=$((($i+1)*$Size+$j+$Nover-1)) # Last file to include in the slab
    if [[ $k2 -gt $N1 ]]; then
        k2=$N1
    fi
    echo "Slab=$i, range $k1 - $k2 of $N1, $(($k2-$k1+1)) frames"

    mkdir -p "${outFileWithoutExt}/"
    enfuse $ENFUSE_ARGS \
           --output=$(printf "${outFileWithoutExt}/${outFileWithoutExt}_SLAB%04d" $i).tif \
           ${files[@]:$(($k1+1)):$(($k2-$k1+1))}
}

align_with_slabs() {
    local over=0.3 # Overlap on each side (fraction), for each slab
    local Nover_min=2 # Overlap should be at least that many images

    local N=${#files[@]}
    local N1=$(($N-1))

    local Size=$(echo $N| awk '{print int(sqrt($1))}') # Size of each slab (not including the overlap
    local Nover=$(echo $Size $over $Nover_min|awk '{A=int($1*$2); if (A<$3) print $3; else print A; fi}') # N overlap is at least $Nover_min

    local Slast=$(($N%$Size-$Nover)) # Last (usually smaller) slab size
    local Nslabs=$(($N/$Size)) # Number of complete slabs
    if [[ $Slast -lt 0 ]]; then
        Nslabs=$(($Nslabs-1))
        Slast=$(($N-$Nslabs*$Size-$Nover))
    fi

    echo "Size=$Size, Nslabs=$Nslabs, Slast=$Slast, Nover=$Nover"

    echo "First stage (creating multiple slabs)"
    local j=0
    for ((i=0; i<$Nslabs; i++)); do
        create_slab $i $j $N1 $Size $Nover
        if [[ $j -lt $Slast ]]; then
            j=$(($j+1))
        fi
    done

    echo "Second stage - merging all slabs into final stacked photo"
    enfuse $ENFUSE_ARGS \
           --output="${outFile}" \
           "${outFileWithoutExt}/${outFileWithoutExt}_SLAB"*.tif
}

################################################################################
##  run  #######################################################################
################################################################################

outFile=$(find_out_filename)
touch $outFile # claim the out file and overwrite it if done
outFileWithoutExt="${outFile%.*}"

if [[ "${#files[@]}" -gt 100 ]]; then
    align_with_slabs
else
    enfuse $ENFUSE_ARGS \
           --output="${outFile}" \
           ${files[@]}
fi
