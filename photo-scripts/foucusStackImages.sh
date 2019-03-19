#!/usr/bin/env nix-shell
#! nix-shell -i bash -p enblend-enfuse

# based on Macro-scripts package (a complete Open Source workflow for processing macro focus stacking photographs)
# Written by Sergey Mashchenko
# links: https://pulsar124.fandom.com/wiki/Open_Source_workflow_for_macro_focus_stacking
#        https://github.com/pulsar123/Macro-scripts

set -e

if [[ $# -ne 1 ]]; then
    echo "requires exactly one argument: the prefix"
    exit 1
fi
prefix="$1"

tmpdir=$(mktemp -d)
echo "tmpdir is: $tmpdir"

ls -1 "$prefix"*.tif 2>/dev/null > "$tmpdir/List"
firstFile=$(basename $(head -1 "$tmpdir/List"))
firstFile="${firstFile%.*}"
firstFile="${firstFile%_*}"

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
    echo "Slab=$i, range $k1 - $k2, $(($k2-$k1+1)) frames"

    enfuse --exposure-weight=0 --saturation-weight=0 --contrast-weight=1 --hard-mask --gray-projector=l-star --output=$(printf "${firstFile}_SLAB%04d" $i).tif $(cat "$tmpdir/List" | sed -n $(($k1+1)),$(($k2+1))p)
}

align_with_slabs() {
    local over=0.3 # Overlap on each side (fraction), for each slab
    local Nover_min=2 # Overlap should be at least that many images

    local N=$(cat "$tmpdir/List"|wc -l)
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
        create_slab $i $j $N1 "$Size" "$Nover"
        if [[ $j -lt $Slast ]]; then
            j=$(($j+1))
        fi
    done

    echo "Second stage - merging all slabs into final stacked photo"
    enfuse --exposure-weight=0 --saturation-weight=0 --contrast-weight=1 --hard-mask --gray-projector=l-star --output="${firstFile}_STACKED.tif" "${firstFile}_SLAB"*.tif
}

align_directly() {
    enfuse --exposure-weight=0 --saturation-weight=0 --contrast-weight=1 --hard-mask                           --output="${firstFile}_STACKED_alt1.tif" "$prefix"*.tif | sed -e 's/^/alt1:  /' &
    enfuse --exposure-weight=0 --saturation-weight=0 --contrast-weight=1 --hard-mask --gray-projector=l-star   --output="${firstFile}_STACKED_alt2.tif" "$prefix"*.tif | sed -e 's/^/alt2:  /' &
    enfuse --exposure-weight=0 --saturation-weight=0 --contrast-weight=1 --hard-mask --contrast-window-size=5  --output="${firstFile}_STACKED_alt3.tif" "$prefix"*.tif | sed -e 's/^/alt3:  /' &
    enfuse --exposure-weight=0 --saturation-weight=0 --contrast-weight=1 --hard-mask --contrast-edge-scale=0.3 --output="${firstFile}_STACKED_alt4.tif" "$prefix"*.tif | sed -e 's/^/alt4:  /' &
    wait
}

align_with_slabs
align_directly
