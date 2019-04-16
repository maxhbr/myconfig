#!/usr/bin/env nix-shell
#! nix-shell -i bash -p hugin

# based on Macro-scripts package (a complete Open Source workflow for processing macro focus stacking photographs)
# Written by Sergey Mashchenko
# links: https://pulsar124.fandom.com/wiki/Open_Source_workflow_for_macro_focus_stacking
#        https://github.com/pulsar123/Macro-scripts

set -e

if [[ $# -lt 2 ]]; then
    echo "less that two arguments, nothing to align"
    exit 1
fi

# if assuming linear color space '-l'
OPT="-l"

firstFile=$(basename $1)
firstFile="${firstFile%.*}"
tmpdir=$(mktemp -d)
trap "{ rm -f $tmpdir; }" EXIT
echo "tmpdir is: $tmpdir"

my_align() {
    prefix="$tmpdir/${1}_"
    shift
    align_image_stack "$OPT" --use-given-order -m -a $prefix $@
}

files=("$@")
N=$#
middle=$(($N / 2 - 1))
echo N=$N, middle=$middle

# Reversing the order of files in the first half:
for f in "${files[@]:0:$middle+1}"; do
    revfiles=( "$f" "${revfiles[@]}" )
done

my_align part1 ${revfiles[@]} 2>&1 | sed -e 's/^/part1:  /' &
my_align part2 ${files[@]:$middle} 2>&1 | sed -e 's/^/part2:  /' &
wait
\rm "$tmpdir/part2_0000.tif"

for name in $tmpdir/part1_*; do
    N1=`echo $(basename $name) |cut -b 7-10`
    N2=`echo $middle $N1 | awk '{printf "%04d\n", $1-$2}'`
    mv $name "${firstFile}_ALIGN${N2}.tif"
done

for name in $tmpdir/part2_*; do
    N1=`echo $(basename $name) |cut -b 7-10`
    N2=`echo $middle $N1 | awk '{printf "%04d\n", $1+$2}'`
    mv $name "${firstFile}_ALIGN${N2}.tif"
done
