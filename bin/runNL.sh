#!/bin/sh
#
# usage:
#   $ runNL.sh [model [exp1 [exp2 [...]]]]
[[ $# -eq 0 ]] && netlogo
[[ $# -eq 1 ]] && netlogo "$(realpath $1)"
[[ $# -eq 2 ]] && {
  DIR=$(realpath $(dirname $1)) 
  MODEL=$(realpath $1)
  shift

  for EXP in $*; do
    mkdir -p "$DIR/experiment_${EXP}"
    nr=`ls "$DIR/experiment_${EXP}" | wc -l`
    echo "run Experiment ${EXP} (${nr})"

    netlogo-headless --model "$MODEL" \
      --experiment "$EXP" \
      --spreadsheet "${DIR}/experiment_${EXP}/${nr}_$EXP"
  done
}
