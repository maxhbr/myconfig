#!/bin/sh
[[ $# -eq 0 ]] && netlogo
[[ $# -eq 1 ]] && netlogo "$(realpath $1)"
[[ $# -eq 2 ]] && {
  DIR=$(realpath $(dirname $1)) 
  mkdir -p "$DIR/experiment_${2}"
  nr=`ls "$DIR/experiment_${2}" | wc -l`
  echo "run Experiment ${2} (${nr})"

  netlogo-headless --model "$(realpath $1)" \
    --experiment "$2" \
    --spreadsheet "${DIR}/experiment_${2}/${nr}_$2"
}
