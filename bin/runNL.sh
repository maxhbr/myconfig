#!/bin/sh
#
# usage:
#
# start NetLogo:
#   $ runNL.sh
# start NetLogo with model:
#   $ runNL.sh model
# run NetLogo headless and run experiments on model:
#   $ runNL.sh model exp1 [exp2 [...]]

NETLOGODIR="/opt/netlogo/"

###############################################################################
####  generate command  #######################################################
###############################################################################
comm="cd ${NETLOGODIR}; java"
if [[ $# -lt 2 ]]; then
  # -Djava.library.path=./lib     ensure JOGL can find native libraries
  # -Djava.ext.dirs=              ignore any existing JOGL installation
  # -XX:MaxPermSize=128m          avoid OutOfMemory errors for large models
  comm="${comm} -Djava.library.path=./lib -Djava.ext.dir= -XX:MaxPermSize=128m"
fi
# -Xmx1024m                       use up to 1GB RAM (edit to increase)
# -Dfile.encoding=UTF-8           ensure Unicode characters in model files are compatible cross-platform
comm="${comm} -Xmx1024m -Dfile.encoding=UTF-8"
if [[ $# -gt 0 ]] && [[ $1 == *3d ]]; then
  # -Dorg.nlogo.is3d=true         run 3D NetLogo
  comm="${comm} -Dorg.nlogo.is3d=true"
fi
if [[ $# -lt 2 ]]; then
  # -jar NetLogo.jar              specify main jar
  comm="${comm} -jar NetLogo.jar"
else
  # -classpath NetLogo.jar        specify main jar
  # org.nlogo.headless.Main       specify we want headless, not GUI
  comm="${comm} -classpath NetLogo.jar org.nlogo.headless.Main"
fi

###############################################################################
####  run NetLogo  ############################################################
###############################################################################
if [[ $# -lt 2 ]]; then
  if [[ $# -eq 1 ]]; then
    eval "${comm} \"$(realpath $1)\""
  else
    eval $comm
  fi
else
  comm="${comm} --model \"$(realpath $1)\""
  DIR=$(realpath $(dirname $1)) 
  shift


  for EXP in $*; do
    mkdir -p "$DIR/exp_${EXP}"
    nr=`ls "$DIR/exp_${EXP}" | wc -l`
    echo "run Experiment ${EXP} (${nr})"

    eval "$comm --experiment \"$EXP\" --spreadsheet \"${DIR}/exp_${EXP}/${nr}_${EXP}.csv\""

    # other arguments for netlogo-headless:
    #--min-pxcor <number>: override world size setting in model file
    #--max-pxcor <number>: override world size setting in model file
    #--min-pycor <number>: override world size setting in model file
    #--max-pycor <number>: override world size setting in model file
  done
fi
