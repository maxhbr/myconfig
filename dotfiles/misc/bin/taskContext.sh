#!/usr/bin/env bash
set -e
################################################################################
# config
CONTEXTFILE=$HOME/.task/wlanContext
IWIFACE=wlp3s0
MAPPING=("TNG:work"
         "QS3j:private")

################################################################################
# definitions
PREVIOUSCONTEXT=$(cat $CONTEXTFILE)
foundSSIDs=$(nmcli -t -f ssid dev wifi)

function setTaskContext {
    if [ ! "$1" == "$PREVIOUSCONTEXT" ]; then
        task context $1
        echo $1>$CONTEXTFILE
    fi
}

function checkSwitchToContext {
    for i in ${MAPPING[@]}; do
        ssid="${i%:*}"
        context="${i#*:}"
        if [[ $foundSSIDs == *"$ssid"* ]]; then
            setTaskContext $context
            return 0
        fi
    done
    setTaskContext none
}

################################################################################
# run
checkSwitchToContext
task $@
