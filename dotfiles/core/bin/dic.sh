#!/usr/bin/env bash

dic(){
    if [[ "$#" -eq 0 ]]; then
        tput bold
        tput setaf 6
        while read line; do
            if [[ "$line" == "" ]]; then
                tput sgr0
                return 0
            fi
            tput sgr0
            w3m -dump "http://pocket.dict.cc?s=\"$line\"" \
                | sed -r -e '/^([ ]{5,}.*)$/d' -e '1,2d' -e '/^$/d' -e '/^\[/d' \
                | head -n $(( $(tput lines) - 3 ))
            echo "=================================================================="
            tput bold
            tput setaf 6
        done
        tput sgr0
    else
        w3m -dump "http://pocket.dict.cc?s=\"$*\"" \
            | sed -r -e '/^([ ]{5,}.*)$/d' -e '1,2d' -e '/^$/d' -e '/^\[/d' \
            | less
    fi
}

################################################################################
# run
if [ $# = 0 ]; then
    rlwrap $0 --rlwrap $@
fi
if [ "$1" = "--rlwrap" ]; then
    shift
fi

dic $@
