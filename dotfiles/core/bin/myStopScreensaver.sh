#!/usr/bin/env nix-shell
#! nix-shell -i bash -p xdotool

myStopScreensaver(){
    printf "run: "
    while true; do
        printf "."
        sleep $((60 * 4))
        xdotool key shift
    done
}

myStopScreensaver
