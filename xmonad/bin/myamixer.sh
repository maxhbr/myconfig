#!/usr/bin/env bash

out=$(amixer sset Master $@ 2>/dev/null)
RESULT=$?
if [ $RESULT -ne 0 ]; then
    out=$(amixer sset PCM $@ 2>/dev/null)
fi

if [[ $out == *"[off]"* ]]; then
    echo "muted"
else
    awk -F"[][]" '/dB/ { print $2 }' <(echo $out)
fi
