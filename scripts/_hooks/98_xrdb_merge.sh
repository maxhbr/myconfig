#!/usr/bin/env bash

if [ -f ~/.Xresources ] && [ ! -z ${DISPLAY+x} ]; then
    xrdb -merge ~/.Xresources
fi
