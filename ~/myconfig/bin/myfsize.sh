#!/bin/bash
printf '\33]50;%s%d\007' "xft:DejaVu Sans Mono:pixelsize=${1}:antialias=true:hinting=true"
printf '\33]711;%s%d\007' "xft:DejaVu Sans Mono:bold:pixelsize=$(($1+1)):antialias=true:hinting=true"
#URxvt*boldFont: xft:DejaVu Sans Mono:bold:pixelsize=13:antialias=true:hinting=true
