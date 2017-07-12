#!/bin/sh
# |------------+-----|
# | eDP1/HDMI1 | DP2 |
# |------------+-----|

xrandr --output VIRTUAL1 --off --output DP3 --off --output DP1 --off --output HDMI3 --off --output HDMI2 --off
xrandr --output eDP1            --mode 2560x1440                        --pos 0x0      --rotate normal
xrandr --output DP2   --primary --mode 2560x1440                        --pos 2560x0   --rotate normal
xrandr --output HDMI1           --mode 3840x2160 --scale-from 2560x1440 --same-as eDP1 --rotate normal
