#!/bin/sh
# |------+-------+-------|
# | eDP1 | DP2-8 | DP2-1 |
# |------+-------+-------|

xrandr --output VIRTUAL1 --off --output DP3 --off --output DP1 --off --output HDMI3 --off --output HDMI2 --off --output HDMI1 --off --output DP2 --off
xrandr --output eDP1            --mode 2560x1440 --pos 0x0    --rotate normal --scale 1x1
xrandr --output DP2-8 --primary --mode 2560x1440 --pos 2560x0 --rotate normal --scale 1x1
xrandr --output DP2-1           --mode 2560x1440 --pos 5120x0 --rotate normal --scale 1x1
