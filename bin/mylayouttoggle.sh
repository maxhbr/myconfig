#!/bin/sh
# toggle between default layout and neo

KEYBOARD=`cat /tmp/mykeyboard`
[[ "$KEYBOARD" = "neo" ]]
RETURN=$?
if [[ $RETURN -eq 0 ]]; then
  setxkbmap de nodeadkeys -option
  setxkbmap -option ctrl:nocaps
  feh --bg-center "/home/hubi/.background/background1.png"
  echo "nodeadkeys"
  echo "nodeadkeys" > /tmp/mykeyboard
elif [[ $RETURN -eq 1 ]]; then
  setxkbmap de neo -option
  feh --bg-center "/home/hubi/.xmonad/neo_Ebenen_1_2_3_4.png"
  echo "neo"
  echo "neo" > /tmp/mykeyboard
fi
