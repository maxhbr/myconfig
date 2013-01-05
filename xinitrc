#!/bin/sh
# ~/.xinitrc
#
# written by maximilian-huber.de
#
# Last modified: Sa Jan 05, 2013  10:05

#toggle to neo via scrol
setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scrol
#setxkbmap -layout de -variant nodeadkeys
# set caps lock as ctrl
setxkbmap -option ctrl:nocaps

xsetroot -cursor_name left_ptr

## Disable beeps
#xset -b # doesnt work

xset dpms 300 600 900 & # screen saver replacement
xbacklight -set 90 &

exec urxvtd & # -q -o -f &

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

if [ -f ~/bin/mydock.sh ]; then
  ~/bin/mydock.sh fast &
else
  [[ -f ~/.icc/X220.icc ]] && dispwin ~/.icc/X220.icc &
  [[ -f ~/.fehbg ]] && sh ~/.fehbg &
  [[ -f ~/bin/myvolume.sh ]] && ~/bin/myvolume.sh m &
fi

exec ck-launch-session dbus-launch xmonad
#exec ck-launch-session dbus-launch --sh-syntax --exit-with-session i3 -V -d all > /tmp/i3log 2>&1
