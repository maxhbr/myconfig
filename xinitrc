#!/bin/sh
# ~/.xinitrc
#
# written by maximilian-huber.de
#
# Last modified: Do Jan 03, 2013  04:04

# Get back ctrl+alt+backspace in new versions of xorg
# setxkbmap -option terminate:ctrl_alt_bksp

xsetroot -cursor_name left_ptr

#toggle to neo via scrol
#setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scrol

#setxkbmap -layout de -variant nodeadkeys
# set caps lock as ctrl
setxkbmap -option ctrl:nocaps

#transparency
xcompmgr -n &
## Disable beeps
#xset -b # doesnt work
xset dpms 300 600 900 & # screen saver replacement
xbacklight -set 90 &

## OSD
#dunst &

exec urxvtd & # -q -o -f &

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

if [ -f ~/bin/mydock.sh ]; then
  ~/bin/mydock.sh fast &
else
  if [ -f ~/.icc/X220.icc ]; then
    dispwin ~/.icc/X220.icc
  fi
  if [ -f ~/.fehbg ]; then
    #exec sh ~/.fehbg &
    sh ~/.fehbg &
  fi
  if [ -f ~/bin/myvolume.sh ]; then
    ~/bin/myvolume.sh m &
  fi
fi

exec ck-launch-session dbus-launch xmonad
#exec ck-launch-session dbus-launch xmonad > /dev/null 2>&1
#exec ck-launch-session dbus-launch --sh-syntax --exit-with-session i3 -V -d all > /tmp/i3log 2>&1
