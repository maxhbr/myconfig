#!/bin/sh
#
# ~/.xinitrc

# Get back ctrl+alt+backspace in new versions of xorg
# setxkbmap -option terminate:ctrl_alt_bksp

xsetroot -cursor_name left_ptr

#transparency
xcompmgr -n &
## Disable beeps
#xset -b # doesnt work
xset dpms 300 600 900 & # screen saver replacement
#/usr/bin/xscreensaver -no-splash &
#setxkbmap -layout de
xbacklight -set 90 &

## OSD
#dunst &


# keynav & # mouseless
exec urxvtd & # -q -o -f &

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

if [ -f ~/bin/mydock.sh ]; then
  #exec ~/bin/mydock.sh fast &
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

# exec conky &

# main Display
#if [ "${DISPLAY}" = ":0" ]; then
#fi

#exec unclutter & # moved to xmonad.hs
exec ck-launch-session dbus-launch xmonad
#exec ck-launch-session dbus-launch xmonad > /dev/null 2>&1
#exec ck-launch-session dbus-launch --sh-syntax --exit-with-session i3 -V -d all > /tmp/i3log 2>&1
