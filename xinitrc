#!/bin/sh
# ~/.xinitrc
#
# written by maximilian-huber.de
#
# Last modified: Sa Jan 26, 2013  10:35

#toggle to neo via scrol
#setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scrol
#setxkbmap -layout de -variant nodeadkeys
# set caps lock as ctrl
setxkbmap -option ctrl:nocaps
xsetroot -cursor_name left_ptr
xset dpms 300 600 900 & # screen saver replacement
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

exec urxvtd -q -f -o &

#if [ -f ~/bin/mydock.sh ]; then
  #~/bin/mydock.sh fast &
#fi

feh --bg-center "/home/hubi/.xmonad/background2o.png" &
amixer sset Master off &
xbacklight -set 70 & 
#rfkill block bluetooth # fehlende rechte
#{ ntpd -qg; hwclock -w; } & 

exec dbus-launch xmonad
