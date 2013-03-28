#!/bin/sh
# ~/.xinitrc
#
# written by maximilian-huber.de
#
# Last modified: Do Mär 21, 2013  03:43

#toggle to neo via scrol
#setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scrol
#setxkbmap -layout de -variant nodeadkeys
# set caps lock as ctrl
setxkbmap -option ctrl:nocaps
xsetroot -cursor_name left_ptr
xset dpms 300 600 900 & # screen saver replacement
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

#TODO: only start once
#[[ $(ps -A | grep -c urxvtd) == "0" ]] && exec urxvtd -q -f -o &
exec urxvtd -q -f -o &

#if [ -f ~/bin/mydock.sh ]; then
  #~/bin/mydock.sh fast &
#fi

feh --bg-center "/home/hubi/.xmonad/background2o.png" &
amixer sset Master off &
xbacklight -set 70 & 
xcalib .icc/x230.icc &
#rfkill block bluetooth # fehlende rechte
#{ ntpd -qg; hwclock -w; } & 

([[ -f ~/bin/mailrun.sh ]] && ~/bin/mailrun.sh) &

exec dbus-launch xmonad
