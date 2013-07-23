#!/bin/sh
# ~/.xinitrc
#
# written by maximilian-huber.de
#
# Last modified: Di Jul 23, 2013  02:41

setxkbmap -option ctrl:nocaps -layout de -variant nodeadkeys
xsetroot -cursor_name left_ptr
xset dpms 300 600 900 & # screen saver replacement
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

#TODO: only start once
#[[ $(ps -A | grep -c urxvtd) == "0" ]] && exec urxvtd -q -f -o &
exec urxvtd -q -f -o &

(
  if xrandr -q | grep -q "HDMI2 connected"; then
    xrandr --output HDMI2 --mode 1920x1080 --right-of LVDS1 \
      --output LVDS1 --mode 1366x768
    xcalib -s 0 .icc/x230.icc &
    #xcalib -s 1 .icc/23.icc &
  else
    xcalib .icc/x230.icc &
  fi
  feh --bg-center "/home/hubi/.xmonad/background2o.png"
) &
  #sleep 20; /home/hubi/bin/myInformationToBkg.sh

#amixer sset Master off &
amixer -q set Master off &
xbacklight =70 &

([[ -f ~/bin/mailrun.sh ]] && ~/bin/mailrun.sh) &

exec dbus-launch xmonad
