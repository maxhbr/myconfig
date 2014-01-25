#!/bin/sh
# ~/.xinitrc
#
# written by maximilian-huber.de
#
# Last modified: Sat Jan 25, 2014  04:22

#setxkbmap -option ctrl:nocaps -layout de -variant nodeadkeys
#setxkbmap -layout de,de -variant neo,nodeadkeys -option -option grp:sclk_toggle -option grp_led:scroll
setxkbmap -layout de,de -variant neo,nodeadkeys -option -option grp:shifts_toggle -option grp_led:scroll
xsetroot -cursor_name left_ptr
xset dpms 300 600 900 & # screen saver replacement
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

export LANG="de_DE.utf8"

#TODO: only start once
#pid=$(pgrep -f "urxvtd")
#if [[ ! ${pid} -gt 0 ]] ; then
  exec urxvtd -q -f -o &
#fi

(
  if xrandr -q | grep -q "HDMI2 connected"; then
    xrandr --output HDMI2 --mode 1920x1080 --right-of LVDS1 \
      --output LVDS1 --mode 1366x768
    [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
    # [[ -f ~/.icc/23.icc ]] && xcalib -s 0 ~/.icc/23.icc &
  else
    [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
  fi
  feh --bg-center "/home/hubi/.xmonad/BACKGROUND.png"
) &

#amixer sset Master off &
amixer -q set Master off &
xbacklight =70 &

([[ -f ~/bin/mailrun.sh ]] && ~/bin/mailrun.sh) &

exec dbus-launch /home/hubi/.cabal/bin/xmonad
