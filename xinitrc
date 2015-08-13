#!/bin/sh
# ~/.xinitrc
#
# written by maximilian-huber.de
#
# Last modified: Wed Aug 12, 2015  09:47AM

#setxkbmap -option ctrl:nocaps -layout de -variant nodeadkeys
#setxkbmap -layout de,de -variant neo,nodeadkeys -option -option grp:sclk_toggle -option grp_led:scroll
setxkbmap -layout de,de -variant neo,nodeadkeys\
  -option grp:shifts_toggle -option grp_led:scroll\
  -option altwin:swap_lalt_lwin

xsetroot -cursor_name left_ptr
xset dpms 300 600 900 & # screen saver replacement
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
# [[ -f ~/.xmodmap ]] && xmodmap ~/.xmodmap

# export LANG="de_DE.utf8"

#TODO: only start once
#pid=$(pgrep -f "urxvtd")
#if [[ ! ${pid} -gt 0 ]] ; then
  exec urxvtd -q -f -o &
#fi

(
  if xrandr -q | grep -q "HDMI2 connected"; then
    xrandr --output HDMI2 --mode 1920x1080 --right-of LVDS1 \
      --output LVDS1 --mode 1366x768
  fi
  feh --bg-center "~/Bilder/background/BACKGROUND.png"
) &

#amixer sset Master off &
amixer -q set Master off &
xbacklight =70 &
redshift -l 48.2:10.8 &

([[ -f ~/bin/mailrun.sh ]] && ~/bin/mailrun.sh) &

rfkill unblock all &

if [[ -f ~/.nix-profile/bin/xmonad ]]; then
  exec dbus-launch ~/.nix-profile/bin/xmonad
elif [[ -f ~/.cabal/bin/xmonad ]]; then
  exec dbus-launch ~/.cabal/bin/xmonad
else
  exec dbus-launch /usr/bin/xmonad
fi
