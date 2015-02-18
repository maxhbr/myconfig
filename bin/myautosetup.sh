#!/bin/sh
# ~/bin/myautosetup.sh
# Last modified: Sat Feb 14, 2015  12:25

#==============================================================================
#===  Global variables  =======================================================
#==============================================================================
DOCKED=$(cat /sys/devices/platform/dock.0/docked)
SECOND=1

#BatPresent=$(acpi -b | wc -l)
ACPresent=$(acpi -a | grep -c on-line)
XRANDR=`xrandr`

#TODO:
#MPD_HOST="mpd@192.168.178.61"
#mpc -h MPD_HOST --no-status pause

#==============================================================================
#===  Input arguments  ========================================================
#==============================================================================
while [ $# -ne 0 ]; do
  case "$1" in
    single)
      SECOND=0
      ;;
    wait)
      sleep 0.5
      ;;
  esac
  shift
done

#==============================================================================
#===  Main part  ==============================================================
#==============================================================================
[[ $ACPresent == "0" ]] && { xbacklight =70 & } || { xbacklight =100 & }

case "$DOCKED" in
  "0") #=======================================================================
    xset dpms 300 600 900 &

    # DEFAULT_OUTPUT='eDP1'
    # OUTPUTS='DP1 DP2 DP2-1 DP2-2 DP2-3 HDMI1 HDMI2 VIRTUAL1'
    # EXECUTE=""
    # for CURRENT in $OUTPUTS; do
    #   if [[ $XRANDR == *$CURRENT\ connected*  ]]; then # is connected
    #     if [[ $XRANDR == *$CURRENT\ connected\ \(* ]]; then # is disabled
    #       EXECUTE+="--output $CURRENT --auto --above $DEFAULT_OUTPUT "
    #       xset dpms 99997 99998 99999 &
    #     else
    #       EXECUTE+="--output $CURRENT --off "
    #     fi
    #   else # make sure disconnected outputs are off
    #     EXECUTE+="--output $CURRENT --off "
    #   fi
    # done
    # xrandr --output $DEFAULT_OUTPUT --primary --auto $EXECUTE
    xrandr --output VIRTUAL1 --off \
           --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal \
           --output DP1 --off \
           --output DP2-1 --off \
           --output DP2-2 --off \
           --output DP2-3 --off \
           --output HDMI2 --off \
           --output HDMI1 --off \
           --output DP2 --off

    rfkill unblock all &

    # [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
    (
      sleep 1
      amixer -q set Master off
    )&
    [[ -f ~/.asoundrc.default ]] && cp ~/.asoundrc.default ~/.asoundrc
    ;;
  "1") #=======================================================================
    xset dpms 900 1800 2700 &

    if [[ $XRANDR == *DP2-1\ connected\ \(* \
        || $XRANDR == *eDP1\ connected\ \(* ]]; then # is disabled
      /usr/bin/xrandr \
        --output DP2-1 --primary --mode 1920x1080 --right-of eDP1 \
        --output eDP1 --mode 1920x1080
      # [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
    else
      /usr/bin/xrandr --output DP2-1 --mode 1920x1080 --output eDP1 --off

      #Error - unsupported ramp size 0
      #[[ -f ~/.icc/23.icc ]] && xcalib ~/.icc/23.icc &
      xset dpms 99997 99998 99999 &
    fi

    rfkill block all &
    ;;
esac

setxkbmap -layout de,de -variant neo,nodeadkeys -option\
  -option grp:shifts_toggle -option grp_led:scroll
# [[ -f ~/.xmodmap ]] && xmodmap ~/.xmodmap

feh --bg-center "/home/mhuber/Bilder/background/BACKGROUND.png"
