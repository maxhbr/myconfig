#!/bin/sh
# ~/bin/myautosetup.sh
# Last modified: Sat Jan 25, 2014  04:20

#==============================================================================
#===  Global variables  =======================================================
#==============================================================================
DOCKED=$(cat /sys/devices/platform/dock.2/docked)
SECOND=1

#BatPresent=$(acpi -b | wc -l)
ACPresent=$(acpi -a | grep -c on-line)
XRANDR=`xrandr`

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

    DEFAULT_OUTPUT='LVDS1'
    OUTPUTS='VGA1 HDMI1 HDMI2'
    EXECUTE=""
    for CURRENT in $OUTPUTS; do
      if [[ $XRANDR == *$CURRENT\ connected*  ]]; then # is connected
        if [[ $XRANDR == *$CURRENT\ connected\ \(* ]]; then # is disabled
          EXECUTE+="--output $CURRENT --auto --above $DEFAULT_OUTPUT "
          xset dpms 99997 99998 99999 &
        else
          EXECUTE+="--output $CURRENT --off "
        fi
      else # make sure disconnected outputs are off
        EXECUTE+="--output $CURRENT --off "
      fi
    done
    xrandr --output $DEFAULT_OUTPUT --primary --auto $EXECUTE

    [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
    (
      [[ -f ~/.asoundrc.default ]] && cp ~/.asoundrc.default ~/.asoundrc
      sleep 1
      amixer -q set Master off
    )&
    ;;
  "1") #=======================================================================
    xset dpms 900 1800 2700 &

    if [[ $XRANDR == *HDMI2\ connected\ \(* \
        || $XRANDR == *LVDS1\ connected\ \(* ]]; then # is disabled
      /usr/bin/xrandr \
        --output HDMI2 --primary --mode 1920x1080 --right-of LVDS1 \
        --output LVDS1 --mode 1366x768
      [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
    else
      /usr/bin/xrandr --output HDMI2 --mode 1920x1080 --output LVDS1 --off

      #Error - unsupported ramp size 0
      #[[ -f ~/.icc/23.icc ]] && xcalib ~/.icc/23.icc &
      xset dpms 99997 99998 99999 &
    fi

    setxkbmap -layout de,de -variant neo,nodeadkeys -option\
      -option grp:shifts_toggle -option grp_led:scroll
    [[ -f ~/.asoundrc.uca202 ]] && cp ~/.asoundrc.uca202 ~/.asoundrc
    ;;
esac

feh --bg-center "/home/hubi/Bilder/background/BACKGROUND.png"
