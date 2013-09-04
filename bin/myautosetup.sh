#!/bin/sh
# ~/bin/myautosetup.sh
# Last modified: Mi Sep 04, 2013  02:57


#==============================================================================
#===  Global variables  =======================================================
#==============================================================================
DOCKED=$(cat /sys/devices/platform/dock.2/docked)
SECOND=1

#BatPresent=$(acpi -b | wc -l)
ACPresent=$(acpi -a | grep -c on-line)

#==============================================================================
#===  Input arguments  ========================================================
#==============================================================================
while [ $# -ne 0 ]; do
  case "$1" in
    single)
      shift
      SECOND=0
      ;;
    wait)
      sleep 0.5
      ;;
  esac
  shift
done

#==============================================================================
#===  Functions  ==============================================================
#==============================================================================
toggleDisplays(){
  # toggles the extended monitor outputs if something is connected
  DEFAULT_OUTPUT='LVDS1'
  OUTPUTS='HDMI1 HDMI2'
  XRANDR=`xrandr`
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
}

#==============================================================================
#===  Main part  ==============================================================
#==============================================================================
if [[ $ACPresent == "0" ]]; then
  xbacklight =70 &
else
  xbacklight =100 &
fi

case "$DOCKED" in
  "0")
    xset dpms 300 600 900 &
    toggleDisplays
    [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
    ;;
  "1")
    xset dpms 900 1800 2700 &
    /usr/bin/xrandr --output HDMI2 --primary --mode 1920x1080 --right-of LVDS1
    if [ $SECOND -eq 1 ]; then
      /usr/bin/xrandr --output LVDS1 --mode 1366x768
      [[ -f ~/.icc/x230.icc ]] && xcalib -s 0 ~/.icc/x230.icc &
    else
      /usr/bin/xrandr --output LVDS1 --off
      [[ -f ~/.icc/23.icc ]] && xcalib -s 0 ~/.icc/23.icc &
      xset dpms 99997 99998 99999 &
    fi
    ;;
esac

feh --bg-center "/home/hubi/.xmonad/background2o.png"
setxkbmap -option ctrl:nocaps -layout de -variant nodeadkeys
