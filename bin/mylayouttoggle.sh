#!/bin/sh
# toggle between default layout and neo

if [[ -f /tmp/mykeyboard ]]; then
  #setxkbmap de nodeadkeys -option
  setxkbmap -layout de,de -variant nodeadkeys,neo -option -option grp:sclk_toggle -option grp_led:scrol
  setxkbmap -option ctrl:nocaps
  feh --bg-center "/home/mhuber/.xmonad/background1.png"
  echo "nodeadkeys"
  rm /tmp/mykeyboard
else
  case "neo" in
    "neo")
      #setxkbmap de neo -option
      setxkbmap -layout de,de -variant neo,nodeadkeys -option -option grp:sclk_toggle -option grp_led:scrol
      feh --bg-center "/home/mhuber/.xmonad/neo_Ebenen_1_2_3_4.png"
      echo "neo"
      echo "neo" > /tmp/mykeyboard
      ;;
    "dvorak")
      setxkbmap us dvorak -option
      feh --bg-center "/home/mhuber/.xmonad/dvorak.gif"
      echo "dvorak"
      echo "dvorak" > /tmp/mykeyboard
      ;;
  esac
fi
