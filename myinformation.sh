#!/bin/bash
# ~/.xmonad/myinformation.sh

#TODO: iwconfig shouldnt be called double
essid=`iwconfig wlp3s0 | awk -F '"' '/ESSID/ {print $2}'`
stngth=`iwconfig wlp3s0 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1`

case `expr $stngth / 10` in
  0)  bar='[---------]' ;;
  1)  bar='[<fc=#ee9a00>Ξ</fc>--------] ' ;;
  2)  bar='[<fc=#ee9a00>Ξ</fc>--------] ' ;;
  3)  bar='[<fc=#ee9a00>ΞΞ</fc>-------] ' ;;
  4)  bar='[<fc=#ee9a00>ΞΞΞ</fc>------] ' ;;
  5)  bar='[<fc=#ee9a00>ΞΞΞΞ</fc>-----] ' ;;
  6)  bar='[<fc=#ee9a00>ΞΞΞΞΞ</fc>----] ' ;;
  7)  bar='[<fc=#ee9a00>ΞΞΞΞΞΞ</fc>---] ' ;;
  8)  bar='[<fc=#ee9a00>ΞΞΞΞΞΞΞ</fc>--] ' ;;
  9)  bar='[<fc=#ee9a00>ΞΞΞΞΞΞΞΞ</fc>-] ' ;;
  10) bar='[<fc=#ee9a00>ΞΞΞΞΞΞΞΞΞ</fc>] ' ;;
  #*)  bar='[<fc=#ee9a00>!</fc>] ' ;;
  *)  bar='' ;;
esac
echo -n $essid $bar

#OfflineImapON=$(ps -A | grep -c offlineimap)
#Mails1=$(find "$HOME/Mail/by-hubi/INBOX/new/" -type f | wc -l)
#Mails2=$(find "$HOME/Mail/alfa/INBOX/new/" -type f | wc -l)
#if ! [[ $OfflineImapON == "0" ]]; then
  #if ! [[ $Mails1 == "0" && $Mails2 == "0" ]]; then
    #echo -n "| <fc=#00ff00>Mail: $Mails1 / $Mails2</fc> "
  #fi
#elif ! [[ $Mails1 == "0" && $Mails2 == "0" ]]; then
  #echo -n "| <fc=#ff0000>no IMAP: $Mails1 / $Mails2</fc>"
#else
  #echo -n "| <fc=#ff0000>no IMAP</fc>"
#fi

Mails1=$(find "$HOME/Mail/by-hubi/INBOX/new/" -type f | wc -l)
Mails2=$(find "$HOME/Mail/alfa/INBOX/new/" -type f | wc -l)
if test `find /tmp/mailrun-sh-log -mmin -2`; then
  if ! [[ $Mails1 == "0" && $Mails2 == "0" ]]; then
    echo -n "| <fc=#00ff00>Mail: $Mails1 / $Mails2</fc> "
  fi
elif ! [[ $Mails1 == "0" && $Mails2 == "0" ]]; then
  echo -n "| <fc=#ff0000>no IMAP: $Mails1 / $Mails2</fc>"
else
  echo -n "| <fc=#ff0000>no IMAP</fc>"
fi


DropboxON=$(ps -A | grep -c dropbox)
if ! [[ $DropboxON == "0" ]]; then
  echo -n "| DB: $DropboxON "
fi

VirtualBoxON=$(ps -A | grep -c VirtualBox)
if ! [[ $VirtualBoxON == "0" ]]; then
  echo -n "| VB: $VirtualBoxON "
fi

#echo -n " "

echo -n $(amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print " | <fc=#00ff00>m</fc>" } else { print " | Vol: " $2/10 }}' | head -n 1)

#others:
#printDiskInfo
  #RFSP=$(df -h / | tail -1 | awk '{ print $5 }' | tr -d '%')
  #HFSP=$(df -h /home | tail -1 | awk '{ print $5 }' | tr -d '%')

#printBrightnessInfo()
  #BRIFILE=$(cat /sys/class/backlight/acpi_video0/actual_brightness)
  #Bri=$(expr $BRIFILE \* 15)

#anzahl Prozesse:
  #ps -ef | wc -l
