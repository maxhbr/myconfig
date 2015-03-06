#!/bin/bash
# ~/.xmonad/myinformation.sh

###############################################################################
## Wifi
if [[ "$(rfkill list 0 | grep -c yes)" == "0" ]]; then
  #TODO: iwconfig shouldnt be called double
  essid=`iwconfig wlp3s0 \
    | awk -F '"' '/ESSID/ {print $2}'`
  if ! [[ -z "$essid" ]]; then
    stngth=`iwconfig wlp3s0 \
      | awk -F '=' '/Quality/ {print $2}' \
      | cut -d '/' -f 1`
    case `expr $stngth / 10` in
      0)  bar='[---------]' ;;
      1)  bar='[<fc=#ee9a00>#</fc>--------]' ;;
      2)  bar='[<fc=#ee9a00>#</fc>--------]' ;;
      3)  bar='[<fc=#ee9a00>##</fc>-------]' ;;
      4)  bar='[<fc=#ee9a00>###</fc>------]' ;;
      5)  bar='[<fc=#ee9a00>####</fc>-----]' ;;
      6)  bar='[<fc=#ee9a00>#####</fc>----]' ;;
      7)  bar='[<fc=#ee9a00>######</fc>---]' ;;
      8)  bar='[<fc=#ee9a00>#######</fc>--]' ;;
      9)  bar='[<fc=#ee9a00>########</fc>-]' ;;
      10) bar='[<fc=#ee9a00>#########</fc>]' ;;
      #*)  bar='[<fc=#ee9a00>!</fc>] ' ;;
      *)  bar='' ;;
    esac
    echo -n "$essid $bar "
  fi
else
  echo -n "<fc=#ff0000>¬RF</fc> "
fi

###############################################################################
## Mail
Mails1=$(find "$HOME/Mail/mail/INBOX/new/" -type f | wc -l)
Mails2=$(find "$HOME/Mail/by-hubi/INBOX/new/" -type f | wc -l)
Mails3=$(find "$HOME/Mail/alfa/INBOX/new/" -type f | wc -l)
if test `find /tmp/mailrun-sh-log -mmin -2`; then
  if ! [[ $Mails1 == "0" && $Mails2 == "0" && $Mails3 == "0" ]]; then
    echo -n "| <fc=#00ff00>Mail: ${Mails1}/${Mails2}/${Mails3}</fc> "
  fi
elif ! [[ $Mails1 == "0" && $Mails2 == "0" && $Mails3 == "0" ]]; then
  echo -n "| <fc=#ff0000>¬IMAP: ${Mails1}/${Mails2}/${Mails3}</fc> "
else
  echo -n "| <fc=#ff0000>¬IMAP</fc> "
fi

###############################################################################
## count processes
psOut=$(ps -A)

BTSyncON=$(echo $psOut | grep -c btsync)
if ! [[ $BTSyncON == "0" ]]; then
  echo -n "| BT: $BTSyncON "
fi

DropboxON=$(echo $psOut | grep -c dropbox)
if ! [[ $DropboxON == "0" ]]; then
  echo -n "| DB: $DropboxON "
fi

VirtualBoxON=$(echo $psOut | grep -c VirtualBox)
if ! [[ $VirtualBoxON == "0" ]]; then
  echo -n "| VB: $VirtualBoxON "
fi

QemuON=$(echo $psOut | grep -c qemu)
if ! [[ $QemuON == "0" ]]; then
  echo -n "| Qemu: $QemuON "
fi

###############################################################################
## Volume
volume=$(amixer get Master \
  | awk -F'[]%[]' '/%/ {if ($7 == "off") { print " | <fc=#00ff00>m</fc>" } \
                                    else { print " | Vol: " $2/10 }}' \
  | head -n 1)
if ! [[ -z "$volume" ]]; then
  echo -n "$volume "
fi

###############################################################################
## Others:
#printDiskInfo
  #RFSP=$(df -h / | tail -1 | awk '{ print $5 }' | tr -d '%')
  #HFSP=$(df -h /home | tail -1 | awk '{ print $5 }' | tr -d '%')

#printBrightnessInfo()
  #BRIFILE=$(cat /sys/class/backlight/acpi_video0/actual_brightness)
  #Bri=$(expr $BRIFILE \* 15)

#anzahl Prozesse:
  #ps -ef | wc -l
