#!/bin/sh
# a simple script to backup my files
# Last modified: Di Apr 30, 2013  08:51

###############################################################################
####  Funktionen  #############################################################
############################################################################{{{
myecho(){ echo "$1"; echo "$1" >> $LOGFILE; }

have() { type "$1" &> /dev/null; }

setBKDIR(){
  if [[ ! -d "$1" ]]; then
    echo "*****************************************************************"
    echo "****  directory does not exist:  ********************************"
    echo "****  ${1}"
    echo "*****************************************************************"
    exit 1
  fi
  read -r HOST < /etc/hostname
  BKDIR="$1/$HOST-bac"
  if [[ ! -d "$BKDIR" ]]; then
    if [ "$(id -u)" != "0" ]; then
      mkdir -p $BKDIR
    else
      echo "*****************************************************************"
      echo "****  main folder should not be created as root  ****************"
      echo "*****************************************************************"
      exit 1
    fi
  fi
  LOGFILE="$BKDIR/$HOST.log"
}

bkObject () {
  if [ ! -r $1 ]; then
    myecho "**** not readable:   ${1}"
  elif [ -d "$1" ]; then
    myecho $1
    mkdir -p $(dirname $BKDIR$1)
    cp -npr $1 $(dirname $BKDIR$1)
  elif [ -h $1 ]; then
    myecho "****  symbolic link:   ${1}"
  elif [ -f "$1" ]; then
    myecho $1
    mkdir -p $(dirname $BKDIR$1)
    cp -npr $1 $BKDIR$1
  fi
}

bkObjectTar () {
  if [ ! -r $1 ]; then
    myecho "**** not readable:   ${1}"
  elif [ -d "$1" ]; then
    myecho "${1} to ${BKDIR}${1%/}.tar.gz"
    mkdir -p $(dirname $BKDIR$1)
    tar -pvczf "${BKDIR}${1%/}.tar.gz" "${1%/}"
  fi
}

############################################################################}}}
####  Variablen / Argumente   #################################################
############################################################################{{{
# Globale Variablen
BKDIR=""
LOGFILE=""
TIMESTAMP=`date +%d.%m.%Y-%H:%M`

setBKDIR "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
while [[ $# > 0 ]] ; do
  case $1 in
    -d)
      shift
      setBKDIR $1
      ;;
    -o)
      shift
      myecho "*****************************************************************"
      myecho "****  Bachup $1"
      myecho "*****************************************************************"
      bkObjectTar "$1"
      exit 0
      ;;
    -h)
      echo "mybackup.sh"
      echo "a simple script to backup my files"
      echo
      echo "run as user, to backup config and large folders"
      echo "run as root, to backup private files"
      echo "or run with -a to do both"
      echo
      echo "mybackup.sh [-d BKDIR] [-o single_folder] [-a]"
      ;;
    -a)
      sudo sh ${BASH_SOURCE[0]}
      sh ${BASH_SOURCE[0]}
      exit 0
      ;;
  esac
  shift
done

############################################################################}}}
####  Backup!  ################################################################
############################################################################{{{
[[ $BKDIR == *-bac* ]] || exit 2
[[ -d "$BKDIR" ]] || exit 3
myecho "*****************************************************************"
myecho "****  $TIMESTAMP"
myecho "*****************************************************************"
myecho "****  backup itself  ********************************************"
cp -np "${BASH_SOURCE[0]}" $BKDIR
if [ "$(id -u)" != "0" ]; then
  [[ -d "/home/hubi/myconfig/" ]] && cp "${BASH_SOURCE[0]}" /home/hubi/myconfig/
  myecho "****  as ${USER}"
  have pacman && {
    myecho "*****************************************************************"
    myecho "****  save list of installed packages  **************************"
    myecho "*****************************************************************"
    pacman -Qeq > ${BKDIR}/Pakete.txt
  }
  myecho "*****************************************************************"
  myecho "****  Bachup Configuration  *************************************"
  myecho "*****************************************************************"
  while IFS= read -r LINE; do
    [[ ! $LINE == \#* ]] && bkObject "$LINE"
  done <<EOF
/boot/grub/grub.cfg
/etc/NetworkManager/dispatcher.d
/etc/X11/xorg.conf.d
/etc/chromium/default
/etc/conf.d
/etc/fstab
/etc/hostname
/etc/hosts
/etc/laptop-mode/
/etc/locale.conf
/etc/modprobe.d/audio_power_save.conf
/etc/modules-load.d/
/etc/pacget.conf
/etc/pacman.conf
/etc/pm
/etc/resolv.conf
/etc/sysctl.d/disable_watchdog.conf
/etc/sysctl.d/laptop_mode.conf
/etc/systemd/
/etc/udev
/etc/udev/rules.d/disable_wol.rules
/etc/udev/rules.d/pci_pm.rules
/etc/udev/rules.d/usb_power_save.rules
/etc/udev/rules.d/wifi_power_save.rules
/etc/vconsole.conf
/etc/vimrc
/etc/zsh/
/home/hubi/.Xresources
/home/hubi/.aliasrc
/home/hubi/.aliasrc-local
/home/hubi/.bash_profile
/home/hubi/.bashrc
/home/hubi/.config/LyX
/home/hubi/.config/RawTherapee
/home/hubi/.config/dwb
/home/hubi/.config/geeqie
/home/hubi/.config/htop
/home/hubi/.config/mc
/home/hubi/.config/pcmanfm
/home/hubi/.config/ranger
/home/hubi/.conkyrc
/home/hubi/.emacs
/home/hubi/.emacs.d
/home/hubi/.fehbg
/home/hubi/.fontconfig/fonts.conf
/home/hubi/.gtk-bookmarks
/home/hubi/.icc
/home/hubi/.keynavrc
/home/hubi/.muttrc
/home/hubi/.myhelp
/home/hubi/.tmux.conf
/home/hubi/.vifm/vifmrc
/home/hubi/.vim
/home/hubi/.vimencrypt
/home/hubi/.vimrc
/home/hubi/.xinitrc
/home/hubi/.xmonad/batteryrate.sh
/home/hubi/.xmonad/coreTemp.sh
/home/hubi/.xmonad/myinformation.sh
/home/hubi/.xmonad/myvolume.sh
/home/hubi/.xmonad/togglemouse.sh
/home/hubi/.xmonad/xmobar-start
/home/hubi/.xmonad/xmobarrc
/home/hubi/.xmonad/xmonad.hs
/home/hubi/.zshrc.local
/home/hubi/bin
/home/hubi/myconfig/aktualisieren
/usr/bin/chromium
/usr/bin/chromium-tmpfs
/usr/bin/pacget
EOF
  myecho "$TIMESTAMP" > "${BKDIR}/date-config"
  read -p "Backup Data?" -n 1 -r
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    myecho "*****************************************************************"
    myecho "****  Bachup Data  **********************************************"
    myecho "*****************************************************************"
    while IFS= read -r LINE; do
      [[ ! $LINE == \#* ]] && bkObjectTar "$LINE"
    done <<EOF
/home/hubi/Bilder/00-galerie/
/home/hubi/Bilder/Logo/
/home/hubi/Dokumente/
/home/hubi/Dropbox/git/
/home/hubi/Mail/backup/
/home/hubi/studium/
/home/hubi/workspace/
EOF
    myecho "$TIMESTAMP" > "${BKDIR}/date-data"
  fi
  myecho "*****************************************************************"
  myecho "you may also run as ROOT"
else
  myecho "****  as ${USER}"
  myecho "*****************************************************************"
  myecho "****  Bachup Private  *******************************************"
  myecho "*****************************************************************"
  while IFS= read -r LINE; do
    [[ ! $LINE == \#* ]] && bkObject "$LINE"
  done <<EOF
/etc/NetworkManager
/etc/conf.d
/etc/sudoers
/etc/wicd
/etc/systemd/system/offlineimap-hubi.service
/home/hubi/.abook/
/home/hubi/.filezilla
/home/hubi/.mailcap
/home/hubi/.msmtprc
/home/hubi/.mutt/
/home/hubi/.muttrc
/home/hubi/.mytodo
/home/hubi/.purple
/home/hubi/.smbsecrets
/home/hubi/.sylpheed-2.0
/home/hubi/.when/
/home/hubi/Mail/config/
/home/hubi/TODO/
/home/hubi/backup/other/
/home/hubi/backup/private/
EOF
  myecho "$TIMESTAMP" > "${BKDIR}/date-private"
  myecho "*****************************************************************"
  myecho "you may also run as non ROOT"
fi
############################################################################}}}
# vim:set ts=2 sw=2 sts=2 et foldmethod=marker foldmarker={{{,}}}:
