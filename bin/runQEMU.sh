#!/bin/sh
#
# a verry basic script, for runing qemu
#
# usage:
#   $ runQEMU.sh /path/to/disc.img
#   $ runQEMU.sh -cdrom /path/to/cdrom.iso /path/to/disc.img
#   $ runQEMU.sh -img /path/to/disc.img 
#   $ runQEMU.sh -img /path/to/disc.img
#               [-cpus 2]
#               [-ram 2048M]
#               [-cdrom /path/to/cdrom.iso]
#
# for bridged network (usually called from this script):
#   $ runQEMU.sh br0
#
#   written by maximilian-huber.de
# Last modified: Sun Feb 01, 2015  11:24
#

###############################################################################
##  variables  ################################################################
IMG=
BRIDGED=false
CDROM= #ubuntu-14.04.1-server-amd64.iso
NUMCPUS=2
RAM=4096M
USB= #"usb-host,bus=usb-bus.0,hostbus=1,hostport=1.2"
###############################################################################

###############################################################################
##  input parameters  #########################################################
# while (( "$#" )); do
while [[ "$#" -gt 1 ]]; do
  if [ "$1" == "-img" ]; then
    shift; IMG=$1; shift
  fi
  if [ "$1" == "-cpus" ]; then
    shift; NUMCPUS=$1; shift
  fi
  if [ "$1" == "-ram" ]; then
    shift; RAM=$1; shift
  fi
  if [ "$1" == "-cdrom" ]; then
    shift; CDROM=$1; shift
  fi
  if [ "$1" == "-bridge" ]; then
    BRIDGED=true
  fi
  if [ "$1" == "-nobridge" ]; then
    BRIDGED=false
  fi
  if [ "$1" == "-vnc" ]; then
    VNC=true
  fi
  if [ "$1" == "-novnc" ]; then
    VNC=false
  fi
  echo $1
done
if [ $# -eq 1]; then
  if [ -e "$1" ]; then
    if [ -z "$IMG" ]; then
      $IMG=$1
    else
      $CDROM=$1
    fi
    shift
  fi
fi
###############################################################################

###############################################################################
##  check parameters  #########################################################
if [ -z "$IMG" ]; then
  echo "no image given"
  exit 1
fi

if [ ! -e "$IMG" ]; then
  echo $IMG
  echo "image not found!"
  exit 1
fi
###############################################################################

###############################################################################
##  run  ######################################################################
if [ $# -eq 0 ]; then
  param=""
  param="${param} -enable-kvm -cpu host -smp $NUMCPUS -m $RAM"
  param="${param} -machine type=pc,accel=kvm"
  if [ "$BRIDGED" = true ]; then
    # Bridgebased networking
    ME="$(basename "$(test -L "$0" && readlink "$0" || echo "$0")")"
    param="${param} -net nic,vlan=0 -net tap,vlan=0,ifname=tap0,script=$ME"
  else
    # redirect important ports
    param="${param} -redir tcp:10022::22"
    param="${param} -redir tcp:10023::80"
    param="${param} -redir tcp:10024::445"
  fi
  param="${param} -localtime"
  param="${param} -vnc :2"
  if [ ! -z "$USB" ]; then
    param="${param} -usb -device $USB"
  fi
  if [ ! -z "$CDROM" ]; then
    if [ -e "$CDROM" ]; then
      param="${param} -cdrom \"$CDROM\""
    else
      echo "cdrom not found. IGNORED!"
    fi
  fi
  param="${param} -boot d"
  qemu-system-x86_64 $param -hda "$IMG"
else
  # for bridged networking:
  echo "Bringing up $1 for bridged mode..."
  sudo /sbin/ifconfig $1 0.0.0.0 promisc up
  echo "Adding $1 to br0..."
  sudo /sbin/brctl addif br0 $1
  sleep 2
fi
###############################################################################
