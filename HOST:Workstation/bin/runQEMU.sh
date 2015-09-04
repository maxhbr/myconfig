#!/usr/bin/env bash
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
# Last modified: So Feb 01, 2015  11:34
#

###############################################################################
##  variables  ################################################################
IMG=
BRIDGED=true
VNC=false
CDROM= #ubuntu-14.04.1-server-amd64.iso
NUMCPUS=2
RAM=4096M
USB= #"usb-host,bus=usb-bus.0,hostbus=1,hostport=1.2"
###############################################################################

###############################################################################
##  input parameters  #########################################################
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
if [ $# -eq 1 ]; then
  echo $1
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
##  run  ######################################################################
if [ $# -eq 0 ]; then
  ##  check parameters  #######################################################
  if [ -z "$IMG" ]; then
    echo "no image given"
    exit 1
  else
    if [ ! -e "$IMG" ]; then
      echo $IMG
      echo "image not found!"
      exit 1
    fi
  fi
  echo "-- setup -------------------------------------------------------------"
  echo "IMG is $IMG"
  echo "BRIDGED is $BRIDGED"
  echo "VNC is $VNC"
  echo "CDROM is $CDROM"
  echo "NUMCPUS is $NUMCPUS"
  echo "RAM is $RAM"
  echo "USB is $USB"
  echo

  ##  param  ##################################################################
  echo "-- parameters --------------------------------------------------------"
  param=""
  addParam(){
    param="$param $@"
    echo $@
  }
  addParam "-k de"
  addParam "-enable-kvm -cpu host -smp $NUMCPUS -m $RAM"
  addParam "-machine type=pc,accel=kvm"
  if [ "$BRIDGED" = true ]; then
    # Bridgebased networking
    DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
    ME="$DIR/$(basename "$(test -L "$0" && readlink "$0" || echo "$0")")"
    addParam "-net nic,vlan=0 -net tap,vlan=0,ifname=tap0,script=$ME"
  else
    # redirect important ports
    addParam "-redir tcp:10022::22 -redir tcp:10023::80 -redir tcp:10024::445"
  fi
  addParam "-localtime"
  if [ "$VNC" = true ]; then
    addParam "-vnc :2"
  else
    addParam "-nographic"
  fi
  if [ ! -z "$USB" ]; then
    addParam "-usb -device $USB"
  fi
  if [ ! -z "$CDROM" ]; then
    if [ -e "$CDROM" ]; then
      addParam "-cdrom \"$CDROM\""
    # else
    #   echo "cdrom not found. IGNORED!"
    fi
  fi
  addParam "-boot d"
  addParam "-hda $IMG"
  echo

  ##  run  ####################################################################
  echo "-- run ---------------------------------------------------------------"
  sudo qemu-system-x86_64 $param
else
  # for bridged networking:
  echo "Bringing up $1 for bridged mode..."
  sudo /sbin/ifconfig $1 0.0.0.0 promisc up
  echo "Adding $1 to br0..."
  sudo /sbin/brctl addif br0 $1
  sleep 2
fi
###############################################################################
