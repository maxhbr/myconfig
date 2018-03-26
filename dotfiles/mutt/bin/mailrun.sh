#!/bin/sh
# Copyright 2015-2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# ~/bin/mailrun.sh
#
# This script runs periodicaly "offlineimap -o"
# - every 10 minutes a full synch is done
# - every 2 minutes a partial synch of the INBOX'es is done, if AC-power is
#   connected
# - runs a full synch after standby
# - trys to synch, only when network connected
# - the log is saved to /tmp/mailrun-log
# - the routine _monitor() checks, if offlineimap is alive, by checking wether
#   the log file changes

PID=$$
PIDFILE=/tmp/mailrun-sh-pid
LOGFILE=/tmp/mailrun-sh-log
ERRFILE=/tmp/mailrun-sh-err
STATFILE=/tmp/mailrun-mailstatus

pid=$(pgrep -f "/usr/bin/offlineimap")
if [[ ${pid} -gt 0 ]] ; then
  echo "Offlineimap is running with pid ${pid}" >>$ERRFILE 2>&1
  exit 1
fi
if [ -f ${PIDFILE} ] && [ -d "/proc/`cat $PIDFILE`" ]; then
  echo "mailrun.sh is already running" >>$ERRFILE 2>&1
  exit 2 # TODO: or kill the running instance?
fi

echo $PID >$PIDFILE
echo "**** started at $(date) ****" >$LOGFILE 2>&1

# genMailStatusFile() {
#   Mails1=$(find "$HOME/Mail/mail/INBOX/new/" -type f | wc -l)
#   Mails2=$(find "$HOME/Mail/by-hubi/INBOX/new/" -type f | wc -l)
#   Mails3=$(find "$HOME/Mail/alfa/INBOX/new/" -type f | wc -l)
#   echo -n "${Mails1}/${Mails2}/${Mails3}" > $STATFILE
# }

# Monitor offlineimap #######################################################
# checks every minute, if the logfile has changed in the last 5 minutes
_monitor() {
  while true; do
    sleep 60
    if [ -f $1 ]; then
      if test `find $1 -mmin +5`; then
        # dead for more then 5 Minutes
        if [[ $(pgrep -f "/usr/bin/offlineimap") -gt 0 ]] ; then
          sleep 20 # just wait some more, bevore killing offlineimap
          if test `find $1 -mmin +5`; then 
            # test, if it is realy dead
            pid=$(pgrep -f "/usr/bin/offlineimap")
            if [[ ${pid} -gt 0 ]] ; then
              echo "#############################################"
              echo "#############################################"
              echo "Offlineimap has hung with pid ${pid} (at $(date))"
              # kill offlineimap
              pkill -9 -f 'offlineimap\>.*-o'
              echo "-> killed"
              echo "#############################################"
              echo "#############################################"
            fi
          fi
        fi
      fi
    fi
  done >>$2 2>&1
}
_monitor $LOGFILE $ERRFILE &
trap 'kill $(jobs -p)' EXIT

LASTRUN=0
sleeptime=15 # sleep only 15s on the first run
# main loop #################################################################
while true; do
  sleep $sleeptime
  sleeptime=$((2*60))
  # check, if connected #####################################################
  if ! ping -c1 cloud.github.com > /dev/null 2>&1; then
    echo "**** not connected (ping) ****" >>$ERRFILE 2>&1
    # check again ###########################################################
    if ! wget -O - cloud.github.com > /dev/null 2>&1; then
      echo "**** not connected (wget) ****" >>$ERRFILE 2>&1
      continue
    fi
  fi
  # Run offlineimap #########################################################
  echo "**** Offlineimap started (at $(date)) ****"
  if [[ $(pgrep -f "/usr/bin/offlineimap") -gt 0 ]] ; then
    echo "#############################################"
    echo "Offlineimap is already running with pid ${pid}"
    echo "#############################################"
  else
    CURR=$(date '+%s')
    if [ $(($CURR-$LASTRUN)) -gt $((10*60)) ]; then
      LASTRUN=$CURR
      /usr/bin/offlineimap -o -u ttyui
      # echo "**** sort Maildir ****"
      # ~/workspace/haskell/mySortMaildir/mySortMaildir.sh
      echo "**** Offlineimap is ready (at $(date)) ****"
    else
      if [[ $(acpi -a | grep -c on-line) == "1" ]]; then
        /usr/bin/offlineimap -o -f INBOX -u ttyui
        echo "**** Offlineimap is ready (at $(date)) ****"
      else
        echo "**** no shortsynch, because no AC ****"
      fi
      echo "**** $((10-($CURR-$LASTRUN)/60))m till next full run ****"
    fi
  fi
done >>$LOGFILE 2>&1

rm $PIDFILE
