#!/bin/sh
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
#
# Last modified: Di Jul 23, 2013  09:07

PID=$$
PIDFILE=/tmp/mailrun-sh-pid
LOGFILE=/tmp/mailrun-sh-log

pid=$(pgrep -f "/usr/bin/offlineimap")
if [[ ${pid} -gt 0 ]] ; then
  echo "Offlineimap is running with pid ${pid}"
  exit 1
fi
if [ -f ${PIDFILE} ] && [ -d "/proc/`cat $PIDFILE`" ]; then
  echo "mailrun.sh is already running"
  exit 2 # TODO: or kill the running instance?
fi

echo $PID >$PIDFILE
echo "**** started at $(date) ****" >$LOGFILE 2>&1

# Monitor offlineimap #######################################################
# checks every minute, if the logfile has changed in the last 5 minutes
_monitor() {
  LOGFILE=/tmp/mailrun-sh-log
  while true; do
    sleep 60
    if [ -f $LOGFILE ]; then
      if test `find ${LOGFILE} -mmin +5`; then
        # dead for more then 5 Minutes
        if [[ $(pgrep -f "/usr/bin/offlineimap") -gt 0 ]] ; then
          sleep 20 # just wait some more, bevore killing offlineimap
          if test `find ${LOGFILE} -mmin +5`; then 
            # test, if it is realy dead
            pid=$(pgrep -f "/usr/bin/offlineimap")
            if [[ ${pid} -gt 0 ]] ; then
              # kill offlineimap
              pkill -9 -f 'offlineimap\>.*-o'
              echo "#############################################"
              echo "#############################################"
              echo "Offlineimap has hung with pid ${pid} (killed)"
              echo "#############################################"
              echo "#############################################"
            fi
          fi
        fi
      fi
    fi
  done >>$LOGFILE 2>&1
}
_monitor &
trap 'kill $(jobs -p)' EXIT

LASTRUN=0
sleeptime=15 # sleep only 15s on the first run
# main loop #################################################################
while true; do
  sleep $sleeptime
  sleeptime=$((2*60))
  # check, if connected #####################################################
  if ! ping -c1 cloud.github.com > /dev/null 2>&1; then
    echo "**** not connected (ping) ****" >&2
    # check again ###########################################################
    if ! wget -O - cloud.github.com > /dev/null 2>&1; then
      echo "**** not connected (wget) ****" >&2
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
