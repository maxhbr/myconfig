#!/bin/sh
# ~/bin/mailrun.sh
#
# This script runs periodicaly "offlineimap -o"
# - every 10 minutes a full synch is done
# - every 2 minutes a partial synch of the INBOX'es is done, if AC-power is
#   connected
# - runs a full synch after standby
# - the log is saved to /tmp/mailrun-log
# - the routine _monitor() checks, if offlineimap is alive, by checking wether
#   the log file changes
#
# Last modified: So Jul 21, 2013  11:05

PID=$$

PIDFILE=/tmp/mailrun-sh-pid
TIMEFILE=/tmp/mailrun-sh-lastrun
LOGFILE=/tmp/mailrun-sh-log

pid=$(pgrep -f "/usr/bin/offlineimap")
if [[ ${pid} -gt 0 ]] ; then
  echo "Offlineimap is running with pid ${pid}"
  exit 1
fi
if [ -f ${PIDFILE} ] && [ -d "/proc/`cat $PIDFILE`" ]; then
  echo "mailrun.sh is already running"
  exit 2 # or kill the running instance?
fi

rm -f $TIMEFILE
echo $PID > $PIDFILE
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
        pid=$(pgrep -f "/usr/bin/offlineimap")
        if [[ ${pid} -gt 0 ]] ; then
          echo "#############################################"
          echo "#############################################"
          echo "Offlineimap has hung with pid ${pid} (killed)"
          pkill -9 -f 'offlineimap\>.*-o'
          echo "#############################################"
          echo "#############################################"
        fi
      fi
    fi
  done >>$LOGFILE 2>&1
}
_monitor &
#MONITOR_PID=$!
trap 'kill $(jobs -p)' EXIT

sleeptime=15 # sleep only 15s on the first run
# main loop #################################################################
while true; do
  sleep $sleeptime
  sleeptime=120
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
    if [ ! -f $TIMEFILE ] || [ $(($CURR-$(cat $TIMEFILE))) -gt $((600)) ]; then
      echo "$CURR" >$TIMEFILE
      /usr/bin/offlineimap -o -u ttyui
    else
      if [[ $(acpi -a | grep -c on-line) == "1" ]]; then
        /usr/bin/offlineimap -o -f INBOX -u ttyui
      else
        echo "**** no shortsynch, because no AC ****"
      fi
    fi
    echo "**** Offlineimap is ready (at $(date)) ****"
  fi
done >>$LOGFILE 2>&1

rm $PIDFILE
rm $TIMEFILE
