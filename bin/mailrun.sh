#!/bin/sh

pid=$(pgrep -f "/usr/bin/offlineimap")
if [[ ${pid} -gt 0 ]] ; then
  echo "Offlineimap is running with pid ${pid}"
  exit 1
fi

echo $$ > /tmp/mailrun.pid

TIMEFILE=/tmp/mailrun-lastrun
rm -f $TIMEFILE
LOGFILE=/tmp/mailrun-log
echo "**** started at $(date)****" >>$LOGFILE 2>&1

## Monitor offlineimap
# checks every minute, if the logfile has changed in the last 5 minutes
_monitor() {
  while true; do
    sleep 60
    if [ -f /tmp/mailrun-log ]; then
      if test `find /tmp/mailrun-log -mmin +5`; then
        # dead for more then 5 Minutes
        pid=$(pgrep -f "/usr/bin/offlineimap")
        if [[ ${pid} -gt 0 ]] ; then
          echo "#############################################" >> $LOGFILE 2>&1
          echo "#############################################" >> $LOGFILE 2>&1
          echo "Offlineimap has hung with pid ${pid}" >> $LOGFILE 2>&1
          pkill -9 -f 'offlineimap\>.*-o'
          echo "#############################################" >> $LOGFILE 2>&1
          echo "#############################################" >> $LOGFILE 2>&1
        else
          echo "#############################################" >> $LOGFILE 2>&1
          echo "has hung, but there is no running offlineimap" >> $LOGFILE 2>&1
          echo "#############################################" >> $LOGFILE 2>&1
        fi
      fi
    fi
  done
}
_monitor &
#MONITOR_PID=$!
trap 'kill $(jobs -p)' EXIT

sleeptime=15 # sleep only 15s an the first run
## main loop
while true; do
  sleep $sleeptime
  sleeptime=60
  # check, if connected #####################################################
  if ! ping -c1 cloud.github.com > /dev/null 2>&1; then
    echo "**** not connected (ping) ****" >&2
    if ! wget -O - cloud.github.com > /dev/null 2>&1; then
      echo "**** not connected (wget) ****" >&2
      continue
    fi
  fi
  # Run offlineimap #########################################################
  echo "**** Offlineimap started (at $(date))****"
  CURR=$(date '+%s')
  if [[ $(pgrep -f "/usr/bin/offlineimap") -gt 0 ]] ; then
    echo "#############################################"
    echo "Offlineimap is already running with pid ${pid}"
    echo "#############################################"
  else
    if [ ! -f $TIMEFILE ] || [ $(($CURR-$(cat $TIMEFILE))) -gt $((600)) ]; then
      echo "$CURR" >$TIMEFILE
      offlineimap -o -u ttyui
    else
      offlineimap -o -f INBOX -u ttyui
    fi
    echo "**** Offlineimap is ready (at $(date))****"
  fi
  ###########################################################################
done >> $LOGFILE 2>&1
