#!/bin/sh

mpcFade() {
  HOST="mpd@192.168.178.28"

  t=$(($1 * 60))
  vol=`mpc -h $HOST | grep "volume" | awk '{print $2}'`
  vol=${vol%\%}
  for (( i = 0; i < $vol; i++ )); do
    mpc -h $HOST --no-status volume $(($vol - $i))
    echo $(($vol - $i))
    sleep $(($t / $vol))
  done
  mpc -h $HOST --no-status pause
  sleep 10
  mpc -h $HOST --no-status volume $vol
}

mpcFade 1
