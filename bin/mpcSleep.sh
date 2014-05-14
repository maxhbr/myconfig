#!/bin/sh

HOST="mpd@192.168.178.28"

vol=`mpc -h $HOST | grep "volume" | awk '{print $2}'`
vol=${vol%\%}
base="60"

mpcFade() {
  for (( i = 0; i < $(($1-$2)); i++ )); do
    mpc -h $HOST --no-status volume $(($1 - $i))
    echo $(($1 - $i))
    sleep 1
  done
}

sleep "$((6 * $base))"

mpcFade $vol $(($vol / 2))
sleep "$((4 * $base))"

mpcFade $(($vol / 2)) $(($vol / 4))
sleep "$((3 * $base))"

mpcFade $(($vol / 4)) $(($vol / 6))
sleep "$((2 * $base))"

mpcFade $(($vol / 6)) $(($vol / 8))
sleep "$((1 * $base))"

mpcFade $(($vol / 8)) $(($vol / 10))
sleep "$((1 * $base))"

mpcFade $(($vol / 10)) $(($vol / 15))
sleep "$((1 * $base))"

mpcFade $(($vol / 10)) 0
mpc -h $HOST --no-status pause
sleep 10

mpc -h $HOST --no-status volume $vol
