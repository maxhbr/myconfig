#!/usr/bin/env bash
# stolen from: https://raw.githubusercontent.com/hallettj/dot-xmonad/master/home/bin/volume
set -e

change=$1
cmd=${0##*/}
max=100

if [ -z $change ] ; then
  echo "Usage: $cmd VOLUME"
  echo "Examples:"
  echo "    $cmd +10%"
  echo "    $cmd -10%"
  echo "    $cmd 56%"
  exit 1
fi

function running_sink {
  pactl list sinks short | grep RUNNING | cut -f1 | head -n1
}

function default_sink {
  name=$(pacmd stat | grep 'Default sink name:' | awk -F ': *' '{ print $2 }')
  pactl list sinks short | fgrep "$name" | cut -f1
}

function current_volume {
  local sink="$1"
  # From https://unix.stackexchange.com/a/230533
  sinks="$(pactl list sinks)"
  mute=false
  if echo "$sinks" \
          | grep "Mute:\|Stumm:" \
          | head -n $(( $sink + 1 )) \
          | tail -n 1 \
          | grep -q "Mute: yes\|Stumm: ja"; then
      mute=true
  fi

  [[ "$mute" = true ]] && echo -n "("
  echo -n "$(echo "$sinks" \
                 | grep '^[[:space:]]Volume:\|^[[:space:]]Lautstärke:' \
                 | head -n $(( $sink + 1 )) \
                 | tail -n 1 \
                 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')"
  [[ "$mute" = true ]] && echo -n ")"
}

SINK=$(running_sink)
if [ -z "$SINK" ]; then
  SINK=$(default_sink)
fi

if [[ "$change" == "toggle" ]]; then
    pactl set-sink-mute "$SINK" "$change"
elif [[ "$change" == "mute" ]]; then
    pactl set-sink-mute "$SINK" 1
else
    if [[ "$change" == +* ]]; then
        pactl set-sink-mute "$SINK" 0

        if [[ "$(current_volume "$SINK")" -ge 100 ]]; then
            echo max
            exit 0
        fi
    fi
    pactl set-sink-volume "$SINK" $change
fi
current_volume "$SINK"


pactl list sinks >/tmp/sinks
