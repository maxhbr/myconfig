#!/usr/bin/env bash
# see: http://anderspapitto.com/posts/2016-11-07-scripting_pulseaudio_bluetooth_jack.html

set -e
tries=0
limit=10
exec &> >(tee -a /tmp/mybtheadset_sh)
echo "=========================================================================="
echo "= $(date) ="

DEVICE=$(bluetoothctl <<< devices | egrep '^Device' | awk '{ print $2 }')
until bluetoothctl <<< show | grep -q 'Powered: yes'; do
    tries=$((tries+1)); [[ $tries -gt $limit ]] && exit 1

    echo "${tries}: power on"
    bluetoothctl <<< 'power on'
    sleep 1
done
until pacmd list-sinks | egrep -q 'name:.*bluez_sink'; do
    tries=$((tries+1))
    [[ $tries -gt $limit ]] && exit 1

    echo "${tries}: connect $DEVICE"
    bluetoothctl <<< "connect $DEVICE"
    sleep 1
done

TARGET_CARD=$(pacmd list-cards | grep 'name:' | egrep -o 'bluez.*[^>]')
TARGET_SINK=$(pacmd list-sinks | grep 'name:' | egrep -o 'bluez.*[^>]')

until pacmd list-cards | egrep -q 'active profile: <a2dp_sink>'; do
    tries=$((tries+1)); [[ $tries -gt $limit ]] && exit 1

    echo "${tries}: a2dp_sink"
    pacmd set-card-profile $TARGET_CARD a2dp_sink
    sleep 1
done

pactl set-sink-volume $TARGET_SINK 75%
pacmd set-default-sink $TARGET_SINK
for index in $(pacmd list-sink-inputs | grep index | awk '{ print $2 }'); do
    tries=$((tries+1)); [[ $tries -gt $limit ]] && exit 1

    echo "${tries}: set sink"
    pacmd move-sink-input $index $TARGET_SINK
    sleep 1
done

echo "success"
