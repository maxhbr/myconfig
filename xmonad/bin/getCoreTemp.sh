#!/usr/bin/env bash

prefix=$1
postfix=${2:-C}
medCol=${3:-orange}
hiCol=${4:-red}

declare -a tempPaths=("/sys/devices/platform/coretemp.0/hwmon/hwmon0/temp1_input"
                      "/sys/devices/platform/coretemp.0/hwmon/hwmon0/temp2_input"
                      "/sys/devices/platform/coretemp.0/hwmon/hwmon0/temp3_input"
                      "/sys/devices/platform/coretemp.0/hwmon/hwmon0/temp4_input"
                      "/sys/devices/platform/coretemp.0/hwmon/hwmon0/temp5_input")

temp=0
for p in "${tempPaths[@]}"; do
   nextTemp=$(cat "$p")
   if (( $nextTemp > $temp )); then
       temp=$nextTemp
   fi
done
temp="$(expr $temp / 1000)"
if (( $temp > 80 )); then
    temp="<fc=red>$temp</fc>"
elif (( $temp > 70 )); then
    temp="<fc=orange>$temp</fc>"
fi
echo "$1${temp}$2"
