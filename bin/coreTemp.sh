#!/bin/bash
temp2=`cat /sys/devices/platform/coretemp.0/temp3_input`
echo `expr $temp2 / 1000`C
